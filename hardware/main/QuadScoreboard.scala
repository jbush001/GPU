//
//   Copyright 2026 Jeff Bush
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.
//

package gpu

import chisel3._
import chisel3.util._

/**
 * QuadScoreboard keeps track of the state of triangles and their associated quads.
 * It manages allocation of triangle IDs, tracks outstanding quads, and detects when
 * all triangles in a batch have been fully rendered.
 */
class QuadScoreboard(implicit val cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    // To setup engine (not yet implemented)
    val allocateTriangleId = Decoupled(UInt(cfg.triangleIdBits.W))

    // From Rasterizer
    val rasterizationFinished = Flipped(Valid(UInt(cfg.triangleIdBits.W)))
    val issueQuad = Flipped(Valid(UInt(cfg.triangleIdBits.W)))

    // A quad has entered PixelShaderConductor.
    val submitQuad = Input(Bool())

    // From PixelShaderConductor
    val retireQuad = Flipped(Valid(UInt(cfg.triangleIdBits.W)))

    val batchFinished = Input(Bool())

    // Indicates all triangles in a batch have been rendered.
    val idle = Output(Bool())

    // To PixelShaderConductor
    val flushPixelShader = Output(Bool())
  })

  class TriangleInfo extends Bundle {
    val rasterizing = Bool()
    val outstandingQuads = UInt(8.W)
  }

  val triangles = RegInit(VecInit.fill(cfg.maxConcurrentTriangles)(0.U.asTypeOf(new TriangleInfo)))
  val quadsAwaitingSubmission = RegInit(0.U(8.W))

  val freeTriangles = VecInit(triangles.map(triangle => !triangle.rasterizing && triangle.outstandingQuads === 0.U))
  val nextFreeIndex = PriorityEncoder(freeTriangles)

  io.idle := freeTriangles.reduce(_ && _)
  io.flushPixelShader := !triangles.map(_.rasterizing).reduce(_ || _) &&
    quadsAwaitingSubmission === 0.U && io.batchFinished

  io.allocateTriangleId.bits := nextFreeIndex
  io.allocateTriangleId.valid := freeTriangles.reduce(_ || _)

  when (io.allocateTriangleId.fire) {
    assert(!triangles(nextFreeIndex).rasterizing,
      "Allocating a triangle ID that is already rasterizing")
    assert(triangles(nextFreeIndex).outstandingQuads === 0.U,
      "Allocating a triangle ID that still has outstanding quads")
    triangles(nextFreeIndex).rasterizing := true.B
  }

  when (io.rasterizationFinished.fire) {
    val finishedIndex = io.rasterizationFinished.bits
    assert(triangles(finishedIndex).rasterizing,
      "Finishing rasterization for a triangle that is not currently rasterizing")
    triangles(finishedIndex).rasterizing := false.B
  }

  when (io.submitQuad && !io.issueQuad.fire) {
    assert(quadsAwaitingSubmission =/= 0.U,
      "Submitting a quad that was not issued by the rasterizer")
    quadsAwaitingSubmission := quadsAwaitingSubmission - 1.U
  }.elsewhen (io.issueQuad.fire && !io.submitQuad) {
    quadsAwaitingSubmission := quadsAwaitingSubmission + 1.U
  }

  for ((triangle, i) <- triangles.zipWithIndex) {
    val issue = io.issueQuad.fire && io.issueQuad.bits === i.U
    val retire = io.retireQuad.fire && io.retireQuad.bits === i.U

    assert(!issue || triangle.rasterizing,
      "Attempting to issue a quad for a triangle that is not currently rasterizing")
    assert(!retire || triangle.outstandingQuads =/= 0.U,
      "Attempting to retire a quad for a triangle that has no outstanding quads")
    when (issue && !retire) {
      triangle.outstandingQuads := triangle.outstandingQuads + 1.U
    }

    when (retire && !issue) {
      triangle.outstandingQuads := triangle.outstandingQuads - 1.U
    }
  }
}
