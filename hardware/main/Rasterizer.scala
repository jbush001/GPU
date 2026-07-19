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

object Consts {
  val triangleEdges = 3
  val pixelsPerQuad = 4
}

class RasterizerSetupParams extends Bundle {
  val boundingBox = BoundingBox()
  val initialValue = Vec(Consts.triangleEdges, SInt(GpuConfig.edgeFunctionBits.W))
  val xStep = Vec(Consts.triangleEdges, SInt(GpuConfig.edgeFunctionBits.W))
  val yStep = Vec(Consts.triangleEdges, SInt(GpuConfig.edgeFunctionBits.W))
}

/** Contains coverage and interpolation data for a single 2x2 pixel quad. */
class QuadOutput extends Bundle {
  /** Coordinates of the upper left corner, relative to the left/top edges
    * of the current tile bounding box.
    */
  val location = Point2D()

  /** Indicates which pixels are covered, with one bit per pixel using the
    * following layout:
    *
    *     0 1
    *     2 3
    */
  val mask = Bits(Consts.pixelsPerQuad.W)

  /** Unnormalized barycentric coordindates of the pixels relative to
    * the triangle vertices.
    * [[https://en.wikipedia.org/wiki/Barycentric_coordinate_system]]
    */
  val lambda = Vec(Consts.pixelsPerQuad, Vec(Consts.triangleEdges, SInt(32.W)))
}

/** Determines pixel coverage for a triangle.
  *
  * This sweeps over the triangle using an approach based on Pineda "A parallel
  * algorithm for polygon rasterization" (SIGGRAPH 88). It outputs 2x2 aligned
  * quads with one bit per pixel to indicate coverage.
  *
  * @todo Support MSAA, which would return an N-bit mask for each of the quads.
  * @todo Optimize the sweep algorithm; it currently scans the entire tile.
  */
class Rasterizer extends Module {
  val io = IO(new Bundle {
    val input = Flipped(Decoupled(new RasterizerSetupParams))
    val output = Decoupled(new QuadOutput)
  })

  object StepCommand extends ChiselEnum {
    val Reset, Right, Down, Left, Wait = Value
  }

  val stepCommand = Wire(StepCommand())

  val inParams = RegEnable(io.input.bits, io.input.fire)
  val startRasterize = RegNext(io.input.fire)

  val quadLoc = Reg(Point2D())

  // We compute the visibility of four pixels in the quad in parallel.
  val pixelCheck = Cat((for (pixel <- 0 until Consts.pixelsPerQuad) yield {
    val edgeCheck = for (edge <- 0 until Consts.triangleEdges) yield {
      // The edge value represents the dot product of this point with the edge,
      // which tells us on which side of the edge it is on. If the pixel
      // is on the inside of all three triangle edges, then it is inside the triangle.
      val edgeValue = Reg(SInt(32.W))

      io.output.bits.lambda(pixel)(edge) := edgeValue

      switch(stepCommand) {
        is(StepCommand.Reset) {
          pixel match {
            case 0 => edgeValue := inParams.initialValue(edge)
            case 1 => edgeValue := inParams.initialValue(edge) + inParams.xStep(edge)
            case 2 => edgeValue := inParams.initialValue(edge) + inParams.yStep(edge)
            case 3 => edgeValue := (inParams.initialValue(edge) + inParams.xStep(edge)
                + inParams.yStep(edge))
          }
        }
        is(StepCommand.Right) {
          edgeValue := edgeValue + (inParams.xStep(edge) << 1).pad(32)
        }
        is(StepCommand.Down) {
          edgeValue := edgeValue + (inParams.yStep(edge) << 1).pad(32)
        }
        is(StepCommand.Left) {
          edgeValue := edgeValue - (inParams.xStep(edge) << 1).pad(32)
        }
      }

      edgeValue(31)
    }

    edgeCheck.reduceLeft(_ & _)
  }).reverse)

  object State extends ChiselEnum {
    val Idle, StepRight, StepLeft = Value
  }

  val stateReg = RegInit(State.Idle)

  // Stepping state machine. This is fairly simplistic; it sweeps the entire
  // bounding box in a zig-zag pattern.
  io.input.ready := false.B
  io.output.valid := false.B
  stepCommand := StepCommand.Wait;
  switch (stateReg) {
    // Waiting to start a new triangle
    is (State.Idle) {
      io.input.ready := true.B
      when (startRasterize) {
        stepCommand := StepCommand.Reset
        quadLoc := inParams.boundingBox.topLeft
        stateReg := State.StepRight
      } otherwise {
        stepCommand := StepCommand.Wait
      }
    }

    is (State.StepRight) {
      io.output.valid := pixelCheck =/= 0.U;
      when (io.output.ready) {
        when (quadLoc.x === inParams.boundingBox.right) {
          when (quadLoc.y === inParams.boundingBox.bottom) {
            stateReg := State.Idle
          }.otherwise {
            stepCommand := StepCommand.Down;
            quadLoc.y := quadLoc.y + 2.S
            stateReg := State.StepLeft
          }
        }.otherwise {
          stepCommand := StepCommand.Right;
          quadLoc.x := quadLoc.x + 2.S
        }
      }
    }

   is (State.StepLeft) {
      io.output.valid := pixelCheck =/= 0.U
      when (io.output.ready) {
        when(quadLoc.x === inParams.boundingBox.left) {
          when (quadLoc.y === inParams.boundingBox.bottom) {
            stateReg := State.Idle
          }.otherwise {
            stepCommand := StepCommand.Down
            quadLoc.y := quadLoc.y + 2.S
            stateReg := State.StepRight
          }
        }.otherwise {
          stepCommand := StepCommand.Left
          quadLoc.x := quadLoc.x - 2.S
        }
      }
    }
  }

  // Coordinates need to be relative to bounding box.
  io.output.bits.location := quadLoc - inParams.boundingBox.topLeft
  io.output.bits.mask := pixelCheck
}

