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
  * PixelShaderConductor coordinates between the [[Rasterizer]], [[ShaderCore]],
  * and [[TileBuffer]].
  * It collects rasterized quads from the rasterizer, dispatches shading
  * jobs to the shader core, and sends shaded quads to the tile buffer, tracking
  * the state of all in-flight quads.
  */
class PixelShaderConductor(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    // From Rasterizer.
    val rasterizedQuad = Flipped(Decoupled(new RasterizedQuad))
    val flush = Input(Bool())

    // To/From ShaderCore
    val startJob = Decoupled(new Bundle {
      val startPc = UInt(cfg.busAddressBits.W)
      val tag = UInt(cfg.shaderTagBits.W)
    })

    val jobFinished = Flipped(Valid(UInt(cfg.shaderTagBits.W)))

    val shaderRegRead = Flipped(Valid(new Bundle {
      val tag = UInt(cfg.shaderTagBits.W)
      val addr = UInt(3.W)
    }))

    val shaderRegReadData = Output(Vec(cfg.shaderVectorLanes, UInt(32.W)))

    val shaderRegWrite = Flipped(Valid(new Bundle {
      val tag = UInt(cfg.shaderTagBits.W)
      val addr = UInt(3.W)
      val data = Vec(cfg.shaderVectorLanes, UInt(32.W))
    }))

    // To TileBuffer
    val shadedQuad = Valid(new Bundle {
      val location = Point2D()
      val mask = Bits(Consts.pixelsPerQuad.W)
      val colors = Vec(Consts.pixelsPerQuad, Vec(Color.numChannels, Float32()))
      val depths = Vec(Consts.pixelsPerQuad, UInt(cfg.depthBufferBits.W))
    })

    // True when there are no jobs pending
    val idle = Output(Bool())

    // Program varying coefficients, from triangle setup
    val writeVaryingCoeff = Flipped(Valid(new Bundle {
      val index = UInt(5.W)
      val value = new Float32()
    }))
  })

  val quadsPerJob = cfg.shaderVectorLanes / Consts.pixelsPerQuad
  val totalPendingJobs = 10
  val maxVaryingCoeffs = 18

  object JobState extends ChiselEnum {
    val Idle, Filling, ReadyToProcess, Processing, ReadyToDrain, Draining = Value
  }

  class JobInfo extends Bundle {
    val state = JobState()
    val rasterizedQuads = Vec(quadsPerJob, new RasterizedQuad)
    val colors = Vec(Color.numChannels, Vec(cfg.shaderVectorLanes, new Float32()))
    val varyingCoeffIndex = UInt(log2Up(maxVaryingCoeffs).W)
  }

  val jobs = RegInit(VecInit(Seq.fill(totalPendingJobs)(0.U.asTypeOf(new JobInfo))))
  val varyingCoeffs = RegInit(VecInit(Seq.fill(maxVaryingCoeffs)(0.U.asTypeOf(new Float32()))))

  io.idle := (0 until totalPendingJobs).map(i => jobs(i).state === JobState.Idle).reduce(_&&_)

  // Fill jobs
  val nextFillJob = Module(new RRArbiter(Bool(), totalPendingJobs))
  for (i <- 0 until totalPendingJobs) {
    nextFillJob.io.in(i).valid := jobs(i).state === JobState.Idle
    nextFillJob.io.in(i).bits := false.B
  }

  val fillActive = RegInit(false.B)
  val fillIndex = RegInit(0.U(log2Up(totalPendingJobs).W))
  val fillQuadCount = RegInit(0.U(log2Up(quadsPerJob).W))

  // Indicate ready if there are any available jobs to fill.
  io.rasterizedQuad.ready := ((0 until totalPendingJobs).map(
    i => jobs(i).state === JobState.Idle).reduce(_||_) || fillActive
  )

  assert(!(io.flush && io.rasterizedQuad.valid),
    "Cannot have a valid rasterized quad while flushing")

  nextFillJob.io.out.ready := false.B
  when (io.flush && fillActive) {
    assert(fillQuadCount != 0.U)
    // Push empty quads to complete any pending entries.
    jobs(fillIndex).rasterizedQuads(fillQuadCount).mask := 0.U
    when (fillQuadCount === (quadsPerJob - 1).U) {
      // Finished filling, ready for processing
      fillQuadCount := 0.U
      fillActive := false.B
      jobs(fillIndex).state := JobState.ReadyToProcess
    }.otherwise {
      fillQuadCount := fillQuadCount + 1.U
    }
  }.elsewhen (io.rasterizedQuad.fire) {
    when (fillActive) {
      assert(fillQuadCount != 0.U)

      // Fill existing partially readyToProcess job entry
      jobs(fillIndex).rasterizedQuads(fillQuadCount) := io.rasterizedQuad.bits

      when (fillQuadCount === (quadsPerJob - 1).U) {
        // Finished filling, ready for processing
        fillQuadCount := 0.U
        fillActive := false.B
        jobs(fillIndex).state := JobState.ReadyToProcess
      }.otherwise {
        fillQuadCount := fillQuadCount + 1.U
      }
    }.otherwise {
      // Pick a new job entry to start filling
      fillActive := true.B
      fillQuadCount := 1.U
      fillIndex := nextFillJob.io.chosen
      nextFillJob.io.out.ready := true.B
      jobs(nextFillJob.io.chosen).state := JobState.Filling
      jobs(nextFillJob.io.chosen).rasterizedQuads(0) := io.rasterizedQuad.bits
    }
  }

  // Send completed jobs to the shader engine
  val nextShaderJob = Module(new RRArbiter(Bool(), totalPendingJobs))
  for (i <- 0 until totalPendingJobs) {
    nextShaderJob.io.in(i).valid := jobs(i).state === JobState.ReadyToProcess
    nextShaderJob.io.in(i).bits := false.B
  }

  nextShaderJob.io.out.ready := io.startJob.ready

  io.startJob.valid := nextShaderJob.io.out.valid
  io.startJob.bits.startPc := 0.U // XXX need to allow setting shader address
  io.startJob.bits.tag := nextShaderJob.io.chosen

  when (io.startJob.fire) {
    jobs(nextShaderJob.io.chosen).state := JobState.Processing
    jobs(nextShaderJob.io.chosen).varyingCoeffIndex := 0.U
  }

  when (io.jobFinished.fire) {
    val index = io.jobFinished.bits(log2Up(totalPendingJobs) - 1, 0)
    assert(jobs(index).state === JobState.Processing,
      "Job finished signal received for a job that is not processing")
    jobs(index).state := JobState.ReadyToDrain
  }

  // Drain
  val nextDrainJob = Module(new RRArbiter(Bool(), totalPendingJobs))
  for (i <- 0 until totalPendingJobs) {
    nextDrainJob.io.in(i).valid := jobs(i).state === JobState.ReadyToDrain
    nextDrainJob.io.in(i).bits := false.B
  }

  val drainActive = RegInit(false.B)
  val drainIndex = RegInit(0.U(log2Up(totalPendingJobs).W))
  val drainQuadCount = RegInit(0.U(log2Up(quadsPerJob).W))

  io.shadedQuad.valid := (0 until totalPendingJobs).map(i => jobs(i).state === JobState.ReadyToDrain).reduce(_ || _) || drainActive

  val drainSelect = WireInit(0.U(log2Up(totalPendingJobs).W))
  nextDrainJob.io.out.ready := false.B
  io.shadedQuad.bits.location := jobs(drainSelect).rasterizedQuads(drainQuadCount).location
  io.shadedQuad.bits.mask := jobs(drainSelect).rasterizedQuads(drainQuadCount).mask
  io.shadedQuad.bits.depths := VecInit(Seq.fill(Consts.pixelsPerQuad)(0.U(cfg.depthBufferBits.W))) // XXX not implemented
  for (pixelI <- 0 until Consts.pixelsPerQuad) {
    for (channelI <- 0 until Color.numChannels) {
      val pixelIndex = drainQuadCount * Consts.pixelsPerQuad.U + pixelI.U
      io.shadedQuad.bits.colors(pixelI)(channelI) := (
        jobs(drainSelect).colors(channelI)(pixelIndex(log2Up(cfg.shaderVectorLanes) - 1, 0))
      )
    }
  }

  when (io.shadedQuad.fire) {
    when (drainActive) {
      drainSelect := drainIndex
      when (drainQuadCount === (quadsPerJob - 1).U) {
        // Finished draining, ready for next job
        drainQuadCount := 0.U
        drainActive := false.B
        jobs(drainIndex).state := JobState.Idle
      }.otherwise {
        drainQuadCount := drainQuadCount + 1.U
      }
    }.otherwise {
      // Pick a new job to drain
      nextDrainJob.io.out.ready := true.B
      drainActive := true.B
      drainQuadCount := 1.U
      drainIndex := nextDrainJob.io.chosen
      drainSelect := nextDrainJob.io.chosen
    }
  }

  // Register access from shader core.
  val readJob = jobs(io.shaderRegRead.bits.tag(log2Up(totalPendingJobs) - 1, 0))
  val readResult = RegInit(VecInit(Seq.fill(cfg.shaderVectorLanes)(0.U(32.W))))
  for (i <- 0 until cfg.shaderVectorLanes) {
    val quadIndex = (i / Consts.pixelsPerQuad)
    val pixelIndex = (i % Consts.pixelsPerQuad)
    readResult(i) := readJob.rasterizedQuads(quadIndex).lambda(pixelIndex)(io.shaderRegRead.bits.addr(0)).asUInt
  }

  when (io.shaderRegRead.bits.addr === 2.U && io.shaderRegRead.valid) {
    val coeffVal = varyingCoeffs(readJob.varyingCoeffIndex)
    for (i <- 0 until cfg.shaderVectorLanes) {
      readResult(i) := coeffVal.raw
    }

    readJob.varyingCoeffIndex := readJob.varyingCoeffIndex + 1.U
  }

  io.shaderRegReadData := readResult

  when (io.shaderRegWrite.valid) {
    val writeJob = jobs(io.shaderRegWrite.bits.tag(log2Up(totalPendingJobs) - 1, 0))
    switch (io.shaderRegWrite.bits.addr) {
      is (0.U, 1.U, 2.U, 3.U) {
        writeJob.colors(io.shaderRegWrite.bits.addr(1, 0)) := io.shaderRegWrite.bits.data.asTypeOf(Vec(cfg.shaderVectorLanes, new Float32()))
      }
    }
  }

  // Write coefficient memory during setup
  when (io.writeVaryingCoeff.valid) {
    varyingCoeffs(io.writeVaryingCoeff.bits.index) := io.writeVaryingCoeff.bits.value
  }
}
