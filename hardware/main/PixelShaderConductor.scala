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

class TextureFetchRequest(implicit cfg: GpuConfig) extends Bundle {
  val requestId = UInt(cfg.textureRequestIdBits.W)
  val coord = Vec(2, Vec(Consts.pixelsPerQuad, Float32()))
}

class TextureFetchResponse(implicit cfg: GpuConfig) extends Bundle {
  val requestId = UInt(cfg.textureRequestIdBits.W)
  val texels = Vec(Color.numChannels, Vec(Consts.pixelsPerQuad, Float32()))
}

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
      val jobId = UInt(cfg.shaderJobIdBits.W)
    })

    val jobFinished = Flipped(Valid(UInt(cfg.shaderJobIdBits.W)))

    val shaderRegRead = Flipped(Valid(new Bundle {
      val jobId = UInt(cfg.shaderJobIdBits.W)
      val addr = UInt(3.W)
    }))

    // This is the response to shaderRegRead with one cycle of latency.
    // If this is not valid, then the reader should block. This will
    // subsequently assert ioWakeJob.
    val shaderRegReadData = Valid(Vec(cfg.shaderVectorLanes, UInt(32.W)))

    // This is asserted when a previously blocked shaderRegRead can now proceed.
    val ioWakeJob = Valid(UInt(cfg.shaderJobIdBits.W))

    val shaderRegWrite = Flipped(Valid(new Bundle {
      val jobId = UInt(cfg.shaderJobIdBits.W)
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
      val primitiveId = UInt(cfg.primitiveIdBits.W)
      val index = UInt(5.W)
      val value = Float32()
    }))

    // To TextureCache
    val textureFetchRequest = Decoupled(new TextureFetchRequest())

    // From TextureCache
    val textureFetchResponse = Flipped(Valid(new TextureFetchResponse()))
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
    val shadedColors = Vec(Color.numChannels, Vec(cfg.shaderVectorLanes, Float32()))
    val varyingCoeffIndex = UInt(log2Up(maxVaryingCoeffs).W)
    val textureFetchRequestPending = Bool()
    val texelCoord = Vec(2, Vec(cfg.shaderVectorLanes, Float32()))
    val fetchedTexels = Vec(Color.numChannels, Vec(cfg.shaderVectorLanes, Float32()))
    val threadNeedsWake = Bool()
    val returnedTexelBitmap = Bits(quadsPerJob.W)
  }

  val jobs = RegInit(VecInit(Seq.fill(totalPendingJobs)(0.U.asTypeOf(new JobInfo))))
  val varyingCoeffs = RegInit(VecInit(Seq.fill(1 << cfg.primitiveIdBits)(VecInit(Seq.fill(maxVaryingCoeffs)(0.U.asTypeOf(Float32()))))))

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

  // Send fully populated jobs to the shader engine
  val nextShaderJob = Module(new RRArbiter(Bool(), totalPendingJobs))
  for (i <- 0 until totalPendingJobs) {
    nextShaderJob.io.in(i).valid := jobs(i).state === JobState.ReadyToProcess
    nextShaderJob.io.in(i).bits := false.B
  }

  nextShaderJob.io.out.ready := io.startJob.ready

  io.startJob.valid := nextShaderJob.io.out.valid
  io.startJob.bits.startPc := 0.U // XXX need to allow setting shader address
  io.startJob.bits.jobId := nextShaderJob.io.chosen

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

  // Drain shaded quads to the tile buffer
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
        jobs(drainSelect).shadedColors(channelI)(pixelIndex(log2Up(cfg.shaderVectorLanes) - 1, 0))
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

  val textureResponseId = io.textureFetchResponse.bits.requestId
  val textureResponseJobId = textureResponseId(cfg.shaderJobIdBits - 1, 0)
  val textureResponseQuad = textureResponseId(cfg.textureRequestIdBits - 1, cfg.shaderJobIdBits)

  // Register access from shader core. This has one cycle of latency, but we
  // register the request and perform the lookup in the second cycle so we can
  // cleanly bypass texture results that happen the same cycle.
  val regReadValidStage2 = RegNext(io.shaderRegRead.valid, init = false.B)
  val regReadJobIdStage2 = RegNext(io.shaderRegRead.bits.jobId(log2Up(totalPendingJobs) - 1, 0))
  val regReadAddrStage2 = RegNext(io.shaderRegRead.bits.addr)

  val readJob = jobs(regReadJobIdStage2)

  // This logic handles the second stage/cycle of the shader register read.
  io.shaderRegReadData.valid := false.B // default
  io.shaderRegReadData.bits := DontCare
  when (regReadValidStage2) {
    io.shaderRegReadData.valid := true.B
    switch (regReadAddrStage2) {
      // Read barycentric coordinates
      is (0.U, 1.U) {
        for (i <- 0 until cfg.shaderVectorLanes) {
          val quadIndex = (i / Consts.pixelsPerQuad)
          val pixelIndex = (i % Consts.pixelsPerQuad)
          io.shaderRegReadData.bits(i) := readJob.rasterizedQuads(quadIndex).lambda(pixelIndex)(
            regReadAddrStage2(0)).asUInt
        }
      }

      // Read varying coefficient memory
      is (2.U) {
        for (quadI <- 0 until quadsPerJob) {
          val coeffVal = varyingCoeffs(readJob.rasterizedQuads(quadI).primitiveId)(readJob.varyingCoeffIndex)
          for (pixelI <- 0 until Consts.pixelsPerQuad) {
            io.shaderRegReadData.bits(quadI * Consts.pixelsPerQuad + pixelI) := coeffVal.raw
          }
        }

        readJob.varyingCoeffIndex := readJob.varyingCoeffIndex + 1.U
      }

      // Read fetched texel
      is (3.U, 4.U, 5.U, 6.U) {
        val colorChannel = regReadAddrStage2 - 3.U
        // Bypass result if the last value arrives from texture cache the same cycle
        when (io.textureFetchResponse.valid
          && textureResponseJobId === regReadJobIdStage2
          && (jobs(textureResponseJobId).returnedTexelBitmap | (1 << (quadsPerJob - 1)).U).andR) {
          val lastTexelOffset = cfg.shaderVectorLanes - Consts.pixelsPerQuad
          for (lane <- 0 until lastTexelOffset) {
            io.shaderRegReadData.bits(lane) := jobs(textureResponseJobId).fetchedTexels(colorChannel)(lane).raw
          }

          for (lane <- lastTexelOffset until cfg.shaderVectorLanes) {
            io.shaderRegReadData.bits(lane) := io.textureFetchResponse.bits.texels(colorChannel)(lane - lastTexelOffset).raw
          }
        }.elsewhen (!readJob.returnedTexelBitmap.andR) {
          io.shaderRegReadData.valid := false.B  // Need to wait for result
          readJob.threadNeedsWake := true.B
        }.otherwise {
          val texelVal = readJob.fetchedTexels(colorChannel(1, 0))
          for (lane <- 0 until cfg.shaderVectorLanes) {
            io.shaderRegReadData.bits(lane) := texelVal(lane).raw
          }
        }
      }
    }
  }.otherwise {
    io.shaderRegReadData.valid := false.B
  }

  when (io.shaderRegWrite.valid) {
    val writeJob = jobs(io.shaderRegWrite.bits.jobId(log2Up(totalPendingJobs) - 1, 0))
    switch (io.shaderRegWrite.bits.addr) {
      is (0.U, 1.U, 2.U, 3.U) {
        writeJob.shadedColors(io.shaderRegWrite.bits.addr(1, 0)) := io.shaderRegWrite.bits.data.asTypeOf(Vec(cfg.shaderVectorLanes, Float32()))
      }

      is (4.U, 5.U) {
        // S, T coordinates
        writeJob.texelCoord(io.shaderRegWrite.bits.addr(0)) := io.shaderRegWrite.bits.data.asTypeOf(Vec(cfg.shaderVectorLanes, Float32()))
      }
    }

    // Writing to the T register has the side effect of enqueuing the texture
    // fetch request.
    when (io.shaderRegWrite.bits.addr === 5.U) {
      assert(!writeJob.textureFetchRequestPending,
        "Multiple texture requests from the same job")
      writeJob.textureFetchRequestPending := true.B
      writeJob.returnedTexelBitmap := 0.U
    }
  }

  // Pick a texture fetch request to issue to the texture pipeline. We issue
  // one quad at a time, but fully issue all quads for a job before moving to the next one.
  val nextTextureRequestJob = Module(new RRArbiter(Bool(), totalPendingJobs))
  for (i <- 0 until totalPendingJobs) {
    nextTextureRequestJob.io.in(i).valid := jobs(i).textureFetchRequestPending && !jobs(i).returnedTexelBitmap.orR
    nextTextureRequestJob.io.in(i).bits := false.B
  }

  val textureRequestActive = RegInit(false.B)
  val textureRequestIndex = RegInit(0.U(log2Up(totalPendingJobs).W))
  val textureRequestQuad = RegInit(0.U((cfg.textureRequestIdBits - cfg.shaderJobIdBits).W))

  io.textureFetchRequest.valid := nextTextureRequestJob.io.out.valid || textureRequestActive

  val textureFetchSelect = WireInit(0.U(log2Up(totalPendingJobs).W))
  io.textureFetchRequest.bits.requestId := Cat(textureRequestQuad, textureFetchSelect)
  val coordOffset = textureRequestQuad << 2
  for (coordI <- 0 until 2) {
    for (pixel <- 0 until Consts.pixelsPerQuad) {
      io.textureFetchRequest.bits.coord(coordI)(pixel) := jobs(textureFetchSelect).texelCoord(coordI)(coordOffset + pixel.U(4.W))
    }
  }

  nextTextureRequestJob.io.out.ready := io.textureFetchRequest.ready

  when (io.textureFetchRequest.fire) {
    when (textureRequestActive) {
      textureFetchSelect := textureRequestIndex
      when (textureRequestQuad === (quadsPerJob - 1).U) {
        textureRequestActive := false.B
        jobs(textureRequestIndex).textureFetchRequestPending := false.B
        textureRequestQuad := 0.U
      }.otherwise {
        textureRequestQuad := textureRequestQuad + 1.U
      }
    }.otherwise {
      textureRequestActive := true.B
      textureRequestIndex := nextTextureRequestJob.io.chosen
      textureFetchSelect := nextTextureRequestJob.io.chosen
      textureRequestQuad := 1.U
    }
  }

  // Handle texture fetch response
  io.ioWakeJob.valid := false.B
  when (io.textureFetchResponse.valid) {
    val coordOffset = textureResponseQuad << 2
    val job = jobs(textureResponseJobId)
    for (i <- 0 until Color.numChannels) {
      for (j <- 0 until Consts.pixelsPerQuad) {
        job.fetchedTexels(i)(coordOffset + j.U) := io.textureFetchResponse.bits.texels(i)(j)
      }
    }

    job.returnedTexelBitmap := job.returnedTexelBitmap | (1.U << textureResponseQuad)
    when ((job.returnedTexelBitmap | (1.U << textureResponseQuad)).andR && job.threadNeedsWake) {
      io.ioWakeJob.valid := true.B
      job.threadNeedsWake := false.B
    }
  }

  io.ioWakeJob.bits := textureResponseJobId

  // Write coefficient memory during setup
  when (io.writeVaryingCoeff.valid) {
    varyingCoeffs(io.writeVaryingCoeff.bits.primitiveId)(io.writeVaryingCoeff.bits.index) :=
      io.writeVaryingCoeff.bits.value
  }
}
