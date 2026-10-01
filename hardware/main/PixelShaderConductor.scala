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
  * PixelShaderConductor handles batching requests for [[ShaderCore]],
  * It collects rasterized quads from the rasterizer, dispatches shading
  * jobs to the shader core, and sends shaded quads to the tile buffer, tracking
  * the state of all in-flight quads. The ShaderCore performs register reads
  * and writes to access data for pixels.
  */
class PixelShaderConductor(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    /** From [[DepthInterpolator]]. Incoming quad to be shaded. */
    val sourceQuad = Flipped(Decoupled(new InterpolatedQuad))

    /** Asserted for one cycle when all triangles have been processed. */
    val lastTriangle = Output(Bool())

    /** Asserted for one cycle when a single triangle has been fully processed. */
    val triangleFinished = Valid(UInt(cfg.triangleIdBits.W))

    /** To [[ShaderCore]], requests starting a new thread to process pixels
      * The jobId will be used for subsequent register accesses to uniquely identify
      * this job.
      */
    val startJob = Decoupled(new Bundle {
      val startPc = UInt(cfg.busAddressBits.W)
      val jobId = UInt(cfg.shaderJobIdBits.W)
    })

    /** From [[ShaderCore]], Signals that the job has completed and its outputs
      * have been written via shaderRegWrite.
      */
    val jobFinished = Flipped(Valid(UInt(cfg.shaderJobIdBits.W)))

    /** From [[ShaderCore]],  Requests a read of register `addr` for job `jobId`.
      * The response arrives one cycle later on shaderRegReadData.
      */
    val shaderRegRead = Flipped(Valid(new Bundle {
      val jobId = UInt(cfg.shaderJobIdBits.W)
      val addr = UInt(3.W)
    }))

    /** This is the response to shaderRegRead.
      * If valid is low in the cycle after a request, the data is not yet
      * available and the requesting thread must stall until ioWakeJob is
      * asserted for its jobId
      */
    val shaderRegReadData = Valid(Vec(cfg.shaderVectorLanes, UInt(32.W)))

    /** From [[ShaderCore]], Asserted when a read that returned invalid on
      * shaderRegReadData can now proceed. Only used if shaderRegReadData.valid
      * was low in the cycle after the request.
      */
    val ioWakeJob = Valid(UInt(cfg.shaderJobIdBits.W))

    /** From [[ShaderCore]], Writes `data` (one 32-bit value per lane) to
      * register `addr` of job `jobId`.
      */
    val shaderRegWrite = Flipped(Valid(new Bundle {
      val jobId = UInt(cfg.shaderJobIdBits.W)
      val addr = UInt(3.W)
      val data = Vec(cfg.shaderVectorLanes, UInt(32.W))
    }))

    /** To [[TileBuffer]], fully shaded quad to write back */
    val shadedQuad = Valid(new Bundle {
      val location = Point2D()
      val mask = Bits(Consts.pixelsPerQuad.W)
      val colors = Vec(Consts.pixelsPerQuad, Vec(Color.numChannels, Float32()))
      val depths = Vec(Consts.pixelsPerQuad, Float32())
    })

    /** From triangle setup. Writes one varying coefficient (used to interpolate
      * per-vertex attributes across the triangle) at `index` for `triangleId`.
      */
    val writeVaryingCoeff = Flipped(Valid(new Bundle {
      val triangleId = UInt(cfg.triangleIdBits.W)
      val index = UInt(5.W)
      val value = Float32()
    }))

    /** To [[TextureCache]]. Requests a texture sample. */
    val textureFetchRequest = Decoupled(new TextureFetchRequest())

    /** From [[TextureCache]]. Returns the sampled texture data. Must be
      * matched with a previous textureFetchRequest.
      */
    val textureFetchResponse = Flipped(Valid(new TextureFetchResponse()))
  })

  val quadsPerJob = cfg.shaderVectorLanes / Consts.pixelsPerQuad
  val totalPendingJobs = 1 << cfg.shaderJobIdBits
  val maxVaryingCoeffs = 18

  object JobState extends ChiselEnum {
    val Idle, Filling, ReadyToProcess, Processing, ReadyToDrain = Value
  }

  class JobInfo extends Bundle {
    val state = JobState()
    val validQuadCount = UInt(log2Up(quadsPerJob + 1).W)
    val sourceQuads = Vec(quadsPerJob, new InterpolatedQuad)
    val shadedColors = Vec(Color.numChannels, Vec(cfg.shaderVectorLanes, Float32()))
    val varyingCoeffIndex = UInt(log2Up(maxVaryingCoeffs).W)
    val textureFetchRequestPending = Bool()
    val texelCoord = Vec(2, Vec(cfg.shaderVectorLanes, Float32()))
    val fetchedTexels = Vec(Color.numChannels, Vec(cfg.shaderVectorLanes, Float32()))
    val threadNeedsWake = Bool()
    val returnedTexelBitmap = Bits(quadsPerJob.W)
  }

  // The jobs array is indexed by jobId.
  val jobs = RegInit(VecInit.fill(totalPendingJobs)(0.U.asTypeOf(new JobInfo)))

  // We support multiple triangles in-flight at once. This array stores coeffients for all active
  // ones.
  val varyingCoeffs = RegInit(VecInit.fill(cfg.maxConcurrentTriangles, maxVaryingCoeffs)(0.U.asTypeOf(Float32())))

  // Collects quads for a full SIMD width in the shader.
  val fillIndex = RegInit(0.U(cfg.shaderJobIdBits.W))
  val fillQuadCount = RegInit(0.U(log2Up(quadsPerJob).W))
  val fillJob = jobs(fillIndex)

  // Indicate ready if the current job is being filled or the tail is free.
  io.sourceQuad.ready := (fillJob.state === JobState.Filling
    || fillJob.state === JobState.Idle)


  when (io.sourceQuad.fire) {
    assert(fillQuadCount === 0.U || fillJob.state === JobState.Filling)
    fillJob.sourceQuads(fillQuadCount) := io.sourceQuad.bits
    fillJob.validQuadCount := fillQuadCount +& 1.U
    when (fillQuadCount === (quadsPerJob - 1).U || io.sourceQuad.bits.lastTriangle) {
      // Finished filling, ready for processing
      fillQuadCount := 0.U
      fillJob.state := JobState.ReadyToProcess
      fillIndex := fillIndex + 1.U
    }.otherwise {
      fillQuadCount := fillQuadCount + 1.U
      fillJob.state := JobState.Filling
    }
  }

  // Send fully populated jobs to the shader engine
  val dispatchIndex = RegInit(0.U(cfg.shaderJobIdBits.W))

  io.startJob.valid := jobs(dispatchIndex).state === JobState.ReadyToProcess
  io.startJob.bits.startPc := 0.U // XXX need to allow setting shader address
  io.startJob.bits.jobId := dispatchIndex

  when (io.startJob.fire) {
    jobs(dispatchIndex).state := JobState.Processing
    jobs(dispatchIndex).varyingCoeffIndex := 0.U
    dispatchIndex := dispatchIndex + 1.U
  }

  when (io.jobFinished.fire) {
    val index = io.jobFinished.bits
    assert(jobs(index).state === JobState.Processing,
      "Job finished signal received for a job that is not processing")
    jobs(index).state := JobState.ReadyToDrain
  }

  // Drain shaded quads to the tile buffer
  val drainIndex = RegInit(0.U(cfg.shaderJobIdBits.W))
  val drainJob = jobs(drainIndex)
  val drainQuadCount = RegInit(0.U(log2Up(quadsPerJob).W))
  val drainQuad = drainJob.sourceQuads(drainQuadCount)

  io.shadedQuad.valid := drainJob.state === JobState.ReadyToDrain

  io.shadedQuad.bits.location := drainQuad.location
  io.shadedQuad.bits.mask := drainQuad.mask
  io.shadedQuad.bits.depths := drainQuad.depths
  for (pixelI <- 0 until Consts.pixelsPerQuad) {
    for (channelI <- 0 until Color.numChannels) {
      val pixelIndex = drainQuadCount * Consts.pixelsPerQuad.U + pixelI.U
      io.shadedQuad.bits.colors(pixelI)(channelI) := (
        drainJob.shadedColors(channelI)(pixelIndex(log2Up(cfg.shaderVectorLanes) - 1, 0))
      )
    }
  }

  when (io.shadedQuad.fire) {
    when (drainQuadCount === drainJob.validQuadCount - 1.U) {
      // Finished draining this job
      drainQuadCount := 0.U
      drainJob.state := JobState.Idle
      drainIndex := drainIndex + 1.U
    }.otherwise {
      drainQuadCount := drainQuadCount + 1.U
    }
  }

  io.triangleFinished.valid := drainQuad.lastQuad && io.shadedQuad.fire
  io.triangleFinished.bits := drainQuad.triangleId
  io.lastTriangle := drainQuad.lastTriangle && io.shadedQuad.fire

  val textureResponseId = io.textureFetchResponse.bits.requestId
  val textureResponseJobId = textureResponseId(cfg.shaderJobIdBits - 1, 0)
  val textureResponseQuad = textureResponseId(cfg.textureRequestIdBits - 1, cfg.shaderJobIdBits)

  // Register access from shader core. This has one cycle of latency, but we
  // register the request and perform the lookup in the second cycle so we can
  // cleanly bypass texture results that happen the same cycle.
  val regReadValidStage2 = RegNext(io.shaderRegRead.valid, init = false.B)
  val regReadJobIdStage2 = RegNext(io.shaderRegRead.bits.jobId)
  val regReadAddrStage2 = RegNext(io.shaderRegRead.bits.addr)

  val readJob = jobs(regReadJobIdStage2)

  // This logic handles the second stage/cycle of the shader register read.
  io.shaderRegReadData.valid := false.B // default
  io.shaderRegReadData.bits := DontCare
  when (regReadValidStage2) {
    assert(readJob.state === JobState.Processing)
    io.shaderRegReadData.valid := true.B
    switch (regReadAddrStage2) {
      // Read barycentric coordinates
      is (0.U, 1.U) {
        for (i <- 0 until cfg.shaderVectorLanes) {
          val quadIndex = (i / Consts.pixelsPerQuad)
          val pixelIndex = (i % Consts.pixelsPerQuad)
          io.shaderRegReadData.bits(i) := readJob.sourceQuads(quadIndex).lambda(pixelIndex)(
            regReadAddrStage2(0)).asUInt
        }
      }

      // Read varying coefficient memory
      is (2.U) {
        for (quadI <- 0 until quadsPerJob) {
          val coeffVal = varyingCoeffs(readJob.sourceQuads(quadI).triangleId)(readJob.varyingCoeffIndex)
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
          io.shaderRegReadData.bits := jobs(textureResponseJobId).fetchedTexels(colorChannel).map(_.raw)

          for (lane <- lastTexelOffset until cfg.shaderVectorLanes) {
            io.shaderRegReadData.bits(lane) := io.textureFetchResponse.bits.texels(colorChannel)(lane - lastTexelOffset).raw
          }
        }.elsewhen (!readJob.returnedTexelBitmap.andR) {
          io.shaderRegReadData.valid := false.B  // Need to wait for result
          readJob.threadNeedsWake := true.B
        }.otherwise {
          val texelVal = readJob.fetchedTexels(colorChannel(1, 0))
          io.shaderRegReadData.bits := texelVal.map(_.raw)
        }
      }
    }
  }.otherwise {
    io.shaderRegReadData.valid := false.B
  }

  when (io.shaderRegWrite.valid) {
    val writeJob = jobs(io.shaderRegWrite.bits.jobId)
    assert(writeJob.state === JobState.Processing)
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
  for ((in, job) <- nextTextureRequestJob.io.in.zip(jobs)) {
    in.valid := job.textureFetchRequestPending && !job.returnedTexelBitmap.orR
    in.bits := DontCare
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
        assert(jobs(textureRequestIndex).state === JobState.Processing)
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
    assert(job.state === JobState.Processing)
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
    varyingCoeffs(io.writeVaryingCoeff.bits.triangleId)(io.writeVaryingCoeff.bits.index) :=
      io.writeVaryingCoeff.bits.value
  }
}
