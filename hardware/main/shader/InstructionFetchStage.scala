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

package gpu.shader

import chisel3._
import chisel3.util._
import gpu._

class FetchRequest(implicit val cfg: GpuConfig) extends Bundle {
  val pc = new ICacheAddress
  val thread = UInt(log2Up(cfg.shaderThreads).W)
}

class FetchResponse(implicit val cfg: GpuConfig) extends Bundle {
  val instruction = UInt(cfg.busDataBits.W)
  val pc = UInt(cfg.busAddressBits.W)
  val thread = UInt(log2Up(cfg.shaderThreads).W)
}

/**
  * Instruction cache. This is a direct mapped cache, with two cycles of latency.
  * The first stage reads tag memory to determine if the requested address is in
  * the cache. The second stage reads the instruction memory and returns the
  * instruction if it is a hit.
  */
class InstructionFetchStage(implicit cfg: GpuConfig) extends Module {
  val instructionWidth = 32

  val io = IO(new Bundle {
    // From thread select
    val fetchRequest = Input(Valid(new FetchRequest))

    // To ICacheFillUnit
    val fillRequest = Valid(new CacheFillRequest)

    // From ICacheFillUnit. Update cache data
    val updateCache = Flipped(Valid(new CacheUpdateRequest))

    // Output
    val output = Valid(new FetchResponse)

    val nearMiss = Output(Bool())
  })

  ///////////////////////////////////////////////////////////
  // Stage 1: read tag memory
  ///////////////////////////////////////////////////////////
  val tagMemory = SyncReadMem(cfg.icacheLines, UInt(cfg.tagBits.W))
  val tagValid = RegInit(VecInit(Seq.fill(cfg.icacheLines)(false.B)))

  val stage1 = new {
    val tag = tagMemory.read(io.fetchRequest.bits.pc.index)
    val valid = RegNext(tagValid(io.fetchRequest.bits.pc.index))
    val fetchRequest = RegNext(io.fetchRequest)
  }

  ///////////////////////////////////////////////////////////
  // Stage 2: check for cache hit
  ///////////////////////////////////////////////////////////
  val instructionMemory = SyncReadMem(cfg.icacheLines * (cfg.cacheLineSizeBytes / 8),
    UInt(cfg.busDataBits.W))

  val stage2 = new {
    // A near miss occurs when a cache line is filled the same cycle that a thread tries
    // to read it. We shouldn't treat as a miss, since the system will hang, but we do
    // need to restart the thread to pick up the instruction.
    val nearMiss = (io.updateCache.bits.last && io.updateCache.bits.address.index === stage1.fetchRequest.bits.pc.index
            && io.updateCache.bits.address.tag === stage1.tag && stage1.fetchRequest.valid)

    val cacheHit = stage1.tag === stage1.fetchRequest.bits.pc.tag && stage1.valid
    val cacheMiss = stage1.fetchRequest.valid && !cacheHit
    io.fillRequest.valid := cacheMiss && !nearMiss
    io.fillRequest.bits.address := stage1.fetchRequest.bits.pc.cacheLineAligned
    io.fillRequest.bits.thread := stage1.fetchRequest.bits.thread

    io.output.bits.pc := RegNext(stage1.fetchRequest.bits.pc.raw)
    val readValue = instructionMemory.read(Cat(stage1.fetchRequest.bits.pc.index,
      stage1.fetchRequest.bits.pc.cacheLineOffset(cfg.cacheLineOffsetBits - 1, 3)))

    io.output.valid := RegNext(stage1.fetchRequest.valid && (cacheHit && !nearMiss))
    io.nearMiss := RegNext(nearMiss)
    io.output.bits.thread := RegNext(stage1.fetchRequest.bits.thread)
  }

  // Cache memory is 64 bits, but instructions are 32, so need to select correct
  // half of returned data
  val instWords = stage2.readValue.asTypeOf(Vec(2, UInt(32.W)))
  io.output.bits.instruction := Mux(io.output.bits.pc(2), instWords(1), instWords(0))

  // Update cache on fill
  when (io.updateCache.valid) {
    instructionMemory.write(Cat(io.updateCache.bits.address.index,
      io.updateCache.bits.address.cacheLineOffset(cfg.cacheLineOffsetBits - 1, (log2Up(cfg.busDataBits / 8)))),
      io.updateCache.bits.data)
  }

  when (io.fillRequest.valid) {
    // Mark cache line invalid while it is being filled, as it will be in an
    // incomplete state.
    tagValid(io.fillRequest.bits.address.index) := false.B
  }

  when (io.updateCache.valid && io.updateCache.bits.last) {
    // Fill has completed, update metadata and mark line as valid.
    tagMemory.write(io.updateCache.bits.address.index, io.updateCache.bits.address.tag)
    tagValid(io.updateCache.bits.address.index) := true.B
  }
}

