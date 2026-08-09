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

/** Instruction cache
 */
class InstructionFetch(implicit cfg: GpuConfig) extends Module {
  val instructionWidth = 32

  val io = IO(new Bundle {
    // From thread select
    val fetchEnable = Input(Bool())
    val fetchPc = Input(new ICacheAddress)
    val fetchThread = Input(UInt(log2Up(cfg.shaderThreads).W))

    // To ICacheFill
    val cacheMiss = Output(Bool())
    val missAddress = Output(new ICacheAddress)
    val missThread = Output(UInt(log2Up(cfg.shaderThreads).W))

    // From ICacheFill. Update cache data
    val updateCacheEn = Input(Bool())
    val updateCacheAddress = Input(new ICacheAddress)
    val updateCacheData = Input(UInt((cfg.busDataBits).W))
    val updateCacheDone = Input(Bool())

    // Output
    val outValid = Output(Bool())
    val outInstruction = Output(UInt(instructionWidth.W))
    val outPc = Output(UInt(cfg.busAddressBits.W))
    val outNearMiss = Output(Bool())
    val outThread = Output(UInt(log2Up(cfg.shaderThreads).W))
  })

  ///////////////////////////////////////////////////////////
  // Stage 1: read tag memory
  ///////////////////////////////////////////////////////////
  val tagMemory = SyncReadMem(cfg.icacheLines, UInt(cfg.tagBits.W))
  val tagValid = RegInit(VecInit(Seq.fill(cfg.icacheLines)(false.B)))

  val stage1 = new {
    val tag = tagMemory.read(io.fetchPc.index)
    val valid = RegNext(tagValid(io.fetchPc.index))
    val pc = RegNext(io.fetchPc)
    val thread = RegNext(io.fetchThread)
    val fetchEnable = RegNext(io.fetchEnable, init = false.B)
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
    val nearMiss = (io.updateCacheDone && io.updateCacheAddress.index === stage1.pc.index
            && io.updateCacheAddress.tag === stage1.tag && stage1.fetchEnable)

    val cacheHit = stage1.tag === stage1.pc.tag && stage1.valid
    val cacheMiss = stage1.fetchEnable && !cacheHit
    io.cacheMiss := cacheMiss && !nearMiss
    io.missAddress := stage1.pc.cacheLineAligned
    io.missThread := stage1.thread

    io.outPc := RegNext(stage1.pc.raw)
    val readValue = instructionMemory.read(Cat(stage1.pc.index,
      stage1.pc.cacheLineOffset(cfg.cacheLineOffsetBits - 1, 3)))

    io.outValid := RegNext(stage1.fetchEnable && (cacheHit && !nearMiss))
    io.outNearMiss := RegNext(nearMiss)
    io.outThread := RegNext(stage1.thread)
  }

  // Cache memory is 64 bits, but instructions are 32, so need to select correct
  // half of returned data
  io.outInstruction := Mux(io.outPc(2), stage2.readValue(63, 32), stage2.readValue(31, 0))

  // Update cache on fill
  when (io.updateCacheEn) {
    instructionMemory.write(Cat(io.updateCacheAddress.index,
      io.updateCacheAddress.cacheLineOffset(cfg.cacheLineOffsetBits - 1, (log2Up(cfg.busDataBits / 8)))),
      io.updateCacheData)
  }

  when (io.cacheMiss) {
    // Mark cache line invalid while it is being filled, as it will be in an
    // incomplete state.
    tagValid(io.missAddress.index) := false.B
  }

  when (io.updateCacheDone) {
    // Fill has completed, update metadata and mark line as valid.
    tagMemory.write(io.updateCacheAddress.index, io.updateCacheAddress.tag)
    tagValid(io.updateCacheAddress.index) := true.B
  }
}

