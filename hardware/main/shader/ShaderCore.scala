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

/** Multithreaded SIMD vector processor that executes shader programs.
  *
  * Each job is one hardware thread running `GpuConfig.shaderVectorLanes`
  * parallel pixels. The core switches between threads each cycle to hide
  * latency from register reads and texture fetches. This core assigns
  * incoming job requests to idle threads, which run until a HALT
  * instruction executes.
  *
  * The core doesn't directly access fixed-function state. Shader inputs
  * and outputs, including texture results, are read and written through
  * the shaderRegRead/shaderRegWrite ports, via[[PixelShaderConductor]].
  *
  * Instructions are fetched from system memory through icacheReadPort.
  *
  * @see docs/shader-microarchitecture.md for the pipeline and <doc> for the
  *      instruction set.
  */
  class ShaderCore(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val icacheReadPort = new MemReadPort
    val startJob = Flipped(Decoupled(new Bundle {
      val startPc = UInt(cfg.busAddressBits.W)

    /** Identifies this job to the [[PixelShaderConductor]]. Chosen by the
      * conductor and unique among jobs in flight. The core attaches it to
      * shaderRegRead and shaderRegWrite requests.
      */
      val jobId = UInt(cfg.shaderJobIdBits.W)
    }))

    val jobFinished = Valid(UInt(cfg.shaderJobIdBits.W))

    val regRead = Valid(new Bundle {
      val jobId = UInt(cfg.shaderJobIdBits.W)
      val addr = UInt(3.W)
    })

    /** This returns data one cycle after a regRead request. */
    val regReadData = Flipped(Valid(Vec(cfg.shaderVectorLanes, UInt(32.W))))

    /** If regReadData caused a wait, this will signal when it should wake up */
    val ioWakeJob = Flipped(Valid(UInt(cfg.shaderJobIdBits.W)))

    val regWrite = Valid(new Bundle {
      val jobId = UInt(cfg.shaderJobIdBits.W)
      val addr = UInt(3.W)
      val data = Vec(cfg.shaderVectorLanes, UInt(32.W))
    })

    val invalidateICache = Input(Bool())
  })

  val icacheFillUnit = Module(new ICacheFillUnit)

  // Core execution pipeline
  val fetchSelectStage = Module(new FetchSelectStage)
  val instructionFetchStage = Module(new InstructionFetchStage)
  val instructionDecodeStage = Module(new InstructionDecodeStage)
  val executeStage = Module(new ExecuteStage)

  fetchSelectStage.io.startJob <> io.startJob
  fetchSelectStage.io.resetThread <> instructionDecodeStage.io.resetThread
  fetchSelectStage.io.fetchRequest <> instructionFetchStage.io.fetchRequest
  fetchSelectStage.io.icacheWakeThreads := icacheFillUnit.io.icacheWakeThreads
  fetchSelectStage.io.icacheMiss := instructionFetchStage.io.icacheMiss
  fetchSelectStage.io.icacheNearMiss := instructionFetchStage.io.icacheNearMiss
  fetchSelectStage.io.icacheMissThread := instructionFetchStage.io.icacheMissThread
  fetchSelectStage.io.halt <> executeStage.io.halt
  fetchSelectStage.io.rollback <> executeStage.io.rollback
  fetchSelectStage.io.ioWakeJob <> io.ioWakeJob
  fetchSelectStage.io.ioWaitThread <> instructionDecodeStage.io.ioWaitThread

  instructionFetchStage.io.fillRequest <> icacheFillUnit.io.fillRequest
  instructionFetchStage.io.updateCache <> icacheFillUnit.io.updateCache
  instructionFetchStage.io.fetchedInstruction <> instructionDecodeStage.io.fetchedInstruction
  instructionFetchStage.io.squash <> executeStage.io.squash
  instructionFetchStage.io.invalidateICache := io.invalidateICache

  instructionDecodeStage.io.decodedInstruction <> executeStage.io.decodedInstruction
  io.regRead <> instructionDecodeStage.io.regRead
  instructionDecodeStage.io.regReadData <> io.regReadData
  io.regWrite <> instructionDecodeStage.io.regWrite

  executeStage.io.writeback <> instructionDecodeStage.io.writeback
  io.jobFinished <> executeStage.io.jobFinished

  icacheFillUnit.io.readPort <> io.icacheReadPort
}

