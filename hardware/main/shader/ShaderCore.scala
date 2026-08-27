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

/**
  * Top level shader core, which can run multiple shader program threads.
  * The regRead/regWrite ports form the primary interface to fixed function
  * hardware.
  */
class ShaderCore(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val icacheReadPort = new MemReadPort
    val startJob = Flipped(Decoupled(new Bundle {
      val startPc = UInt(cfg.busAddressBits.W)

      /** The tag uniquely identifies this job. It is included with each
        * external special register read.
        */
      val tag = UInt(cfg.shaderTagBits.W)
    }))

    val jobFinished = Valid(UInt(cfg.shaderTagBits.W))

    val regRead = Valid(new Bundle {
      val tag = UInt(cfg.shaderTagBits.W)
      val addr = UInt(3.W)
    })

    /** This returns data one cycle after a regRead request. */
    val regReadData = Input(Vec(cfg.shaderVectorLanes, UInt(32.W)))

    val regWrite = Valid(new Bundle {
      val tag = UInt(cfg.shaderTagBits.W)
      val addr = UInt(3.W)
      val data = Vec(cfg.shaderVectorLanes, UInt(32.W))
    })
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
  fetchSelectStage.io.wakeThreads := icacheFillUnit.io.wakeThreads
  fetchSelectStage.io.icacheMiss := instructionFetchStage.io.icacheMiss
  fetchSelectStage.io.icacheNearMiss := instructionFetchStage.io.icacheNearMiss
  fetchSelectStage.io.icacheMissThread := instructionFetchStage.io.icacheMissThread
  fetchSelectStage.io.halt <> executeStage.io.halt
  fetchSelectStage.io.rollback <> executeStage.io.rollback

  instructionFetchStage.io.fillRequest <> icacheFillUnit.io.fillRequest
  instructionFetchStage.io.updateCache <> icacheFillUnit.io.updateCache
  instructionFetchStage.io.fetchedInstruction <> instructionDecodeStage.io.fetchedInstruction
  instructionFetchStage.io.squash <> executeStage.io.squash

  instructionDecodeStage.io.decodedInstruction <> executeStage.io.decodedInstruction
  io.regRead <> instructionDecodeStage.io.regRead
  instructionDecodeStage.io.regReadData := io.regReadData
  io.regWrite <> instructionDecodeStage.io.regWrite

  executeStage.io.writeback <> instructionDecodeStage.io.writeback
  io.jobFinished <> executeStage.io.jobFinished

  icacheFillUnit.io.readPort <> io.icacheReadPort
}

