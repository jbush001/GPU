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
  * Parameters are available to shader program by reading special registers.
  * These registers are initialized when a thread is started and can be used to
  * pass data from fixed function units to the shader program.
  */
class ShaderParams(implicit val cfg: GpuConfig) extends Bundle {
  val numParams = 2
  val params = Vec(numParams, Vec(cfg.shaderVectorLanes, UInt(32.W)))
}

/**
  * Top level shader core, which can run multiple shader program threads.
  */
class ShaderCore(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val icacheReadPort = new MemReadPort
    val startJob = Flipped(Decoupled(new Bundle {
      val startPc = UInt(cfg.busAddressBits.W)
      val params = new ShaderParams
    }))

    // A bit of a debug hack for now
    val outputResult = Valid(Vec(cfg.shaderVectorLanes, UInt(32.W)))
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
  fetchSelectStage.io.wakeThreadBitmap := icacheFillUnit.io.wakeThreadBitmap
  fetchSelectStage.io.icacheMiss := instructionFetchStage.io.miss
  fetchSelectStage.io.icacheNearMiss := instructionFetchStage.io.nearMiss
  fetchSelectStage.io.icacheMissThread := instructionFetchStage.io.missThread
  fetchSelectStage.io.haltRequest <> executeStage.io.haltRequest
  fetchSelectStage.io.rollback <> executeStage.io.rollback

  instructionFetchStage.io.fillRequest <> icacheFillUnit.io.fillRequest
  instructionFetchStage.io.updateCache <> icacheFillUnit.io.updateCache
  instructionFetchStage.io.output <> instructionDecodeStage.io.input
  instructionFetchStage.io.squashThread <> executeStage.io.squashThread

  instructionDecodeStage.io.output <> executeStage.io.in
  instructionDecodeStage.io.startParams := fetchSelectStage.io.startParams
  io.outputResult := instructionDecodeStage.io.outputResult

  executeStage.io.writeback <> instructionDecodeStage.io.writeback

  icacheFillUnit.io.readPort <> io.icacheReadPort
}

