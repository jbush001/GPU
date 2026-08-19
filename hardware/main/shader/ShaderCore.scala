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

class ShaderCore(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val icacheReadPort = new MemReadPort
    val startJob = Flipped(Decoupled(new Bundle {
      val startPc = UInt(cfg.busAddressBits.W)
    }))

    // A bit of a debug hack for now
    val outputResult = Valid(Vec(cfg.shaderVectorLanes, UInt(32.W)))
  })

  val icacheFillUnit = Module(new ICacheFillUnit)

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

  fetchSelectStage.io.rollback.valid := false.B
  fetchSelectStage.io.rollback.bits := DontCare

  instructionFetchStage.io.fillRequest <> icacheFillUnit.io.fillRequest
  instructionFetchStage.io.updateCache <> icacheFillUnit.io.updateCache
  instructionFetchStage.io.output <> instructionDecodeStage.io.input
  instructionFetchStage.io.squashThread <> executeStage.io.squashThread

  instructionDecodeStage.io.output <> executeStage.io.in
  io.outputResult := instructionDecodeStage.io.outputResult

  executeStage.io.writeback <> instructionDecodeStage.io.writeback

  icacheFillUnit.io.readPort <> io.icacheReadPort
}

