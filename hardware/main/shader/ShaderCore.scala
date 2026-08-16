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
  })

  val icacheFillUnit = Module(new ICacheFillUnit)

  val fetchSelectStage = Module(new FetchSelectStage)
  val instructionFetchStage = Module(new InstructionFetchStage)
  val instructionDecodeStage = Module(new InstructionDecodeStage)

  fetchSelectStage.io.startJob <> io.startJob
  fetchSelectStage.io.resetThread <> instructionDecodeStage.io.resetThread
  fetchSelectStage.io.fetchRequest <> instructionFetchStage.io.fetchRequest
  fetchSelectStage.io.haltRequest.valid := false.B
  fetchSelectStage.io.haltRequest.bits := DontCare
  fetchSelectStage.io.wakeThreadBitmap := icacheFillUnit.io.wakeThreadBitmap
  fetchSelectStage.io.icacheMiss := instructionFetchStage.io.miss
  fetchSelectStage.io.icacheNearMiss := instructionFetchStage.io.nearMiss
  fetchSelectStage.io.icacheMissThread := instructionFetchStage.io.missThread
  fetchSelectStage.io.rollback.valid := false.B
  fetchSelectStage.io.rollback.bits := DontCare

  instructionFetchStage.io.fillRequest <> icacheFillUnit.io.fillRequest
  instructionFetchStage.io.updateCache <> icacheFillUnit.io.updateCache
  instructionFetchStage.io.output <> instructionDecodeStage.io.input

  instructionDecodeStage.io.writeback.valid := false.B
  instructionDecodeStage.io.writeback.bits := DontCare

  icacheFillUnit.io.readPort <> io.icacheReadPort
}

