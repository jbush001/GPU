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
  val threadScheduleStage = Module(new ThreadScheduleStage)
  val instructionFetchStage = Module(new InstructionFetchStage)

  icacheFillUnit.io.readPort <> io.icacheReadPort
  icacheFillUnit.io.fillRequest <> instructionFetchStage.io.fillRequest
  icacheFillUnit.io.updateCache <> instructionFetchStage.io.updateCache
  threadScheduleStage.io.wakeThreadBitmap := icacheFillUnit.io.wakeThreadBitmap

  threadScheduleStage.io.startJob <> io.startJob

  instructionFetchStage.io.fetchRequest <> threadScheduleStage.io.fetchRequest

  // Not implemented yet
  threadScheduleStage.io.haltRequest.valid := false.B
  threadScheduleStage.io.haltRequest.bits := DontCare
  threadScheduleStage.io.stallThreadBitmap := 0.U
  threadScheduleStage.io.rollback.valid := false.B
  threadScheduleStage.io.rollback.bits := DontCare
}

