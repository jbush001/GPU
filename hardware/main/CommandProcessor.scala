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

object ControlRegister extends ChiselEnum {
  val ClearColor,
    ClearDepth,
    TileWriteFlags,
    StartFlush,
    ShaderAddress,
    NumVaryings,
    TileOffsetX,
    TileOffsetY = Value
}

/** Configuration and control for all units from the command processor.
  */
class ControlBus extends Bundle {
  val writeValue = Valid(new Bundle {
    val address = ControlRegister()
    val data = UInt(32.W)
  })
}

/** The Command Processor (CP) interfaces between the host CPU and the GPU
  * execution units. It parses sequential command streams from shared memory,
  * configures hardware state, and orchestrates downstream work pipelines.
  *
  * Command packets are variable length, starting with a single byte opcode.
  * These are fully documented in [[docs/command-format.md]]
  *
  * @todo this is a stub.
  */
class CommandProcessor(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val start = Flipped(Valid(UInt(cfg.busAddressBits.W)))
    val readPort = new MemReadPort
    val controlBus = new ControlBus
  })

  io.readPort.burst.valid := false.B
  io.readPort.burst.bits := DontCare
  io.readPort.data.ready := true.B
  io.controlBus.writeValue.valid := false.B
  io.controlBus.writeValue.bits := DontCare
}
