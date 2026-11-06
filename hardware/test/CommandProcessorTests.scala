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
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funsuite.AnyFunSuite

class CommandProcessorTests extends AnyFunSuite with ChiselSim with TestHelpers {
  implicit val cfg: GpuConfig = GpuConfig()

  def rInstruction(opcode: UInt, rd: Int, rs1: Int, rs2: Int, imm: Int = 0): Int = {
    ((opcode.litValue.toInt & 0x1F) | ((rd & 0x7) << 5) | ((rs1 & 0x7) << 8)
      | ((rs2 & 0x7) << 11) | ((imm & 0x3FFFF) << 14))
  }

  def loadProgram(dut: CommandProcessor, program: Seq[Int]): Unit = {
    dut.io.halt.poke(true)
    dut.io.loadProgram.valid.poke(true)
    for (instr <- program) {
      dut.io.loadProgram.bits.poke(instr)
      dut.clock.step()
    }
    dut.io.loadProgram.valid.poke(false)
    dut.io.halt.poke(false)
  }

  def runProgram(dut: CommandProcessor): Int = {
    dut.io.halt.poke(false)

    while (!dut.io.controlBus.writeValue.valid.peek().litToBoolean) {
      dut.clock.step()
    }

    dut.io.controlBus.writeValue.bits.data.peek().litValue.toInt
  }

  test("CommandProcessor basic operation") {
    simulate(new CommandProcessor) { dut =>
      loadProgram(dut, Seq(
        rInstruction(CpOpcode.MOVE, 1, 0, 0, 0),
        rInstruction(CpOpcode.MOVEHI, 1, 1, 0, 0x80000000 >> 14),
        rInstruction(CpOpcode.MOVE, 2, 0, 0, 1234),
        rInstruction(CpOpcode.ADDI, 2, 2, 0, 1),
        rInstruction(CpOpcode.SW, 0, 1, 2),
        0,
        0,
        0,
      ))

      val result = runProgram(dut)
      assert(result == 1235)
    }
  }
}
