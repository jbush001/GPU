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
import gpu._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funsuite.AnyFunSuite

class InstructionDecodeTests extends AnyFunSuite with ChiselSim {
  implicit val cfg: GpuConfig = GpuConfig()

  def rInst(opcode: Int, rd: Int, rs1: Int, rs2: Int): Long = {
    opcode | (rd << 7) | (rs1 << 14) | (rs2 << 21)
  }

  def kInst(opcode: Int, rd: Int, imm: Int): Long = {
    return (opcode | (rd << 7) | (imm << 16)) & 0xffffffffL
  }

  test("InstructionDecodeStage r_instruction vector") {
      simulate(new InstructionDecodeStage()) { dut =>

      // Write a few registers
      dut.io.writeback.valid.poke(true.B)
      dut.io.writeback.bits.thread.poke(1.U)
      dut.io.writeback.bits.regId.poke(64.U)
      for (i <- 0 until cfg.shaderVectorLanes) {
        dut.io.writeback.bits.value(i).poke((i + 100).U)
      }
      dut.io.writeback.bits.mask.poke(0xff.U)
      dut.clock.step(1)

      dut.io.writeback.bits.thread.poke(1.U)
      dut.io.writeback.bits.regId.poke(65.U)
      for (i <- 0 until cfg.shaderVectorLanes) {
        dut.io.writeback.bits.value(i).poke((i + 1000).U)
      }
      dut.io.writeback.bits.mask.poke(0xff.U)
      dut.clock.step(1)
      dut.io.writeback.valid.poke(false.B)

      dut.io.input.valid.poke(true.B)
      val address = 0x1000
      dut.io.input.bits.pc.poke(address.U)
      dut.io.input.bits.thread.poke(1.U)
      dut.io.input.bits.instruction.poke(rInst(12, 0x0B, 64, 65).U)
      dut.clock.step(1)

      dut.io.output.valid.expect(true.B)
      dut.io.output.bits.pc.expect(address.U)
      dut.io.output.bits.thread.expect(1.U)
      dut.io.output.bits.opcode.expect(12.U)
      dut.io.output.bits.destReg.expect(0x0B.U)

      for (i <- 0 until cfg.shaderVectorLanes) {
        dut.io.output.bits.operand1(i).expect((i + 100).U)
        dut.io.output.bits.operand2(i).expect((i + 1000).U)
      }
    }
  }

  test("InstructionDecodeStage masked vector write") {
      simulate(new InstructionDecodeStage()) { dut =>
        // write register with mask
        dut.io.writeback.valid.poke(true.B)
        dut.io.writeback.bits.thread.poke(1.U)
        dut.io.writeback.bits.regId.poke(64.U)
        for (i <- 0 until cfg.shaderVectorLanes) {
          dut.io.writeback.bits.value(i).poke(0xffff.U)
        }

        dut.io.writeback.bits.mask.poke(0xff.U)
        dut.clock.step(1)

        for (i <- 0 until cfg.shaderVectorLanes) {
          dut.io.writeback.bits.value(i).poke((i + 100).U)
        }

        dut.io.writeback.bits.mask.poke(0x55.U)
        dut.clock.step(1)
        dut.io.writeback.valid.poke(false.B)

        dut.io.input.bits.thread.poke(1.U)
        dut.io.input.bits.instruction.poke(rInst(12, 0x0B, 64, 65).U)
        dut.clock.step(1)

      for (i <- 0 until cfg.shaderVectorLanes) {
        if (i % 2 == 0) {
          dut.io.output.bits.operand1(i).expect((i + 100).U)
        } else {
          dut.io.output.bits.operand1(i).expect(0xffff.U)
        }
      }
    }
  }

  test("InstructionDecodeStage scalar") {
    simulate(new InstructionDecodeStage()) { dut =>

      // Write scalar registers
      dut.io.writeback.valid.poke(true.B)
      dut.io.writeback.bits.thread.poke(1.U)
      dut.io.writeback.bits.regId.poke(2.U)
      dut.io.writeback.bits.value(0).poke(1234.U)
      dut.clock.step(1)
      dut.io.writeback.bits.regId.poke(3.U)
      dut.io.writeback.bits.value(0).poke(5678.U)
      dut.clock.step(1)
      dut.io.writeback.valid.poke(false.B)

      dut.io.input.valid.poke(true.B)
      val address = 0x1000
      dut.io.input.bits.pc.poke(address.U)
      dut.io.input.bits.thread.poke(1.U)
      dut.io.input.bits.instruction.poke(rInst(12, 0x0B, 2, 3).U)
      dut.clock.step(1)

      dut.io.output.valid.expect(true.B)
      for (i <- 0 until cfg.shaderVectorLanes) {
        dut.io.output.bits.operand1(i).expect(1234.U)
        dut.io.output.bits.operand2(i).expect(5678.U)
      }
    }
  }

  // The destination is also operand1 for constant instructions
  test("InstructionDecodeStage loadConstant") {
    simulate(new InstructionDecodeStage()) { dut =>

      dut.io.writeback.valid.poke(true.B)
      dut.io.writeback.bits.thread.poke(1.U)
      dut.io.writeback.bits.regId.poke(5.U)
      dut.io.writeback.bits.value(0).poke(1234.U)
      dut.clock.step(1)
      dut.io.writeback.valid.poke(false.B)

      dut.io.input.valid.poke(true.B)
      dut.io.input.bits.thread.poke(1.U)
      dut.io.input.bits.instruction.poke(kInst(28, 5, 0).U) // loadhi
      dut.clock.step(1)

      dut.io.output.valid.expect(true.B)

      for (i <- 0 until cfg.shaderVectorLanes) {
        dut.io.output.bits.operand1(i).expect(1234.U)
      }
    }
  }
}
