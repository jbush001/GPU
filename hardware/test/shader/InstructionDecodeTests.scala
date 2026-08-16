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
    (opcode | (rd << 7) | (imm << 16)) & 0xffffffffL
  }

  def writeVector(dut: InstructionDecodeStage, thread: Int, regId: Int, values: Seq[Int]): Unit = {
    dut.io.writeback.valid.poke(true.B)
    dut.io.writeback.bits.thread.poke(thread.U)
    dut.io.writeback.bits.regId.poke(regId.U)
    for (i <- 0 until cfg.shaderVectorLanes) {
      dut.io.writeback.bits.value(i).poke(values(i).U)
    }
    dut.clock.step(1)
    dut.io.writeback.valid.poke(false.B)
  }

  def writeScalar(dut: InstructionDecodeStage, thread: Int, regId: Int, value: Int): Unit = {
    dut.io.writeback.valid.poke(true.B)
    dut.io.writeback.bits.thread.poke(thread.U)
    dut.io.writeback.bits.regId.poke(regId.U)
    dut.io.writeback.bits.value(0).poke(value.U)
    dut.clock.step(1)
    dut.io.writeback.valid.poke(false.B)
  }

  test("InstructionDecodeStage r_instruction vector") {
      simulate(new InstructionDecodeStage()) { dut =>

      // Write a few registers
      writeVector(dut, 1, 64, (0 until cfg.shaderVectorLanes).map(i => i + 100))
      writeVector(dut, 1, 65, (0 until cfg.shaderVectorLanes).map(i => i + 1000))

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
      // Write default vector value
      writeVector(dut, 1, 64, Seq.fill(cfg.shaderVectorLanes)(0xffff))

      // Write exec mask
      writeScalar(dut, 1, 32, "b01010101".U.litValue.toInt)

      // Write new vector value
      writeVector(dut, 1, 64, (0 until cfg.shaderVectorLanes).map(i => i + 100))

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

  test("InstructionDecodeStage mask reset") {
    simulate(new InstructionDecodeStage()) { dut =>
      // Write a default vector value
      writeVector(dut, 1, 64, Seq.fill(cfg.shaderVectorLanes)(0xffff))

      // Write exec mask
      writeScalar(dut, 1, 32, "b01010101".U.litValue.toInt)

      // Reset thread, which should reset the mask to all 1s
      dut.io.resetThread.valid.poke(true.B)
      dut.io.resetThread.bits.poke(1.U)
      dut.clock.step(1)
      dut.io.resetThread.valid.poke(false.B)

      // Write new vector value
      writeVector(dut, 1, 64, (0 until cfg.shaderVectorLanes).map(i => i + 100))

      dut.io.input.bits.thread.poke(1.U)
      dut.io.input.bits.instruction.poke(rInst(12, 0x0B, 64, 65).U)
      dut.clock.step(1)

      // Ensure it wrote to all lanes
      for (i <- 0 until cfg.shaderVectorLanes) {
        dut.io.output.bits.operand1(i).expect((i + 100).U)
      }
    }
  }

  test("InstructionDecodeStage exec mask register read back") {
    simulate(new InstructionDecodeStage()) { dut =>
      // Write exec mask
      writeScalar(dut, 1, 32, "b01010101".U.litValue.toInt)

      dut.io.input.bits.thread.poke(1.U)
      dut.io.input.bits.instruction.poke(rInst(12, 0x0B, 32, 65).U) // read exec mask
      dut.clock.step(1)

      dut.io.output.bits.operand1(0).expect("b01010101".U)
    }
  }

  test("InstructionDecodeStage scalar") {
    simulate(new InstructionDecodeStage()) { dut =>

      writeScalar(dut, 1, 2, 1234)
      writeScalar(dut, 1, 3, 5678)

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

      writeScalar(dut, 1, 5, 1234)

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

  test("InstructionDecodeStage constants") {
    simulate(new InstructionDecodeStage()) { dut =>
      dut.io.input.valid.poke(true.B)
      dut.io.input.bits.thread.poke(1.U)

      def floatToUInt(fval: Float): UInt = (java.lang.Float.floatToIntBits(fval) & 0xffffffffL).U
      def intToUInt(ival: Int): UInt = (ival & 0xffffffffL).U

      val constants = Seq(
        (53, intToUInt(0)), // constant 0
        (54, intToUInt(1)),
        (55, intToUInt(-1)),
        (56, intToUInt(2)),
        (57, intToUInt(4)),
        (58, floatToUInt(0.5f)),
        (59, floatToUInt(-0.5f)),
        (60, floatToUInt(1.0f)),
        (61, floatToUInt(-1.0f)),
        (62, floatToUInt(2.0f)),
        (63, floatToUInt(-2.0f))
      )

      // Test as first operand
      for ((regId, value) <- constants) {
        dut.io.input.bits.instruction.poke(rInst(12, 0, regId, 0).U)
        dut.clock.step(1)

        for (i <- 0 until cfg.shaderVectorLanes) {
          dut.io.output.bits.operand1(i).expect(value)
        }
      }

      // Test as second operand
      for ((regId, value) <- constants) {
        dut.io.input.bits.instruction.poke(rInst(12, 0, 0, regId).U)
        dut.clock.step(1)

        for (i <- 0 until cfg.shaderVectorLanes) {
          dut.io.output.bits.operand2(i).expect(value)
        }
      }
    }
  }

  test("InstructionDecodeStage read vector lane id") {
    simulate(new InstructionDecodeStage()) { dut =>
      dut.io.input.valid.poke(true.B)
      dut.io.input.bits.thread.poke(1.U)

      dut.io.input.bits.instruction.poke(rInst(12, 0, 111, 0).U) // lane id
      dut.clock.step(1)

      for (i <- 0 until cfg.shaderVectorLanes) {
        dut.io.output.bits.operand1(i).expect(i.U)
      }
    }
  }

  test("InstructionDecodeStage writeback multi-thread") {
    simulate(new InstructionDecodeStage()) { dut =>
      writeScalar(dut, 0, 2, 111)
      writeScalar(dut, 1, 2, 222)

      dut.io.input.valid.poke(true.B)
      dut.io.input.bits.thread.poke(0.U)
      dut.io.input.bits.instruction.poke(rInst(12, 0, 2, 0).U)
      dut.clock.step(1)

      for (i <- 0 until cfg.shaderVectorLanes) {
        dut.io.output.bits.operand1(i).expect(111.U)
      }

      dut.io.input.bits.thread.poke(1.U)
      dut.io.input.bits.instruction.poke(rInst(12, 0, 2, 0).U)
      dut.clock.step(1)

      for (i <- 0 until cfg.shaderVectorLanes) {
        dut.io.output.bits.operand1(i).expect(222.U)
      }
    }
  }

  test("InstructionDecodeStage exec mask multi-thread") {
    simulate(new InstructionDecodeStage()) { dut =>
      writeScalar(dut, 0, 32, "b01010101".U.litValue.toInt)
      writeScalar(dut, 1, 32, "b10101010".U.litValue.toInt)

      dut.io.input.valid.poke(true.B)
      dut.io.input.bits.thread.poke(0.U)
      dut.io.input.bits.instruction.poke(rInst(12, 0, 32, 0).U) // read exec mask
      dut.clock.step(1)

      dut.io.output.bits.operand1(0).expect("b01010101".U)

      dut.io.input.bits.thread.poke(1.U)
      dut.io.input.bits.instruction.poke(rInst(12, 0, 32, 0).U) // read exec mask
      dut.clock.step(1)

      dut.io.output.bits.operand1(0).expect("b10101010".U)
    }
  }

  // Read and write the same register during the same cycle
  test("InstructionDecodeStage write under read scalar") {
    simulate(new InstructionDecodeStage()) { dut =>
      // write default values
      writeScalar(dut, 1, 2, 0)

      // Now write and read the same register
      dut.io.writeback.valid.poke(true.B)
      dut.io.writeback.bits.thread.poke(1.U)
      dut.io.writeback.bits.regId.poke(2.U)
      for (i <- 0 until cfg.shaderVectorLanes) {
        dut.io.writeback.bits.value(i).poke(111.U)
      }

      dut.io.input.valid.poke(true.B)
      dut.io.input.bits.thread.poke(1.U)
      dut.io.input.bits.instruction.poke(rInst(12, 0, 2, 0).U)
      dut.clock.step(1)

      for (i <- 0 until cfg.shaderVectorLanes) {
        dut.io.output.bits.operand1(i).expect(111.U)
      }
    }
  }

  test("InstructionDecodeStage write under read vector") {
    simulate(new InstructionDecodeStage()) { dut =>
      // Write default values
      writeVector(dut, 1, 64, Seq.fill(cfg.shaderVectorLanes)(0))

      // Now write and read a the same register.
      dut.io.writeback.valid.poke(true.B)
      dut.io.writeback.bits.thread.poke(1.U)
      dut.io.writeback.bits.regId.poke(64.U)
      for (i <- 0 until cfg.shaderVectorLanes) {
        dut.io.writeback.bits.value(i).poke((i + 100).U)
      }

      dut.io.input.valid.poke(true.B)
      dut.io.input.bits.thread.poke(1.U)
      dut.io.input.bits.instruction.poke(rInst(12, 0, 64, 0).U)
      dut.clock.step(1)

      for (i <- 0 until cfg.shaderVectorLanes) {
        dut.io.output.bits.operand1(i).expect((i + 100).U)
      }
    }
  }

  test("InstructionDecodeStage write under read scalar special") {
    simulate(new InstructionDecodeStage()) { dut =>
      dut.io.writeback.valid.poke(true.B)
      dut.io.writeback.bits.thread.poke(1.U)
      dut.io.writeback.bits.regId.poke(32.U) // exec mask
      dut.io.writeback.bits.value(0).poke("b01010101".U)
      dut.io.input.valid.poke(true.B)
      dut.io.input.bits.thread.poke(1.U)
      dut.io.input.bits.instruction.poke(rInst(12, 0, 32, 0).U) // read exec mask
      dut.clock.step(1)

      dut.io.output.bits.operand1(0).expect("b01010101".U)
    }
  }

  test("InstructionDecodeStage write under read vector special") {
    simulate(new InstructionDecodeStage()) { dut =>
      val regIndex = 105 // store pixel red
      dut.io.writeback.valid.poke(true.B)
      dut.io.writeback.bits.thread.poke(1.U)
      dut.io.writeback.bits.regId.poke(regIndex.U)
      for (i <- 0 until cfg.shaderVectorLanes) {
        dut.io.writeback.bits.value(i).poke((i + 100).U)
      }

      dut.io.input.valid.poke(true.B)
      dut.io.input.bits.thread.poke(1.U)
      dut.io.input.bits.instruction.poke(rInst(12, 0, regIndex, 0).U) // read lane id
      dut.clock.step(1)

      for (i <- 0 until cfg.shaderVectorLanes) {
        dut.io.output.bits.operand1(i).expect((i + 100).U)
      }
    }
  }
}
