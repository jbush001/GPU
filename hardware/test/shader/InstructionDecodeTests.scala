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

  val EXEC_MASK = 127
  val VECTOR_GPR_BASE = 128
  val CONST_ZERO = 116

  def rInst(opcode: OpCode.Type, rd: Int, rs1: Int, rs2: Int): Long = {
    opcode.litValue.toLong | (rd << 7) | (rs1 << 15) | (rs2 << 23)
  }

  def kInst(opcode: OpCode.Type, rd: Int, imm: Int): Long = {
    (opcode.litValue.toLong | (rd << 7) | (imm << 16)) & 0xffffffffL
  }

  def writeVector(dut: InstructionDecodeStage, thread: Int, regId: Int, values: Seq[Int]): Unit = {
    require(regId >= VECTOR_GPR_BASE, "Vector register ID must be 128 or higher")
    dut.io.writeback.valid.poke(true.B)
    dut.io.writeback.bits.thread.poke(thread.U)
    dut.io.writeback.bits.destReg.poke(regId.U)
    for (i <- 0 until cfg.shaderVectorLanes) {
      dut.io.writeback.bits.value(i).poke(values(i).U)
    }
    dut.clock.step(1)
    dut.io.writeback.valid.poke(false.B)
  }

  def writeScalar(dut: InstructionDecodeStage, thread: Int, regId: Int, value: Int): Unit = {
    require(regId < 128, "Scalar register ID must be less than 128")
    dut.io.writeback.valid.poke(true.B)
    dut.io.writeback.bits.thread.poke(thread.U)
    dut.io.writeback.bits.destReg.poke(regId.U)
    dut.io.writeback.bits.value(0).poke(value.U)
    dut.clock.step(1)
    dut.io.writeback.valid.poke(false.B)
  }

  test("InstructionDecodeStage r_instruction vector") {
      simulate(new InstructionDecodeStage()) { dut =>

      // Write a few GPRs
      writeVector(dut, 1, VECTOR_GPR_BASE, (0 until cfg.shaderVectorLanes).map(i => i + 100))
      writeVector(dut, 1, VECTOR_GPR_BASE + 1, (0 until cfg.shaderVectorLanes).map(i => i + 1000))

      dut.io.fetchedInstruction.valid.poke(true.B)
      val address = 0x1000
      dut.io.fetchedInstruction.bits.pc.poke(address.U)
      dut.io.fetchedInstruction.bits.thread.poke(1.U)
      dut.io.fetchedInstruction.bits.jobId.poke(11.U)
      dut.io.fetchedInstruction.bits.instruction.poke(rInst(OpCode.Subf,
        VECTOR_GPR_BASE + 2, VECTOR_GPR_BASE, VECTOR_GPR_BASE + 1).U)
      dut.clock.step(1)

      // Set these to zero to ensure unit uses values from first cycle.
      dut.io.fetchedInstruction.valid.poke(false.B)
      dut.io.fetchedInstruction.bits.instruction.poke(0.U)
      dut.io.fetchedInstruction.bits.pc.poke(0.U)
      dut.io.fetchedInstruction.bits.thread.poke(0.U)

      dut.io.decodedInstruction.valid.expect(true.B)
      dut.io.decodedInstruction.bits.meta.pc.expect(address.U)
      dut.io.decodedInstruction.bits.meta.thread.expect(1.U)
      dut.io.decodedInstruction.bits.meta.jobId.expect(11.U)
      dut.io.decodedInstruction.bits.meta.opcode.expect(OpCode.Subf)
      dut.io.decodedInstruction.bits.meta.destReg.expect((VECTOR_GPR_BASE + 2).U)

      for (i <- 0 until cfg.shaderVectorLanes) {
        dut.io.decodedInstruction.bits.operand1(i).expect((i + 100).U)
        dut.io.decodedInstruction.bits.operand2(i).expect((i + 1000).U)
      }
    }
  }

  test("InstructionDecodeStage masked vector write") {
    simulate(new InstructionDecodeStage()) { dut =>
      // Write default vector value
      writeVector(dut, 1, VECTOR_GPR_BASE, Seq.fill(cfg.shaderVectorLanes)(0xffff))

      // Write exec mask
      writeScalar(dut, 1, EXEC_MASK, "b0101010101010101".U.litValue.toInt)

      // Write new vector value
      writeVector(dut, 1, VECTOR_GPR_BASE, (0 until cfg.shaderVectorLanes).map(i => i + 100))

      // Read back to ensure value was written as expected
      dut.io.fetchedInstruction.valid.poke(true.B)
      dut.io.fetchedInstruction.bits.thread.poke(1.U)
      dut.io.fetchedInstruction.bits.instruction.poke(rInst(OpCode.Subf, VECTOR_GPR_BASE + 2,
        VECTOR_GPR_BASE, VECTOR_GPR_BASE + 1).U)
      dut.clock.step(1)

      for (i <- 0 until cfg.shaderVectorLanes) {
        if (i % 2 == 0) {
          dut.io.decodedInstruction.bits.operand1(i).expect((i + 100).U)
        } else {
          dut.io.decodedInstruction.bits.operand1(i).expect(0xffff.U)
        }
     }
    }
  }

  test("InstructionDecodeStage thread reset") {
    simulate(new InstructionDecodeStage()) { dut =>
      // Write a default vector value
      writeVector(dut, 1, VECTOR_GPR_BASE, Seq.fill(cfg.shaderVectorLanes)(0xffff))

      // Write exec mask
      writeScalar(dut, 1, EXEC_MASK, "b0101010101010101".U.litValue.toInt)

      // Reset thread, which should reset the mask to all 1s
      dut.io.resetThread.valid.poke(true.B)
      dut.io.resetThread.bits.poke(1.U)
      dut.clock.step(1)
      dut.io.resetThread.valid.poke(false.B)

      // Write new vector value
      writeVector(dut, 1, VECTOR_GPR_BASE, (0 until cfg.shaderVectorLanes).map(i => i + 100))

      // Read back to ensure value was written as expected
      dut.io.fetchedInstruction.bits.thread.poke(1.U)
      dut.io.fetchedInstruction.bits.instruction.poke(rInst(OpCode.Subf, VECTOR_GPR_BASE + 2,
        VECTOR_GPR_BASE, VECTOR_GPR_BASE + 1).U)
      dut.clock.step(1) // First clock latches instruction, second returns result

      // Ensure it wrote to all lanes
      for (i <- 0 until cfg.shaderVectorLanes) {
        dut.io.decodedInstruction.bits.operand1(i).expect((i + 100).U)
      }
    }
  }

  test("InstructionDecodeStage exec mask register read back") {
    simulate(new InstructionDecodeStage()) { dut =>
      // Write exec mask
      writeScalar(dut, 1, EXEC_MASK, "b0101010101010101".U.litValue.toInt)

      dut.io.fetchedInstruction.bits.thread.poke(1.U)
      dut.io.fetchedInstruction.bits.instruction.poke(rInst(OpCode.Subf, 0,
        EXEC_MASK, CONST_ZERO).U) // read exec mask
      dut.clock.step(1)

      // Set these to zero to ensure unit uses values from first cycle.
      dut.io.fetchedInstruction.valid.poke(false.B)
      dut.io.fetchedInstruction.bits.instruction.poke(0.U)
      dut.io.fetchedInstruction.bits.pc.poke(0.U)
      dut.io.fetchedInstruction.bits.thread.poke(0.U)

      dut.io.decodedInstruction.bits.operand1(0).expect("b0101010101010101".U)
    }
  }

  test("InstructionDecodeStage scalar") {
    simulate(new InstructionDecodeStage()) { dut =>

      writeScalar(dut, 1, 2, 1234)
      writeScalar(dut, 1, 3, 5678)

      dut.io.fetchedInstruction.valid.poke(true.B)
      val address = 0x1000
      dut.io.fetchedInstruction.bits.pc.poke(address.U)
      dut.io.fetchedInstruction.bits.thread.poke(1.U)
      dut.io.fetchedInstruction.bits.instruction.poke(rInst(OpCode.Subf, 0x0B, 2, 3).U)
      dut.clock.step(1)

      // Set these to zero to ensure unit uses values from first cycle.
      dut.io.fetchedInstruction.valid.poke(false.B)
      dut.io.fetchedInstruction.bits.instruction.poke(0.U)
      dut.io.fetchedInstruction.bits.pc.poke(0.U)
      dut.io.fetchedInstruction.bits.thread.poke(0.U)

      dut.io.decodedInstruction.valid.expect(true.B)
      for (i <- 0 until cfg.shaderVectorLanes) {
        dut.io.decodedInstruction.bits.operand1(i).expect(1234.U)
        dut.io.decodedInstruction.bits.operand2(i).expect(5678.U)
      }
    }
  }

  // The destination is also operand1 for constant instructions
  test("InstructionDecodeStage loadConstant") {
    simulate(new InstructionDecodeStage()) { dut =>

      writeScalar(dut, 1, 5, 1234)

      dut.io.fetchedInstruction.valid.poke(true.B)
      dut.io.fetchedInstruction.bits.thread.poke(1.U)
      dut.io.fetchedInstruction.bits.instruction.poke(kInst(OpCode.LoadHi, 5, 0).U) // loadhi
      dut.clock.step(1)

      // Set these to zero to ensure unit uses values from first cycle.
      dut.io.fetchedInstruction.valid.poke(false.B)
      dut.io.fetchedInstruction.bits.instruction.poke(0.U)
      dut.io.fetchedInstruction.bits.thread.poke(0.U)

      dut.io.decodedInstruction.valid.expect(true.B)

      for (i <- 0 until cfg.shaderVectorLanes) {
        dut.io.decodedInstruction.bits.operand1(i).expect(1234.U)
      }
    }
  }

  test("InstructionDecodeStage constants") {
    simulate(new InstructionDecodeStage()) { dut =>
      dut.io.fetchedInstruction.valid.poke(true.B)
      dut.io.fetchedInstruction.bits.thread.poke(1.U)

      def floatToUInt(fval: Float): UInt = (java.lang.Float.floatToIntBits(fval) & 0xffffffffL).U
      def intToUInt(ival: Int): UInt = (ival & 0xffffffffL).U

      val constants = Seq(
        (116, intToUInt(0)), // constant 0
        (117, intToUInt(1)),
        (118, intToUInt(-1)),
        (119, intToUInt(2)),
        (120, intToUInt(4)),
        (121, floatToUInt(0.5f)),
        (122, floatToUInt(-0.5f)),
        (123, floatToUInt(1.0f)),
        (124, floatToUInt(-1.0f)),
        (125, floatToUInt(2.0f)),
        (126, floatToUInt(-2.0f))
      )

      // Test as first operand
      for ((regId, value) <- constants) {
        dut.io.fetchedInstruction.valid.poke(true.B)
        dut.io.fetchedInstruction.bits.instruction.poke(rInst(OpCode.Subf, 0, regId, 0).U)
        dut.clock.step(1)

        // Set these to zero to ensure unit uses values from first cycle.
        dut.io.fetchedInstruction.valid.poke(false.B)
        dut.io.fetchedInstruction.bits.instruction.poke(0.U)


        for (i <- 0 until cfg.shaderVectorLanes) {
          dut.io.decodedInstruction.bits.operand1(i).expect(value)
        }
      }

      // Test as second operand
      for ((regId, value) <- constants) {
        dut.io.fetchedInstruction.bits.instruction.poke(rInst(OpCode.Subf, 0, 0, regId).U)
        dut.clock.step(1)

        for (i <- 0 until cfg.shaderVectorLanes) {
          dut.io.decodedInstruction.bits.operand2(i).expect(value)
        }
      }
    }
  }

  test("InstructionDecodeStage read vector lane id") {
    simulate(new InstructionDecodeStage()) { dut =>
      dut.io.fetchedInstruction.valid.poke(true.B)
      dut.io.fetchedInstruction.bits.thread.poke(1.U)

      dut.io.fetchedInstruction.bits.instruction.poke(rInst(OpCode.Subf, 0, 223, 0).U) // lane id
      dut.clock.step(1)

      // Set these to zero to ensure unit uses values from first cycle.
      dut.io.fetchedInstruction.valid.poke(false.B)
      dut.io.fetchedInstruction.bits.instruction.poke(0.U)

      for (i <- 0 until cfg.shaderVectorLanes) {
        dut.io.decodedInstruction.bits.operand1(i).expect(i.U)
      }
    }
  }

  test("InstructionDecodeStage writeback multi-thread") {
    simulate(new InstructionDecodeStage()) { dut =>
      writeScalar(dut, 0, 2, 111)
      writeScalar(dut, 1, 2, 222)

      dut.io.fetchedInstruction.valid.poke(true.B)
      dut.io.fetchedInstruction.bits.thread.poke(0.U)
      dut.io.fetchedInstruction.bits.instruction.poke(rInst(OpCode.Subf, 0, 2, 0).U)
      dut.clock.step(1)

      for (i <- 0 until cfg.shaderVectorLanes) {
        dut.io.decodedInstruction.bits.operand1(i).expect(111.U)
      }

      dut.io.fetchedInstruction.bits.thread.poke(1.U)
      dut.io.fetchedInstruction.bits.instruction.poke(rInst(OpCode.Subf, 0, 2, 0).U)
      dut.clock.step(1)

      for (i <- 0 until cfg.shaderVectorLanes) {
        dut.io.decodedInstruction.bits.operand1(i).expect(222.U)
      }
    }
  }

  test("InstructionDecodeStage exec mask multi-thread") {
    simulate(new InstructionDecodeStage()) { dut =>
      writeScalar(dut, 0, EXEC_MASK, "b01010101".U.litValue.toInt)
      writeScalar(dut, 1, EXEC_MASK, "b10101010".U.litValue.toInt)

      dut.io.fetchedInstruction.valid.poke(true.B)
      dut.io.fetchedInstruction.bits.thread.poke(0.U)
      dut.io.fetchedInstruction.bits.instruction.poke(rInst(OpCode.Subf, 0, EXEC_MASK, 0).U) // read exec mask
      dut.clock.step(1)

      dut.io.decodedInstruction.bits.operand1(0).expect("b01010101".U)

      dut.io.fetchedInstruction.bits.thread.poke(1.U)
      dut.io.fetchedInstruction.bits.instruction.poke(rInst(OpCode.Subf, 0, EXEC_MASK, 0).U) // read exec mask
      dut.clock.step(1)

      dut.io.decodedInstruction.bits.operand1(0).expect("b10101010".U)
    }
  }

  test("InstructionDecodeStage hasWriteback") {
    simulate(new InstructionDecodeStage()) { dut =>
      dut.io.fetchedInstruction.valid.poke(true.B)
      dut.io.fetchedInstruction.bits.thread.poke(1.U)
      dut.io.fetchedInstruction.bits.instruction.poke(rInst(OpCode.Subf, 0, 2, 0).U) // has writeback
      dut.clock.step(1)

      dut.io.decodedInstruction.bits.meta.hasWriteback.expect(true.B)

      dut.io.fetchedInstruction.bits.instruction.poke(rInst(OpCode.Bnz, 0, 2, 0).U) // no writeback
      dut.clock.step(1)

      dut.io.decodedInstruction.bits.meta.hasWriteback.expect(false.B)
    }
  }

  test("InstructionDecodeStage read shader reg ready") {
    simulate(new InstructionDecodeStage()) { dut =>

      dut.io.fetchedInstruction.valid.poke(true.B)
      dut.io.fetchedInstruction.bits.thread.poke(1.U)
      dut.io.fetchedInstruction.bits.jobId.poke(15.U)
      dut.io.fetchedInstruction.bits.instruction.poke(rInst(OpCode.Or, 0, 224, CONST_ZERO).U)

      dut.io.regRead.valid.expect(true.B)
      dut.io.regRead.bits.addr.expect(0.U) // Corresponds to register 224
      dut.io.regRead.bits.jobId.expect(15.U)
      dut.clock.step(1)
      dut.io.fetchedInstruction.valid.poke(false.B)

      dut.io.regRead.valid.expect(false.B)
      dut.io.regReadData.valid.poke(true.B)
      for (lane <- 0 until cfg.shaderVectorLanes) {
        dut.io.regReadData.bits(lane).poke((100 + lane).U)
      }

      for (lane <- 0 until cfg.shaderVectorLanes) {
        dut.io.decodedInstruction.bits.operand1(lane).expect((100 + lane).U)
      }

      dut.io.decodedInstruction.valid.expect(true.B)
    }
  }

  test("InstructionDecodeStage read shader reg not ready") {
    simulate(new InstructionDecodeStage()) { dut =>
      dut.io.fetchedInstruction.valid.poke(true.B)
      dut.io.fetchedInstruction.bits.thread.poke(1.U)
      dut.io.fetchedInstruction.bits.jobId.poke(15.U)
      dut.io.fetchedInstruction.bits.instruction.poke(rInst(OpCode.Or, 0, 224, CONST_ZERO).U)

      dut.io.regRead.valid.expect(true.B)
      dut.io.regRead.bits.addr.expect(0.U) // Corresponds to register 224
      dut.io.regRead.bits.jobId.expect(15.U)
      dut.clock.step(1)

      dut.io.regReadData.valid.poke(false.B) // Not ready

      dut.io.fetchedInstruction.valid.poke(false.B)
      dut.io.fetchedInstruction.bits.thread.poke(10.U)

      // Ensure the output is not valid, since the result was not ready.
      dut.io.decodedInstruction.valid.expect(false.B)
      dut.io.ioWaitThread.valid.expect(true.B)
      dut.io.ioWaitThread.bits.expect(1.U)
    }
  }

  test("InstructionDecodeStage write shader reg") {
    simulate(new InstructionDecodeStage()) { dut =>
      dut.io.writeback.valid.poke(true.B)
      dut.io.writeback.bits.thread.poke(1.U)
      dut.io.writeback.bits.jobId.poke(15.U)
      dut.io.writeback.bits.destReg.poke(241.U)
      for (lane <- 0 until cfg.shaderVectorLanes) {
        dut.io.writeback.bits.value(lane).poke((200 + lane).U)
      }

      dut.io.regWrite.valid.expect(true.B)
      dut.io.regWrite.bits.addr.expect(1.U) // Corresponds to register 241
      dut.io.regWrite.bits.jobId.expect(15.U)
      for (lane <- 0 until cfg.shaderVectorLanes) {
        dut.io.regWrite.bits.data(lane).expect((200 + lane).U)
      }
    }
  }
}
