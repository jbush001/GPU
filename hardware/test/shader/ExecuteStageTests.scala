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

import gpu._

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funsuite.AnyFunSuite

class ExecuteStageTests extends AnyFunSuite with ChiselSim {
  implicit val cfg: GpuConfig = new GpuConfig

  val execLatency = 3 // Must match ExecuteStage.scala

  def floatToRawBits(fval: Float) = java.lang.Float.floatToIntBits(fval) & 0xffffffffL

  test("ExecuteStage fp multiply") {
    simulate(new ExecuteStage) { dut =>
      dut.io.in.valid.poke(true.B)
      dut.io.in.bits.opcode.poke(OpCode.Mulf.U)
      dut.io.in.bits.operand1(0).poke(floatToRawBits(6.0f).U)
      dut.io.in.bits.operand2(0).poke(floatToRawBits(2.5f).U)
      dut.clock.step()
      dut.io.in.bits.operand1(0).poke(0.U)
      dut.io.in.bits.operand2(0).poke(0.U)
      dut.io.in.valid.poke(false.B)
      dut.io.result.valid.expect(false.B)
      dut.clock.step()
      dut.io.result.valid.expect(false.B)
      dut.clock.step()
      dut.io.result.valid.expect(true.B)
      dut.io.result.bits(0).expect(floatToRawBits(15.0f).U) // 6.0 * 2.5
      dut.clock.step()
      dut.io.result.valid.expect(false.B)
    }
  }

  test("ExecuteStage fp add") {
    simulate(new ExecuteStage) { dut =>
      dut.io.in.valid.poke(true.B)
      dut.io.in.bits.opcode.poke(OpCode.Addf.U)
      dut.io.in.bits.operand1(0).poke(floatToRawBits(6.0f).U)
      dut.io.in.bits.operand2(0).poke(floatToRawBits(2.5f).U)
      dut.clock.step(3)
      dut.io.result.valid.expect(true.B)
      dut.io.result.bits(0).expect(floatToRawBits(8.5f).U) // 6.0 + 2.5
    }
  }

  test("ExecuteStage fp subtract") {
    simulate(new ExecuteStage) { dut =>
      dut.io.in.valid.poke(true.B)
      dut.io.in.bits.opcode.poke(OpCode.Subf.U)
      dut.io.in.bits.operand1(0).poke(floatToRawBits(6.0f).U)
      dut.io.in.bits.operand2(0).poke(floatToRawBits(2.5f).U)
      dut.clock.step(3)
      dut.io.result.valid.expect(true.B)
      dut.io.result.bits(0).expect(floatToRawBits(3.5f).U) // 6.0 - 2.5
    }
  }

  test("ExecuteStage single cycle ops") {
    simulate(new ExecuteStage) { dut =>
      type TestVector = (Int, Long, Long, Long)
      val testVectors: Seq[TestVector] = Seq(
        (OpCode.Addi, 5, 3, 8),
        (OpCode.Subi, 5, 3, 2),
        (OpCode.Muli, 5, 3, 15),
        (OpCode.Mulih, 0x0000_0001L, 0x0000_0002L, 0x0000_0000L),
        //(OpCode.Mulih, 0x0000_0001L, 0x8000_0000L, 0x0000_0001L), fixme
        (OpCode.Lsl, 5, 1, 10),
        (OpCode.Asr, 0x8000_0000L, 1, 0xC000_0000L),
        (OpCode.Lsr, 0x8000_0000L, 1, 0x4000_0000L)
      )

      for (cycle <- 0 until testVectors.length + execLatency) {
        if (cycle < testVectors.length) {
          val (opcode, op1, op2, expected) = testVectors(cycle)
          dut.io.in.valid.poke(true.B)
          dut.io.in.bits.opcode.poke(opcode.U)
          for (lane <- 0 until dut.cfg.shaderVectorLanes) {
            dut.io.in.bits.operand1(lane).poke((op1 & 0xffffffffL).U)
            dut.io.in.bits.operand2(lane).poke((op2 & 0xffffffffL).U)
          }
        } else {
          dut.io.in.valid.poke(false.B)
        }

        if (cycle >= execLatency) {
          val (opcode, op1, op2, expected) = testVectors(cycle - execLatency)
          dut.io.result.valid.expect(true.B)
          for (lane <- 0 until dut.cfg.shaderVectorLanes) {
            val result = dut.io.result.bits(lane).peek().litValue
            if (result != (expected & 0xffffffffL)) {
              fail(s"Test failed for opcode $opcode, lane $lane got $result, expected=${expected & 0xffffffffL}")
            }
          }
        } else {
          dut.io.result.valid.expect(false.B)
        }

        dut.clock.step()
      }
    }
  }

  test("ExecuteStage setxxx") {
    simulate(new ExecuteStage) { dut =>
      // opcode, operand1, operand2, expected result
      type TestVector = (Int, Long, Long, Boolean)
      val testVectors: Seq[TestVector] = Seq(
        (OpCode.Setgtf, floatToRawBits(5.0f), floatToRawBits(3.0f), true),
        (OpCode.Setgtf, floatToRawBits(3.0f), floatToRawBits(5.0f), false),
        (OpCode.Setgei, 4, 5, false),
        (OpCode.Setgei, 5, 5, true),
        (OpCode.Setlti, 5, 3, false),
        (OpCode.Setlti, 3, 5, true),
        (OpCode.Setgeu, -1, 0, true), // unsigned comparison
        (OpCode.Setltu, 0, -1, true), // unsigned comparison
        (OpCode.Seteq, 5, 5, true),
        (OpCode.Seteq, 5, 4, false),
        (OpCode.Setne, 5, 4, true),
        (OpCode.Setne, 5, 5, false)
      )

      for (cycle <- 0 until testVectors.length + execLatency) {
        if (cycle < testVectors.length) {
          val (opcode, op1, op2, expected) = testVectors(cycle)
          dut.io.in.valid.poke(true.B)
          dut.io.in.bits.opcode.poke(opcode.U)
          for (lane <- 0 until dut.cfg.shaderVectorLanes) {
            dut.io.in.bits.operand1(lane).poke((op1 & 0xffffffffL).U)
            dut.io.in.bits.operand2(lane).poke((op2 & 0xffffffffL).U)
          }
        } else {
          dut.io.in.valid.poke(false.B)
        }

        if (cycle >= execLatency) {
          val (opcode, op1, op2, expected) = testVectors(cycle - execLatency)
          dut.io.result.valid.expect(true.B)
          val result = dut.io.result.bits(0).peek().litValue
          if (result != (if (expected) 0xff else 0)) {
            fail(s"Test failed for opcode $opcode, got $result, expected=${if (expected) 0xff else 0}")
          }
        } else {
          dut.io.result.valid.expect(false.B)
        }

        dut.clock.step()
      }
    }
  }
}
