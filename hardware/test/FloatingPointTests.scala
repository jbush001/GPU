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

class FloatingPointTests extends AnyFunSuite with ChiselSim {
  // Helper for reciprocal tests
  def fpTruncate(value: Float): Float =
    java.lang.Float.intBitsToFloat(java.lang.Float.floatToIntBits(value) & 0xfffe0000)

  def floatToRawBits(fval: Float) = java.lang.Float.floatToIntBits(fval) & 0xffffffffL

  def runFpPipelineTest[T <: Module, V](
    dut: T,
    pipelineDelay: Int,
    testVectors: Seq[V],
    driveInput: (T, V) => Unit,
    verifyOutput: (T, V, Int) => Unit
  ) = {
    for (cycle <- 0 until testVectors.length + pipelineDelay) {
      if (cycle < testVectors.length) {
        driveInput(dut, testVectors(cycle))
      }

      if (cycle >= pipelineDelay) {
        verifyOutput(dut, testVectors(cycle - pipelineDelay), cycle - pipelineDelay)
      }

      dut.clock.step()
    }
  }

  def reportTestFailure(index: Int, a: Float, b: Float, expected: Float, actual: Float) = {
    println(f"mismatch at test entry $index: $a%.3f $b%.3f")
    println(f"  expected = $expected%.6f  (0x${this.floatToRawBits(expected)}%08x)")
    println(f"  actual   = $actual%.6f  (0x${this.floatToRawBits(actual)}%08x)")
  }

  test("fp adder") {
   simulate(new FpAddSub()) { dut =>
      type TestVector = (Boolean, Float, Float, Float)
      val testVectors: Seq[TestVector] = Seq(
        (false, 3.0f, 2.0f, 5.0f), // pos + pos
        (false, -4.0f, -5.0f, -9.0f), // neg + neg
        (false, 7.7f, -3.5f, 4.2f), // pos + smaller neg
        (false, 7.0f, -13.0f, -6.0f), // pos + larger neg
        (false, -40.0f, 37.0f, -3.0f), // neg + smaller pos
        (false, -27.0f, 35.0f, 8.0f), // neg + larger pos
        (false, 5.0f, -5.0f, 0.0f), // Exact cancellation
        (false, -5.0f, 5.0f, 0.0f),
        (false, 17.79f, 19.32f, 37.11f), // Exponents equal. Will carry into next significand bit
        (false, 0.34f, 44.23f, 44.57f), // Exponent 2 larger
        (false, 44.23f, 0.034f, 44.264f), // Exponent 1 larger
        (false, -1.0f, 5.0f, 4.0f), // First element is negative and has smaller exponent
        (false, -5.0f, 1.0f, -4.0f), // First element is negative and has larger exponent
        (false, 5.0f, -1.0f, 4.0f), // Second element is negative and has smaller exponent
        (false, 1.0f, -5.0f, -4.0f), // Second element is negative and has larger exponent
        (false, 5.0f, 0.0f, 5.0f), // Zero identity
        (false, 0.0f, 5.0f, 5.0f), // " "
        (false, 0.0f, 0.0f, 0.0f), // " "
        (false, 7.0f, -7.0f, 0.0f), // Sum is zero, positive first operand
        (false, -7.0f, 7.0f, 0.0f), // Sum is zero, negative first operand
        (false, 1000000.0f, 0.0000001f, 1000000.0f), //  Second op is lost because of precision
        (false, 0.0000001f, 0.00000001f, 0.00000011f), // Very small number
        (false, 1000000.0f, 10000000.0f, 11000000.0f), // Large number
        (false, -0.0f, 2.323f, 2.323f), // negative zero
        (false, 2.323f, -0.0f, 2.323f), // negative zero
        (false, Float.PositiveInfinity, Float.PositiveInfinity, Float.PositiveInfinity), // Infinity and NaN cases...
        (false, Float.PositiveInfinity, 1.0f, Float.PositiveInfinity),
        (false, Float.NegativeInfinity, 1.0f, Float.NegativeInfinity),
        (false, 0.0f, Float.NegativeInfinity, Float.NegativeInfinity),
        (false, 1.0f, Float.PositiveInfinity, Float.PositiveInfinity),
        (false, 1.0f, Float.NegativeInfinity, Float.NegativeInfinity),
        (false, Float.PositiveInfinity, Float.NegativeInfinity, Float.NaN),
        (false, Float.NaN, 1.0f, Float.NaN),
        (false, 1.0f, Float.NaN, Float.NaN),
        (false, Float.NaN, Float.NaN, Float.NaN),

        // Subtraction
        (true, 3.0f, 2.0f, 1.0f),
        (true, -3.0f, -2.0f, -1.0f),
        (true, 3.0f, -2.0f, 5.0f),
        (true, -3.0f, 2.0f, -5.0f),
        (true, 2.0f, -3.0f, 5.0f),
        (true, -2.0f, 3.0f, -5.0f)
      )

      dut.io.operand1.raw.poke(0)
      dut.io.operand2.raw.poke(0)
      dut.clock.step() // Wait for reset to complete

      runFpPipelineTest(
        dut,
        3,
        testVectors,
        (dut:FpAddSub, test: TestVector) => {
          dut.io.subtract.poke(test._1)
          dut.io.operand1.raw.poke(this.floatToRawBits(test._2))
          dut.io.operand2.raw.poke(this.floatToRawBits(test._3))
        },
        (dut: FpAddSub, test: TestVector, index: Int) => {
          val expectedBits = this.floatToRawBits(test._4)
          val actualBits: Long = dut.io.result.raw.peek().litValue.toLong & 0xffffffffL
          if (math.abs(expectedBits - actualBits) > 1) {
            reportTestFailure(index, test._2, test._3, test._4,
              java.lang.Float.intBitsToFloat(actualBits.toInt))
            fail()
          }
        }
      )
    }
  }

  test("fp multiplier") {
    simulate(new FpMul()) { dut =>
      type TestVector = (Float, Float, Float)
      val testVectors: Seq[TestVector] = Seq(
        ( 100.0f, 25.0f, 2500.0f), // positive * positive
        ( -10.0f, 32.0f, -320.0f), // negative * positive
        ( 0.5f, -90.0f, -45.0f), // positive * negative
        ( -15.0f, -4.0f, 60.0f), // negative * negative
        ( -15.0f, 0.0f, -0.0f), // zero identity, negative
        ( 0.0f, 15.0f, 0.0f), // zero identity, positive
        ( 0.00001f, 12345.0f, 0.12345f),
        ( 200000.5f, 123.0f, 24600061.5f),
        ( 1.0E25f, 1.0E25f, Float.PositiveInfinity), // Overflow
        ( 1.0E-20f, 1.0E-20f, 0.0f), // Underflow
        ( Float.PositiveInfinity, Float.PositiveInfinity, Float.PositiveInfinity), // Infinity and NaN cases...
        ( Float.PositiveInfinity, 1.0f, Float.PositiveInfinity),
        ( Float.NegativeInfinity, 1.0f, Float.NegativeInfinity),
        ( Float.NegativeInfinity, -1.0f, Float.PositiveInfinity),
        ( Float.PositiveInfinity, -1.0f, Float.NegativeInfinity),
        ( 0.0f, Float.NegativeInfinity, Float.NaN),
        ( 0.0f, Float.PositiveInfinity, Float.NaN),
        ( Float.NegativeInfinity, 0.0f, Float.NaN),
        ( Float.PositiveInfinity, 0.0f, Float.NaN),
        ( Float.PositiveInfinity, Float.NegativeInfinity, Float.NegativeInfinity),
        ( Float.NaN, 1.0f, Float.NaN),
        ( 1.0f, Float.NaN, Float.NaN),
        ( Float.NaN, Float.NaN, Float.NaN),
        ( Float.NaN, Float.PositiveInfinity, Float.NaN),
        ( Float.PositiveInfinity, Float.NaN, Float.NaN),
        ( Float.NegativeInfinity, Float.NaN, Float.NaN),
        ( Float.NaN, Float.NegativeInfinity, Float.NaN),
      )

      runFpPipelineTest(
        dut,
        3,
        testVectors,
        (dut: FpMul, test: TestVector) => {
          dut.io.operand1.raw.poke(this.floatToRawBits(test._1))
          dut.io.operand2.raw.poke(this.floatToRawBits(test._2))
        },
        (dut: FpMul, test: TestVector, index: Int) => {
          val expectedBits = this.floatToRawBits(test._3)
          val actualBits: Long = dut.io.result.raw.peek().litValue.toLong & 0xffffffffL
          if (math.abs(expectedBits - actualBits) > 1) {
            reportTestFailure(index, test._1, test._2, test._3,
              java.lang.Float.intBitsToFloat(actualBits.toInt))
            fail()
          }
        }
      )
    }
  }

  test("fp recip") {
    simulate(new FpReciprocalEstimate()) { dut =>
      type TestVector = (Float, Float)
      val testVectors: Seq[TestVector] = Seq(
        (1.0f, 1.0f),
        (2.0f, 0.5f),
        (0.5f, 2.0f),
        (4.0f, 0.25f),
        (3.0f, this.fpTruncate(0.333333f)),
        (1.5f, this.fpTruncate(0.666666f)),
        (0.333333f, this.fpTruncate(3.0f)),
        (1000.0f, this.fpTruncate(0.001f)),
        (0.99999f, 1.0f), // Last table entry
        (0.0f, Float.NaN), // Division by zero
        (Float.NegativeInfinity, -0.0f), // Division by inf
        (Float.PositiveInfinity, 0.0f), // Division by inf
      )
      dut.io.operand.raw.poke(0)
      dut.clock.step() // Wait for reset to complete

      runFpPipelineTest(
        dut,
        1,
        testVectors,
        (dut: FpReciprocalEstimate, test: TestVector) => {
          dut.io.operand.raw.poke(this.floatToRawBits(test._1))
        },
        (dut: FpReciprocalEstimate, test: TestVector, index: Int) => {
          val expectedBits = this.floatToRawBits(test._2)
          val actualBits: Long = dut.io.result.raw.peek().litValue.toLong & 0xffffffffL
          if (math.abs(expectedBits - actualBits) > 1) {
            reportTestFailure(index, test._1, 0.0f, test._2,
              java.lang.Float.intBitsToFloat(actualBits.toInt))
            fail()
          }
        }
      )
    }
  }

  test("fp to int") {
    simulate(new FpToInt()) { dut =>
      type TestVector = (Float, Int)
      val testVectors: Seq[TestVector] = Seq(
        (1.0f, 1),
        (-1.0f, -1),
        (0.0f, 0),
        (1234.56f, 1234),
        (0.1234f, 0),
        (-5543.1f, -5543),
        (Float.NaN, 0),
        (Float.PositiveInfinity, Int.MaxValue),
        (Float.NegativeInfinity, Int.MinValue),
        (2000000000.0f, 2000000000),
        (-2000000000.0f, -2000000000),
        (3000000000.0f, Int.MaxValue),
        (-3000000000.0f, Int.MinValue),
        (1E+30f, Int.MaxValue),
        (-1E+30f, Int.MinValue),
        (1E-30f, 0),
        (-1E-30f, 0),
      )
      dut.io.operand.raw.poke(0.U)
      dut.clock.step() // Wait for reset to complete

      runFpPipelineTest(
        dut,
        1,
        testVectors,
        (dut: FpToInt, test: TestVector) => {
          dut.io.operand.raw.poke(this.floatToRawBits(test._1))
        },
        (dut: FpToInt, test: TestVector, index: Int) => {
          val expectedBits = test._2 & 0xffffffffL
          val actualBits: Long = dut.io.result.peek().litValue.toLong & 0xffffffffL
          if (expectedBits != actualBits) {
            println(f"mismatch at test entry $index: ${test._1}%.3f")
            println(f"  expected = (0x${expectedBits}%08x)")
            println(f"  actual   = (0x${actualBits}%08x)")
            fail()
          }
        }
      )
    }
  }
}
