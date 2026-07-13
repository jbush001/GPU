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
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funsuite.AnyFunSuite

/** Represents a single precision floating point value, in IEEE754 binary32 
  * format.
  * [[https://en.wikipedia.org/wiki/Single-precision_floating-point_format]]
  */
class Float32 extends Bundle {
  val raw = Bits(32.W)

  def negative = raw(31)
  def exponent = raw(30, 23).asUInt
  def fraction = raw(22, 0).asUInt

  def isNaN = (this.exponent === 0xff.U && this.fraction =/= 0.U)
  def isInf = (this.exponent === 0xff.U && this.fraction === 0.U)

  // Note: we don't support subnormal numbers, so treat them as zero.
  def isZero = this.exponent === 0.U

  // This adds the leading hidden bit
  def fullFraction = (!this.isZero ## this.fraction).asUInt

  def absLargerThan(that: Float32): Bool = {
    ((this.exponent > that.exponent)
      || ((this.exponent === that.exponent)
      && this.fullFraction >= that.fullFraction))
  }
}

object Float32 {
  val exponentWidth = 8
  val fractionWidth = 23 // As encoded (not including hidden bit)
  def exponentBias = 127.U(exponentWidth.W) // This is an exponent of zero

  def apply() = new Float32()
}

/** These blocks
  *
  *   - Do not support subnormal numbers and treats them as zero
  *   - Only support round towards zero
  */
trait FloatingPointBlock

/**
  * This has 3 cycles of latency
  */
class FpAddSub extends Module with FloatingPointBlock {
  val io = IO(new Bundle {
    val result = Output(Float32())
    val operand1 = Input(Float32())
    val operand2 = Input(Float32())
    val subtract = Input(Bool())
  })

  //
  // - Determine which operand has the larger absolute value and swap
  //   conditionally into proper lanes
  // - Compute alignment shift count, shift smaller value to align
  // - Check for special cases: inf/NaN
  //
  val stage1 = new {
    val op1IsLarger = io.operand1.absLargerThan(io.operand2)
    val exponentDiff = Mux(op1IsLarger,
      io.operand1.exponent - io.operand2.exponent,
      io.operand2.exponent - io.operand1.exponent)

    val alignShift = Wire(UInt(log2Up(Float32.fractionWidth).W))
    when (exponentDiff <= (Float32.fractionWidth + 1).U) {
      alignShift := exponentDiff(4, 0)
    }.otherwise {
      alignShift := (Float32.fractionWidth + 1).U
    }

    val largerFractionNext = Mux(op1IsLarger, io.operand1.fullFraction, io.operand2.fullFraction)
    val smallerFraction = Mux(op1IsLarger, io.operand2.fullFraction, io.operand1.fullFraction)
    val smallerFractionAlignedNext = smallerFraction >> alignShift

    val logicalSubtractNext = io.operand1.negative ^ io.operand2.negative ^ io.subtract
    val isNanNext = (io.operand1.isNaN || io.operand2.isNaN
      || (io.operand1.isInf && io.operand2.isInf && logicalSubtractNext))
    val isNaN = RegNext(isNanNext, false.B)
    val isInf = RegNext(!isNanNext && (io.operand1.isInf || io.operand2.isInf), false.B)
    val logicalSubtract = RegNext(logicalSubtractNext, false.B)
    val resultExponent = RegNext(Mux(op1IsLarger, io.operand1.exponent, io.operand2.exponent), 0.U)

    // Value with larger magnitude wins
    val resultNegative = RegNext(Mux(op1IsLarger, io.operand1.negative, io.operand2.negative ^ io.subtract), false.B)
    val largerFraction = RegNext(largerFractionNext, 0.U)
    val smallerFractionAligned = RegNext(smallerFractionAlignedNext, 0.U(24.W))
  }

    val sumResultWidth = Float32.fractionWidth + 2

  //
  // - Add/subtract aligned fractions
  //
  val stage2 = new {
    val sumResult = RegNext(Mux(stage1.logicalSubtract,
      stage1.largerFraction.pad(sumResultWidth) - stage1.smallerFractionAligned.pad(sumResultWidth),
      stage1.largerFraction.pad(sumResultWidth) + stage1.smallerFractionAligned.pad(sumResultWidth)), 
      0.U(sumResultWidth.W))
    val exponent = RegNext(stage1.resultExponent, 0.U)
    val resultNegative = RegNext(stage1.resultNegative, false.B)
    val isNaN = RegNext(stage1.isNaN, false.B)
    val isInf = RegNext(stage1.isInf, false.B)
  }

  //
  // - Find leading zero, shift to renormalize
  //
  val stage3 = new {
    val isZeroResult = stage2.sumResult === 0.U
    val normalizeShift = Wire(UInt(log2Up(sumResultWidth).W))
    normalizeShift := sumResultWidth.U
    for (i <- (sumResultWidth - 1) to 0 by -1) {
      when (stage2.sumResult(sumResultWidth - 1 - i)) {
        normalizeShift := i.U
      }
    }

    val normalizedSum = (stage2.sumResult << normalizeShift)(Float32.fractionWidth, 1)

    val resultFraction = WireInit(0.U(Float32.fractionWidth.W))
    val resultExponent = WireInit(0.U(Float32.exponentWidth.W))
    when (stage2.isInf) {
      resultFraction := 0.U
      resultExponent :=  0xff.U(Float32.exponentWidth.W)
    }.elsewhen (stage2.isNaN) {
      resultFraction := 0x400000.U
      resultExponent := 0xff.U(Float32.exponentWidth.W)
    }.elsewhen (isZeroResult) {
      resultFraction := 0.U
      resultExponent := 0.U
    }.otherwise {
      resultFraction := normalizedSum
      resultExponent := stage2.exponent + 1.U - normalizeShift
    }

    val resultNegative = stage2.resultNegative && !isZeroResult

    io.result := RegNext(resultNegative ## resultExponent ## resultFraction, 0.U).asTypeOf(Float32())
  }
}

/**
 * This has 3 cycles of latency
 */
class FpMul extends Module with FloatingPointBlock {
  val io = IO(new Bundle {
    val result = Output(Float32())
    val operand1 = Input(Float32())
    val operand2 = Input(Float32())
  })

  //
  // Add exponents, multiply fractions
  //
  val stage1 = new {
    val isNanNext = (io.operand1.isNaN || io.operand2.isNaN
      || (io.operand1.isInf && io.operand2.isZero)
      || (io.operand1.isZero && io.operand2.isInf))

    val mulExpSum = io.operand1.exponent.pad(10) + io.operand2.exponent.pad(10) -
      Float32.exponentBias
    val mulExponentUnderflow = mulExpSum(Float32.exponentWidth + 1)
    val mulExponentCarry = mulExpSum(Float32.exponentWidth)
    val mulExponentNext = mulExpSum(Float32.exponentWidth - 1, 0)

    val isInfNext = (io.operand1.isInf || io.operand2.isInf
      || (mulExponentCarry && !mulExponentUnderflow))
    val isZeroNext = io.operand1.isZero || io.operand2.isZero || mulExponentUnderflow

    val fractionProductNext = (io.operand1.fullFraction * io.operand2.fullFraction)(47, 23)

    val isZero = RegNext(isZeroNext, false.B)
    val isNaN = RegNext(isNanNext, false.B)
    val isInf = RegNext(isInfNext, false.B)
    val isNegative = RegNext(io.operand1.negative ^ io.operand2.negative, false.B)
    val mulExponent = RegNext(mulExponentNext, 0.U)
    val fractionProduct = RegNext(fractionProductNext, 0.U)
  }

  //
  // This stage is a passthrough. Synthesis tools like Vivado can absorb
  // registers (specifically fractionProduct in this case) into the DSP
  // multiplier blocks to take advantage of their pipelining capabilities.
  // (e.g. Vivado Design Suite User Guide UG901, chapter 4)
  //
  val stage2  = new {
    val isZero = RegNext(stage1.isZero, false.B)
    val isNaN = RegNext(stage1.isNaN, false.B)
    val isInf = RegNext(stage1.isInf, false.B)
    val isNegative = RegNext(stage1.isNegative, false.B)
    val mulExponent = RegNext(stage1.mulExponent, 0.U)
    val fractionProduct = RegNext(stage1.fractionProduct, 0.U)
  }

  val stage3 = new {
    // One position shift to normalize if the product has overflown
    val normShift = stage2.fractionProduct(24)
    val normalizedFraction = Mux(normShift,
      stage2.fractionProduct(Float32.fractionWidth, 1),
      stage2.fractionProduct(Float32.fractionWidth - 1, 0))
    val adjustedExponent = Mux(normShift, stage2.mulExponent + 1.U, stage2.mulExponent)

    val resultNext = WireInit(0.U(32.W))
    when (stage2.isNaN) {
      resultNext := false.B ## 0xff.U(Float32.exponentWidth.W) ## 0x400000.U(Float32.fractionWidth.W)
    }.elsewhen (stage2.isInf) {
      resultNext := stage2.isNegative ## 0xff.U(Float32.exponentWidth.W) ## 0.U(Float32.fractionWidth.W)
    }.elsewhen (stage2.isZero) {
      resultNext := stage2.isNegative ## 0.U(31.W)
    }.otherwise {
      resultNext := stage2.isNegative ## adjustedExponent ## normalizedFraction
    }

    io.result.raw := RegNext(resultNext, 0.U)
  }
}

/**
 * This has one cycle of latency
 */
class FpReciprocalEstimate extends Module with FloatingPointBlock {
  val io = IO(new Bundle {
    val result = Output(Float32())
    val operand = Input(Float32())
  })

  // Generate the fraction lookup table.
  // Because the floating point significant is normalized, its value ranges
  // from [1.0, 2.0). The reciprocal of this range therefore spans (0.5, 1.0].
  // For the table calculation, we treat the table entries as 1.6 fixed point
  // numbers, so the numerator for our calculations is 64 * 64. However, 0.5 is
  // not representable in a normalized value, so we need to also multiply by
  // two (we will compensate in hardware by shifting and adjusting the exponent 
  // to renormalize)
  val numEntries = 64 // Must be a power of two
  val entryWidth = log2Up(numEntries)
  val numerator = (numEntries * numEntries * 2)
  val romValues = Array.tabulate[UInt](numEntries)(i =>
    ((numerator / (numEntries + i)) & (numEntries - 1)).U(entryWidth.W))

  val reciprocalRom = VecInit(romValues.toIndexedSeq)

  // Read value out of lookup table
  val fractionNext = reciprocalRom(io.operand.fraction(22, 17))

  // Adjust the exponent. Note we subtract 1-2 extra values out of the exponent
  // to compensate for the normalization shift that occurs below.
  // In the case of zero, there's nothing to normalize.
  val normalizationCorrection = io.operand.fraction(22, 17) === 0.U
  val exponentNext = 253.U - io.operand.exponent + normalizationCorrection.asUInt

  val resultNext = Wire(UInt(32.W))
  when (io.operand.isZero || io.operand.isNaN) {
    // Division by zero or NaN = NaN
    resultNext := false.B ## 0xff.U(Float32.exponentWidth.W) ## 0x400000.U(Float32.fractionWidth.W)
  }.elsewhen (io.operand.isInf) {
    // Division by +/- inf = 0.0
    resultNext := io.operand.negative ##  0.U(8.W) ## 0.U(23.W)
  }.otherwise {
    resultNext := io.operand.negative ## exponentNext ## (fractionNext << 17).pad(Float32.fractionWidth)
  }

  io.result.raw := RegNext(resultNext, 0.U)
}

// This has one cycle of latency
class FpToInt extends Module with FloatingPointBlock {
  val io = IO(new Bundle {
    val result = Output(UInt(32.W))
    val operand = Input(Float32())
  })

  val resultNext = Wire(UInt(32.W))
  when (io.operand.exponent < Float32.exponentBias || io.operand.isNaN) {
    // Number is less than zero
    resultNext := 0.U
  }.elsewhen (io.operand.exponent > Float32.exponentBias + 30.U || io.operand.isInf) {
    // Number is too large to fit, set to infinity
    when (io.operand.negative) {
      resultNext := 0x80000000L.U
    }.otherwise {
      resultNext := 0x7fffffff.U
    }
  }.otherwise {
    // Shift to align whole portion
    val shiftAmount = Float32.exponentBias - io.operand.exponent + 32.U
    val shifted = ((io.operand.fullFraction ## 0.U((32 - Float32.fractionWidth).W)) >>
      shiftAmount).asUInt.pad(32)
    resultNext := Mux(io.operand.negative,
      (shifted ^ 0xffffffffL.U(32.W)) + 1.U,
      shifted)
  }

  io.result := RegNext(resultNext, 0.U)
}

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