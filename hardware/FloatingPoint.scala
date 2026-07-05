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

//
// Floating point blocks
// - This does not support subnormal numbers and treats them as zero
// - It only supports round towards zero
//

package gpu

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite

// IEEE754 single precision floating point value, binary32 format.
// https://en.wikipedia.org/wiki/Single-precision_floating-point_format
class Float32 extends Bundle {
  val raw = Bits(32 bits)

  def negative = raw(31)
  def exponent = raw(30 downto 23).asUInt
  def fraction = raw(22 downto 0).asUInt

  def isNaN = (this.exponent === 0xff && this.fraction =/= 0)
  def isInf = (this.exponent === 0xff && this.fraction === 0)

  // Note: we don't support subnormal numbers, so treat them as zero.
  def isZero = this.exponent === 0

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
  def exponentBias = U(127, exponentWidth bits) // This is an exponent of zero

  def apply() = new Float32()
}

// This has three cycles of latency
class FpAddSub extends Component {
  val io = new Bundle {
    val result = out(Float32())
    val operand1 = in(Float32())
    val operand2 = in(Float32())
    val subtract = in(Bool())
  }

  //
  // Stage 1
  // - Determine which operand has the larger absolute value and swap
  //   conditionally into proper lanes
  // - Compute alignment shift count, shift smaller value to align
  // - Check for special cases: inf/NaN
  //
  val stage1 = new Area {
    val op1IsLarger = io.operand1.absLargerThan(io.operand2)
    val exponentDiff = Mux(op1IsLarger,
      io.operand1.exponent - io.operand2.exponent,
      io.operand2.exponent - io.operand1.exponent)

    val alignShift = UInt(log2Up(Float32.fractionWidth) bits)
    when (exponentDiff <= Float32.fractionWidth + 1) {
      alignShift := exponentDiff(4 downto 0)
    } otherwise {
      alignShift := Float32.fractionWidth + 1
    }

    val largerFractionNext = Mux(op1IsLarger, io.operand1.fullFraction, io.operand2.fullFraction)
    val smallerFraction = Mux(op1IsLarger, io.operand2.fullFraction, io.operand1.fullFraction)
    val smallerFractionAlignedNext = smallerFraction >> alignShift

    val logicalSubtractNext = io.operand1.negative ^ io.operand2.negative ^ io.subtract
    val isNanNext = (io.operand1.isNaN || io.operand2.isNaN
      || (io.operand1.isInf && io.operand2.isInf && logicalSubtractNext))
    val isNaN = RegNext(isNanNext) init(False)
    val isInf = RegNext(!isNanNext && (io.operand1.isInf || io.operand2.isInf)) init(False)
    val logicalSubtract = RegNext(logicalSubtractNext) init(False)
    val resultExponent = RegNext(Mux(op1IsLarger, io.operand1.exponent, io.operand2.exponent)) init(0)

    // Value with larger magnitude wins
    val resultNegative = RegNext(Mux(op1IsLarger, io.operand1.negative, io.operand2.negative ^ io.subtract)) init(False)
    val largerFraction = RegNext(largerFractionNext) init(0)
    val smallerFractionAligned = RegNext(smallerFractionAlignedNext) init(0)
  }

  //
  // Stage 2
  // - Add/subtract
  //
  val stage2 = new Area {
    val resultWidth = Float32.fractionWidth + 2
    val sumResult = RegNext(Mux(stage1.logicalSubtract,
      stage1.largerFraction.resize(resultWidth) - stage1.smallerFractionAligned.resize(resultWidth),
      stage1.largerFraction.resize(resultWidth) + stage1.smallerFractionAligned.resize(resultWidth))) init(0)
    val exponent = RegNext(stage1.resultExponent) init(0)
    val resultNegative = RegNext(stage1.resultNegative) init(False)
    val isNaN = RegNext(stage1.isNaN) init(False)
    val isInf = RegNext(stage1.isInf) init(False)
  }

  //
  // Stage 3
  // - Find leading zero, shift to renormalize
  //
  val stage3 = new Area {
    val resultWidth = stage2.sumResult.getWidth
    val isZeroResult = stage2.sumResult === 0
    val normalizeShift = UInt(log2Up(resultWidth) bits)
    normalizeShift := resultWidth
    for (i <- (resultWidth - 1) downto 0) {
      when (stage2.sumResult(resultWidth - 1 - i)) {
        normalizeShift := i
      }
    }

    val normalizedSum = (stage2.sumResult << normalizeShift)(Float32.fractionWidth downto 1)

    val resultFraction = UInt(Float32.fractionWidth bits)
    val resultExponent = UInt(Float32.exponentWidth bits)
    when (stage2.isInf) {
      resultFraction := 0
      resultExponent :=  U(0xff, Float32.exponentWidth bits)
    } elsewhen (stage2.isNaN) {
      resultFraction := 0x400000
      resultExponent := U(0xff, Float32.exponentWidth bits)
    } elsewhen (isZeroResult) {
      resultFraction := 0
      resultExponent := 0
    } otherwise {
      resultFraction := normalizedSum
      resultExponent := stage2.exponent + 1 - normalizeShift
    }

    val resultNegative = stage2.resultNegative && !isZeroResult

    io.result.raw := RegNext(resultNegative ## resultExponent.asBits ## resultFraction.asBits) init(0)
  }
}

// This has three cycles of latency
class FpMul extends Component {
  val io = new Bundle {
    val result = out(Float32())
    val operand1 = in(Float32())
    val operand2 = in(Float32())
  }

  // Stage 1
  // Add exponents, multiply fractions
  val stage1 = new Area {
    val isNanNext = (io.operand1.isNaN || io.operand2.isNaN
      || (io.operand1.isInf && io.operand2.isZero)
      || (io.operand1.isZero && io.operand2.isInf))

    val mulExpSum = io.operand1.exponent.resize(10) + io.operand2.exponent.resize(10) -
      Float32.exponentBias
    val mulExponentUnderflow = mulExpSum(Float32.exponentWidth + 1)
    val mulExponentCarry = mulExpSum(Float32.exponentWidth)
    val mulExponentNext = mulExpSum(Float32.exponentWidth - 1 downto 0)

    val isInfNext = (io.operand1.isInf || io.operand2.isInf
      || (mulExponentCarry && !mulExponentUnderflow))
    val isZeroNext = io.operand1.isZero || io.operand2.isZero || mulExponentUnderflow

    val fractionProductNext = (io.operand1.fullFraction * io.operand2.fullFraction)(47 downto 23)

    val isZero = RegNext(isZeroNext) init(False)
    val isNaN = RegNext(isNanNext) init(False)
    val isInf = RegNext(isInfNext) init(False)
    val isNegative = RegNext(io.operand1.negative ^ io.operand2.negative) init(False)
    val mulExponent = RegNext(mulExponentNext) init(0)
    val fractionProduct = RegNext(fractionProductNext) init(0)
  }

  // This stage is a passthrough. Synthesis tools like Vivado can absorb
  // registers (specifically fractionProduct in this case) into the DSP
  // multiplier blocks to take advantage of their pipelining capabilities.
  // (e.g. Vivado Design Suite User Guide UG901, chapter 4)
  val stage2 = new Area {
    val isZero = RegNext(stage1.isZero) init(False)
    val isNaN = RegNext(stage1.isNaN) init(False)
    val isInf = RegNext(stage1.isInf) init(False)
    val isNegative = RegNext(stage1.isNegative) init(False)
    val mulExponent = RegNext(stage1.mulExponent) init(0)
    val fractionProduct = RegNext(stage1.fractionProduct) init(0)
  }

  val stage3 = new Area {
    // One position shift to normalize if the product has overflown
    val normShift = stage2.fractionProduct(24)
    val normalizedFraction = Mux(normShift,
      stage2.fractionProduct(Float32.fractionWidth downto 1),
      stage2.fractionProduct(Float32.fractionWidth - 1 downto 0))
    val adjustedExponent = Mux(normShift, stage2.mulExponent + 1, stage2.mulExponent)

    val resultNext = Bits(32 bits)
    when (stage2.isNaN) {
      resultNext := False ## B(0xff, Float32.exponentWidth bits) ## B(0x400000, Float32.fractionWidth bits)
    } elsewhen (stage2.isInf) {
      resultNext := stage2.isNegative ## B(0xff, Float32.exponentWidth bits) ## B(0, Float32.fractionWidth bits)
    } elsewhen (stage2.isZero) {
      resultNext := stage2.isNegative ## B(0, 31 bits)
    } otherwise {
      resultNext := stage2.isNegative ## adjustedExponent ## normalizedFraction
    }

    io.result.raw := RegNext(resultNext) init(0)
  }
}

// This has one cycle of latency
class FpReciprocalEstimate extends Component {
  val io = new Bundle {
    val result = out(Float32())
    val operand = in(Float32())
  }

  // Generate fraction lookup table.
  // Because the floating point significant is normalized, its value ranges
  // from [1.0, 2.0). The reciprocal of this range therefore is (0.5, 1.0].
  // For the table calculation, we treat the table entries as 1.6 fixed point
  // numbers, so the numerator for our calculations is 64^2. However, 0.5 is
  // not representable in a normalized value, so we need to also multiply by
  // two (we will compensate in hardware by adjusting the exponent to
  // renormalize)
  val numEntries = 64 // Must be a power of two
  val entryWidth = log2Up(numEntries)
  val numerator = (numEntries * numEntries * 2)
  val romValues = Array.tabulate[UInt](numEntries)(i =>
    U((numerator / (numEntries + i)) & (numEntries - 1), entryWidth bits))

  val reciprocalRom = Mem(UInt(entryWidth bits), romValues)

  // Read value out of lookup table
  val fractionNext = reciprocalRom.readAsync(io.operand.fraction(22 downto 17))

  // Adjust the exponent. Note we subtract 1-2 extra values out of the exponent
  // to compensate for the normalization shift that occurs below.
  // In the case of zero, there's nothing to normalize.
  val normalizationCorrection = io.operand.fraction(22 downto 17) === 0
  val exponentNext = 253 - io.operand.exponent + U(normalizationCorrection)

  val resultNext = Bits(32 bits)
  when (io.operand.isZero || io.operand.isNaN) {
    // Division by zero or NaN = NaN
    resultNext := False ## U(0xff, Float32.exponentWidth bits) ## B(0x400000, Float32.fractionWidth bits)
  } elsewhen (io.operand.isInf) {
    // Division by +/- inf = 0.0
    resultNext := io.operand.negative ## U(0, 8 bits) ## U(0, 23 bits)
  } otherwise {
    resultNext := io.operand.negative ## exponentNext ## B(fractionNext << 17, Float32.fractionWidth bits)
  }

  io.result.raw := RegNext(resultNext) init(0)
}

// This has one cycle of latency
class FpToInt extends Component {
  val io = new Bundle {
    val result = out(UInt(32 bits))
    val operand = in(Float32())
  }

  val resultNext = UInt(32 bits)
  when (io.operand.exponent < Float32.exponentBias || io.operand.isNaN) {
    // Number is less than zero
    resultNext := 0
  } elsewhen (io.operand.exponent > Float32.exponentBias + 30 || io.operand.isInf) {
    // Number is too large to fit, set to infinity
    when (io.operand.negative) {
      resultNext := U(0x80000000L, 32 bits)
    } otherwise {
      resultNext := 0x7fffffff
    }
  } otherwise {
    // Shift to align whole portion
    val shiftAmount = Float32.exponentBias - io.operand.exponent + 32
    val shifted = ((io.operand.fullFraction ## U(0, 32 - Float32.fractionWidth bits)) >>
      shiftAmount).asUInt.resize(32)
    resultNext := Mux(io.operand.negative,
      (shifted ^ U(0xffffffffL, 32 bits)) + 1,
      shifted)
  }

  io.result := RegNext(resultNext) init(0)
}

class FloatingPointTests extends AnyFunSuite {
  // Helper for reciprocal tests
  def fpTruncate(value: Float): Float =
    java.lang.Float.intBitsToFloat(java.lang.Float.floatToIntBits(value) & 0xfffe0000)

  def floatToRawBits(fval: Float) = java.lang.Float.floatToIntBits(fval).toLong & 0xffffffffL

  def runFpPipelineTest[T <: Component, V](
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

      dut.clockDomain.waitSampling()

      if (cycle >= pipelineDelay) {
        verifyOutput(dut, testVectors(cycle - pipelineDelay), cycle - pipelineDelay)
      }
    }
  }

  def reportTestFailure(index: Int, a: Float, b: Float, expected: Float, actual: Float) = {
    println(f"mismatch at test entry $index: $a%.3f $b%.3f")
    println(f"  expected = $expected%.6f  (0x${this.floatToRawBits(expected)}%08x)")
    println(f"  actual   = $actual%.6f  (0x${this.floatToRawBits(actual)}%08x)")
  }

  test("fp adder") {
    TestConfig.testSim.compile(new FpAddSub()).doSim { dut =>
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

      dut.clockDomain.forkStimulus(period = 10)
      dut.io.operand1.raw #= 0
      dut.io.operand2.raw #= 0
      dut.clockDomain.waitSampling() // Wait for reset to complete

      runFpPipelineTest(
        dut,
        3,
        testVectors,
        (dut:FpAddSub, test: TestVector) => {
          dut.io.subtract #= test._1
          dut.io.operand1.raw #= this.floatToRawBits(test._2)
          dut.io.operand2.raw #= this.floatToRawBits(test._3)
        },
        (dut: FpAddSub, test: TestVector, index: Int) => {
          val expectedBits = this.floatToRawBits(test._4)
          val actualBits: Long = dut.io.result.raw.toLong & 0xffffffffL
          if (math.abs(expectedBits - actualBits) > 1) {
            reportTestFailure(index, test._2, test._3, test._4,
              java.lang.Float.intBitsToFloat(actualBits.toInt))
            simFailure()
          }
        }
      )
    }
  }

  test("fp multiplier") {
    TestConfig.testSim.compile(new FpMul()).doSim { dut =>
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

      dut.clockDomain.forkStimulus(period = 10)
      dut.io.operand1.raw #= 0
      dut.io.operand2.raw #= 0
      dut.clockDomain.waitSampling() // Wait for reset to complete

      runFpPipelineTest(
        dut,
        3,
        testVectors,
        (dut: FpMul, test: TestVector) => {
          dut.io.operand1.raw #= this.floatToRawBits(test._1)
          dut.io.operand2.raw #= this.floatToRawBits(test._2)
        },
        (dut: FpMul, test: TestVector, index: Int) => {
          val expectedBits = this.floatToRawBits(test._3)
          val actualBits: Long = dut.io.result.raw.toLong & 0xffffffffL
          if (math.abs(expectedBits - actualBits) > 1) {
            reportTestFailure(index, test._1, test._2, test._3,
              java.lang.Float.intBitsToFloat(actualBits.toInt))
            simFailure()
          }
        }
      )
    }
  }

  test("fp recip") {
    TestConfig.testSim.compile(new FpReciprocalEstimate()).doSim { dut =>
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
      dut.clockDomain.forkStimulus(period = 10)
      dut.io.operand.raw #= 0
      dut.clockDomain.waitSampling() // Wait for reset to complete

      runFpPipelineTest(
        dut,
        1,
        testVectors,
        (dut: FpReciprocalEstimate, test: TestVector) => {
          dut.io.operand.raw #= this.floatToRawBits(test._1)
        },
        (dut: FpReciprocalEstimate, test: TestVector, index: Int) => {
          val expectedBits = this.floatToRawBits(test._2)
          val actualBits: Long = dut.io.result.raw.toLong & 0xffffffffL
          if (math.abs(expectedBits - actualBits) > 1) {
            reportTestFailure(index, test._1, 0.0f, test._2,
              java.lang.Float.intBitsToFloat(actualBits.toInt))
            simFailure()
          }
        }
      )
    }
  }

  test("fp to int") {
    TestConfig.testSim.compile(new FpToInt()).doSim { dut =>
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
      dut.clockDomain.forkStimulus(period = 10)
      dut.io.operand.raw #= 0
      dut.clockDomain.waitSampling() // Wait for reset to complete

      runFpPipelineTest(
        dut,
        1,
        testVectors,
        (dut: FpToInt, test: TestVector) => {
          dut.io.operand.raw #= this.floatToRawBits(test._1)
        },
        (dut: FpToInt, test: TestVector, index: Int) => {
          val expectedBits = test._2.toLong & 0xffffffffL
          val actualBits: Long = dut.io.result.toLong & 0xffffffffL
          if (expectedBits != actualBits) {
            println(f"mismatch at test entry $index: ${test._1}%.3f")
            println(f"  expected = (0x${expectedBits}%08x)")
            println(f"  actual   = (0x${actualBits}%08x)")
            simFailure()
          }
        }
      )
    }
  }
}