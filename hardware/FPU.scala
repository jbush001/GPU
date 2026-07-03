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
// Floating point pipeline
// - This does not support subnormal numbers and treats them as zero
// - It only supports round towards zero
//

package gpu

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite

// IEEE754 single precision floating point value, binary32 format.
// // https://en.wikipedia.org/wiki/Single-precision_floating-point_format
class SingleFloat extends Bundle {
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

  def absLargerThan(that: SingleFloat): Bool = {
    ((this.exponent > that.exponent)
      || ((this.exponent === that.exponent)
      && this.fullFraction >= that.fullFraction))
  }
}

object SingleFloat {
  def apply() = new SingleFloat()
}

object FpOperation extends SpinalEnum {
  val None, Add, Sub, Mul, Recip = newElement()
}

class FloatingPointPipeline extends Component {
  val io = new Bundle {
    val result = out(SingleFloat())
    val operand1 = in(SingleFloat())
    val operand2 = in(SingleFloat())
    val operation = in(FpOperation())
  }

  val opStage1 = RegNext(io.operation) init(FpOperation.None)
  val opStage2 = RegNext(opStage1) init(FpOperation.None)
  val opStage3 = RegNext(opStage2) init(FpOperation.None)

  val addPipeline = new FpAddPipeline
  val mulPipeline = new FpMulPipeline
  val reciprocal = new FpReciprocalEstimate

  val recipResult = RegNext(RegNext(reciprocal.io.result))

  addPipeline.io.operand1 := io.operand1
  addPipeline.io.operand2 := io.operand2
  addPipeline.io.subtract := io.operation === FpOperation.Sub
  mulPipeline.io.operand1 := io.operand1
  mulPipeline.io.operand2 := io.operand2
  reciprocal.io.operand := io.operand1

  switch (opStage3) {
    is (FpOperation.Mul) { io.result := mulPipeline.io.result }
    is (FpOperation.Recip) { io.result := recipResult }
    default { io.result := addPipeline.io.result }
  }
}

class FpAddPipeline extends Component {
  val io = new Bundle {
    val result = out(SingleFloat())
    val operand1 = in(SingleFloat())
    val operand2 = in(SingleFloat())
    val subtract = in(Bool())
  }

  //
  // Stage 1
  // - Determine which operand has the larger absolute value and swap
  //   conditionally into proper lanes
  // - Compute alignment shift count
  //
  val stage1 = new Area {
    val op1IsLarger = io.operand1.absLargerThan(io.operand2)
    val exponentDiff = Mux(op1IsLarger,
      io.operand1.exponent - io.operand2.exponent,
      io.operand2.exponent - io.operand1.exponent)

    val alignShift = UInt(5 bits)
    when (exponentDiff <= 24) {
      alignShift := exponentDiff(4 downto 0)
    } otherwise {
      alignShift := 24
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
    val largerFaction = RegNext(largerFractionNext) init(0)
    val smallerFractionAligned = RegNext(smallerFractionAlignedNext) init(0)
  }

  //
  // Stage 2
  // - Shift smaller fraction to align with larger, then add/subtract them
  //
  val stage2 = new Area {
    val sumResult = RegNext(Mux(stage1.logicalSubtract,
      stage1.largerFaction.resize(25) - stage1.smallerFractionAligned.resize(25),
      stage1.largerFaction.resize(25) + stage1.smallerFractionAligned.resize(25))) init(0)
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
    val width = stage2.sumResult.getWidth
    val isZeroResult = stage2.sumResult === 0
    val normalizeShift = UInt(log2Up(width) bits)
    normalizeShift := width
    for (i <- (width - 1) downto 0) {
      when (stage2.sumResult(width - 1 - i)) {
        normalizeShift := i
      }
    }

    val normalizedSum = (stage2.sumResult << normalizeShift)(23 downto 1)

    val resultFraction = UInt(23 bits)
    val resultExponent = UInt(8 bits)
    when (stage2.isInf) {
      resultFraction := 0
      resultExponent :=  U(0xff, 8 bits)
    } elsewhen (stage2.isNaN) {
      resultFraction := 0x400000
      resultExponent := U(0xff, 8 bits)
    } elsewhen (isZeroResult) {
      resultFraction := 0
      resultExponent := 0
    } otherwise {
      resultFraction := normalizedSum
      resultExponent := stage2.exponent + 1 - normalizeShift
    }

    val resultNegative = stage2.resultNegative && !isZeroResult

    // Use SingleFloat apply here...
    io.result.raw := RegNext(resultNegative ## resultExponent.asBits ## resultFraction.asBits) init(0)
  }
}

class FpMulPipeline extends Component {
  val io = new Bundle {
    val result = out(SingleFloat())
    val operand1 = in(SingleFloat())
    val operand2 = in(SingleFloat())
  }

  // Stage 1
  // Add exponents, multiply fractions
  val stage1 = new Area {
    val isNanNext = (io.operand1.isNaN || io.operand2.isNaN
      || (io.operand1.isInf && io.operand2.isZero)
      || (io.operand1.isZero && io.operand2.isInf))

    val mulExpSum = io.operand1.exponent.resize(10) + io.operand2.exponent.resize(10) - U(127, 10 bits)
    val mulExponentUnderflow = mulExpSum(9)
    val mulExponentCarry = mulExpSum(8)
    val mulExponentNext = mulExpSum(7 downto 0)

    val isInfNext = (io.operand1.isInf || io.operand2.isInf
      || (mulExponentCarry && !mulExponentUnderflow))
    val isZeroNext = io.operand1.isZero || io.operand2.isZero || mulExponentUnderflow

    // note: we do the multiplication in one stage here.
    val fractionProductNext = (io.operand1.fullFraction * io.operand2.fullFraction)(47 downto 23)

    val isZero = RegNext(isZeroNext) init(False)
    val isNaN = RegNext(isNanNext) init(False)
    val isInf = RegNext(isInfNext) init(False)
    val isNegative = RegNext(io.operand1.negative ^ io.operand2.negative) init(False)
    val mulExponent = RegNext(mulExponentNext) init(0)
    val fractionProduct = RegNext(fractionProductNext) init(0)
  }

  val stage2 = new Area {
    // One position shift to normalize if the product has overflown
    val normShift = stage1.fractionProduct(24)
    val normalizedFraction = Mux(normShift,
      stage1.fractionProduct(23 downto 1),
      stage1.fractionProduct(22 downto 0))
    val adjustedExponent = Mux(normShift, stage1.mulExponent + 1, stage1.mulExponent)

    val resultNext = Bits(32 bits)
    when (stage1.isNaN) {
      resultNext := False ## B(0xff, 8 bits) ## B(0x400000, 23 bits)
    } elsewhen (stage1.isInf) {
      resultNext := stage1.isNegative ## B(0xff, 8 bits) ## B(0, 23 bits)
    } elsewhen (stage1.isZero) {
      resultNext := stage1.isNegative ## B(0, 31 bits)
    } otherwise {
      resultNext := stage1.isNegative ## adjustedExponent ## normalizedFraction
    }

    val result = RegNext(resultNext) init(0)
  }

  // Stage 3
  io.result.raw := RegNext(stage2.result) init(0)
}

class FpReciprocalEstimate extends Component {
  val io = new Bundle {
    val result = out(SingleFloat())
    val operand = in(SingleFloat())
  }

  // Generate fraction lookup table.
  // Because the floating point significant is normalized, its value ranges
  // from (1.0, 2.0]. The reciprocal of this range therefore is (0.5, 1.0].
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

  // Invert the exponent. Note we subtract 1-2 extra bits here to renormalize
  // the value.
  val normalizationCorrection = io.operand.fraction(22 downto 17) === 0
  val exponentNext = 253 - io.operand.exponent + U(normalizationCorrection)

  val resultNext = Bits(32 bits)
  when (io.operand.isZero || io.operand.isNaN) {
    // Division by zero or NaN = NaN
    resultNext := False ## U(0xff, 8 bits) ## B(0x400000, 23 bits)
  } elsewhen (io.operand.isInf) {
    // Division by +/- inf = 0.0
    resultNext := io.operand.negative ## U(0, 8 bits) ## U(0, 23 bits)
  } otherwise {
    resultNext := io.operand.negative ## exponentNext ## B(fractionNext << 17, 23 bits)
  }

  io.result.raw := RegNext(resultNext) init(0)
}


class FloatingPointSpec extends AnyFunSuite {
  // Helper for reciprocal tests
  def fpTruncate(value: Float): Float =
    java.lang.Float.intBitsToFloat(java.lang.Float.floatToIntBits(value) & 0xfffe0000)

  test("fp pipeline") {
    TestConfig.testSim.compile(new FloatingPointPipeline()).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      dut.io.operand1.raw #= 0
      dut.io.operand2.raw #= 0
      dut.clockDomain.waitSampling() // Wait for reset to complete

      val pipelineDelay = 4
      val testVectors: Array[(FpOperation.E, Float, Float, Float)] = Array(
        // Reciprocal Estimate (second operand is ignored)
        (FpOperation.Recip, 1.0f, 0.0f, 1.0f),
        (FpOperation.Recip, 2.0f, 0.0f, 0.5f),
        (FpOperation.Recip, 0.5f, 0.0f, 2.0f),
        (FpOperation.Recip, 4.0f, 0.0f, 0.25f),
        (FpOperation.Recip, 3.0f, 0.0f, fpTruncate(0.333333f)),
        (FpOperation.Recip, 1.5f, 0.0f, fpTruncate(0.666666f)),
        (FpOperation.Recip, 0.333333f, 0.0f, fpTruncate(3.0f)),
        (FpOperation.Recip, 1000.0f, 0.0f, fpTruncate(0.001f)),
        (FpOperation.Recip, 0.99999f, 0.0f, 1.0f), // Last table entry
        (FpOperation.Recip, 0.0f, 0.0f, Float.NaN), // Division by zero
        (FpOperation.Recip, Float.NegativeInfinity, 0.0f, -0.0f), // Division by inf
        (FpOperation.Recip, Float.PositiveInfinity, 0.0f, 0.0f), // Division by inf

        // Addition
        (FpOperation.Add, 3.0f, 2.0f, 5.0f), // pos + pos
        (FpOperation.Add, -4.0f, -5.0f, -9.0f), // neg + neg
        (FpOperation.Add, 7.7f, -3.5f, 4.2f), // pos + smaller neg
        (FpOperation.Add, 7.0f, -13.0f, -6.0f), // pos + larger neg
        (FpOperation.Add, -40.0f, 37.0f, -3.0f), // neg + smaller pos
        (FpOperation.Add, -27.0f, 35.0f, 8.0f), // neg + larger pos
        (FpOperation.Add, 5.0f, -5.0f, 0.0f), // Exact cancellation
        (FpOperation.Add, -5.0f, 5.0f, 0.0f),
        (FpOperation.Add, 17.79f, 19.32f, 37.11f), // Exponents equal. Will carry into next significand bit
        (FpOperation.Add, 0.34f, 44.23f, 44.57f), // Exponent 2 larger
        (FpOperation.Add, 44.23f, 0.034f, 44.264f), // Exponent 1 larger
        (FpOperation.Add, -1.0f, 5.0f, 4.0f), // First element is negative and has smaller exponent
        (FpOperation.Add, -5.0f, 1.0f, -4.0f), // First element is negative and has larger exponent
        (FpOperation.Add, 5.0f, -1.0f, 4.0f), // Second element is negative and has smaller exponent
        (FpOperation.Add, 1.0f, -5.0f, -4.0f), // Second element is negative and has larger exponent
        (FpOperation.Add, 5.0f, 0.0f, 5.0f), // Zero identity
        (FpOperation.Add, 0.0f, 5.0f, 5.0f), // " "
        (FpOperation.Add, 0.0f, 0.0f, 0.0f), // " "
        (FpOperation.Add, 7.0f, -7.0f, 0.0f), // Sum is zero, positive first operand
        (FpOperation.Add, -7.0f, 7.0f, 0.0f), // Sum is zero, negative first operand
        (FpOperation.Add, 1000000.0f, 0.0000001f, 1000000.0f), //  Second op is lost because of precision
        (FpOperation.Add, 0.0000001f, 0.00000001f, 0.00000011f), // Very small number
        (FpOperation.Add, 1000000.0f, 10000000.0f, 11000000.0f), // Large number
        (FpOperation.Add, -0.0f, 2.323f, 2.323f), // negative zero
        (FpOperation.Add, 2.323f, -0.0f, 2.323f), // negative zero
        (FpOperation.Add, Float.PositiveInfinity, Float.PositiveInfinity, Float.PositiveInfinity), // Infinity and NaN cases...
        (FpOperation.Add, Float.PositiveInfinity, 1.0f, Float.PositiveInfinity),
        (FpOperation.Add, Float.NegativeInfinity, 1.0f, Float.NegativeInfinity),
        (FpOperation.Add, 0.0f, Float.NegativeInfinity, Float.NegativeInfinity),
        (FpOperation.Add, 1.0f, Float.PositiveInfinity, Float.PositiveInfinity),
        (FpOperation.Add, 1.0f, Float.NegativeInfinity, Float.NegativeInfinity),
        (FpOperation.Add, Float.PositiveInfinity, Float.NegativeInfinity, Float.NaN),
        (FpOperation.Add, Float.NaN, 1.0f, Float.NaN),
        (FpOperation.Add, 1.0f, Float.NaN, Float.NaN),
        (FpOperation.Add, Float.NaN, Float.NaN, Float.NaN),

        // Subtraction
        (FpOperation.Sub, 3.0f, 2.0f, 1.0f),
        (FpOperation.Sub, -3.0f, -2.0f, -1.0f),
        (FpOperation.Sub, 3.0f, -2.0f, 5.0f),
        (FpOperation.Sub, -3.0f, 2.0f, -5.0f),
        (FpOperation.Sub, 2.0f, -3.0f, 5.0f),
        (FpOperation.Sub, -2.0f, 3.0f, -5.0f),

        // Multiplication
        (FpOperation.Mul, 100.0f, 25.0f, 2500.0f), // positive * positive
        (FpOperation.Mul, -10.0f, 32.0f, -320.0f), // negative * positive
        (FpOperation.Mul, 0.5f, -90.0f, -45.0f), // positive * negative
        (FpOperation.Mul, -15.0f, -4.0f, 60.0f), // negative * negative
        (FpOperation.Mul, -15.0f, 0.0f, -0.0f), // zero identity, negative
        (FpOperation.Mul, 0.0f, 15.0f, 0.0f), // zero identity, positive
        (FpOperation.Mul, 0.00001f, 12345.0f, 0.12345f),
        (FpOperation.Mul, 200000.5f, 123.0f, 24600061.5f),
        (FpOperation.Mul, 1.0E25f, 1.0E25f, Float.PositiveInfinity), // Overflow
        (FpOperation.Mul, 1.0E-20f, 1.0E-20f, 0.0f), // Underflow
        (FpOperation.Mul, Float.PositiveInfinity, Float.PositiveInfinity, Float.PositiveInfinity), // Infinity and NaN cases...
        (FpOperation.Mul, Float.PositiveInfinity, 1.0f, Float.PositiveInfinity),
        (FpOperation.Mul, Float.NegativeInfinity, 1.0f, Float.NegativeInfinity),
        (FpOperation.Mul, Float.NegativeInfinity, -1.0f, Float.PositiveInfinity),
        (FpOperation.Mul, Float.PositiveInfinity, -1.0f, Float.NegativeInfinity),
        (FpOperation.Mul, 0.0f, Float.NegativeInfinity, Float.NaN),
        (FpOperation.Mul, 0.0f, Float.PositiveInfinity, Float.NaN),
        (FpOperation.Mul, Float.NegativeInfinity, 0.0f, Float.NaN),
        (FpOperation.Mul, Float.PositiveInfinity, 0.0f, Float.NaN),
        (FpOperation.Mul, Float.PositiveInfinity, Float.NegativeInfinity, Float.NegativeInfinity),
        (FpOperation.Mul, Float.NaN, 1.0f, Float.NaN),
        (FpOperation.Mul, 1.0f, Float.NaN, Float.NaN),
        (FpOperation.Mul, Float.NaN, Float.NaN, Float.NaN),
        (FpOperation.Mul, Float.NaN, Float.PositiveInfinity, Float.NaN),
        (FpOperation.Mul, Float.PositiveInfinity, Float.NaN, Float.NaN),
        (FpOperation.Mul, Float.NegativeInfinity, Float.NaN, Float.NaN),
        (FpOperation.Mul, Float.NaN, Float.NegativeInfinity, Float.NaN),
      )

      for (cycle <- 0 until testVectors.length + pipelineDelay) {
        if (cycle < testVectors.length) {
          val testValues = testVectors(cycle)

          dut.io.operation #= testValues._1

          val bits1: Long = java.lang.Float.floatToIntBits(testValues._2.toFloat).toLong & 0xFFFFFFFFL
          dut.io.operand1.raw #= bits1

          val bits2: Long = java.lang.Float.floatToIntBits(testValues._3.toFloat).toLong & 0xFFFFFFFFL
          dut.io.operand2.raw #= bits2
        }

        if (cycle >= pipelineDelay) {
          val expectedBits: Long = java.lang.Float.floatToIntBits(testVectors(cycle - pipelineDelay)._4.toFloat).toLong & 0xFFFFFFFFL
          val actualBits: Long = dut.io.result.raw.toLong & 0xFFFFFFFFL

          // We allow an error of 1ulp because we're truncating rounding so the host machine may
          // represent our expected values differently.
          if (math.abs(expectedBits - actualBits) > 1) {
            val testValues = testVectors(cycle - pipelineDelay)
            val a = testValues._2
            val b = testValues._3
            val expectedFloat = testValues._4
            val actualFloat = java.lang.Float.intBitsToFloat(actualBits.toInt)

            println(f"mismatch at test entry ${cycle - pipelineDelay}: $a%.3f $b%.3f")
            println(f"  expected = $expectedFloat%.6f  (0x$expectedBits%08x)")
            println(f"  actual   = $actualFloat%.6f  (0x$actualBits%08x)")
            simFailure()
          }
        }

        dut.clockDomain.waitSampling()
      }
    }
  }
}