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

/**
  * Represents a single precision floating point value, in IEEE754 binary32
  * format.
  * [[https://en.wikipedia.org/wiki/Single-precision_floating-point_format]]
  * @note Subnormal numbers are not supported and are treated as zeroes.
  * @note All arithmetic operations round towards zero.
  */
class Float32 extends Bundle {
  val raw = Bits(32.W)

  def negative = raw(31)
  def exponent = raw(30, 23).asUInt
  def fraction = raw(22, 0).asUInt

  // This adds the leading hidden bit
  def fullFraction = (!this.isZero ## this.fraction).asUInt

  def isNaN = (this.exponent === 0xff.U && this.fraction =/= 0.U)
  def isInf = (this.exponent === 0xff.U && this.fraction === 0.U)

  // Note: this treats subnormal numbers as zero.
  def isZero = this.exponent === 0.U

  def abs = Float32(false.B, this.exponent, this.fraction)

  def absGreaterThan(that: Float32): Bool = {
    ((this.exponent > that.exponent)
      || ((this.exponent === that.exponent)
      && this.fullFraction > that.fullFraction))
  }

  def >(that: Float32): Bool = {
    val result = WireInit(false.B)
    when (this.negative === that.negative) {
      when (this.negative) {
        result := that.absGreaterThan(this)
      }.otherwise {
        result := this.absGreaterThan(that)
      }
    }.otherwise {
      result := that.negative
    }

    when (this.isNaN || that.isNaN || (this.isZero && that.isZero)) {
      result := false.B
    }

    result
  }

  def toFixedPoint(fractionalBits: Int = 0): SInt = {
    require(fractionalBits >= 0 && fractionalBits <= 30,
      "fractionalBits must be in range [0, 30]")

    val result = Wire(SInt(32.W))
    val unbiasedExponent = this.exponent.asSInt - Float32.exponentBias.asSInt

    when (this.isNaN) {
      result := 0.S
    }.elsewhen (this.isInf || unbiasedExponent > (30 - fractionalBits).S) {
      // Infinity or exponent overflow -> Saturate
      result := Mux(this.negative, Int.MinValue.S, Int.MaxValue.S)
    }.elsewhen (unbiasedExponent < -fractionalBits.S) {
      // Underflow -> 0
      result := 0.S
    }.otherwise {
      val paddedFraction = this.fullFraction ## 0.U(40.W)
      val shiftAmount = (63.S - fractionalBits.S - unbiasedExponent).asUInt
      val shifted = paddedFraction >> shiftAmount

      val magnitude = shifted(31, 0).asSInt
      result := Mux(this.negative, -magnitude, magnitude)
    }

    result
  }

  def reciprocalEstimate(): Float32 = {
    // Generate the fraction lookup table.
    // Because the floating point significand is normalized, its value ranges
    // from [1.0, 2.0). The reciprocal of this range therefore spans (0.5, 1.0].
    // We treat table entries as 1.6 fixed point numbers for the calculation,
    // so the numerator for our calculations is 64 * 64. However, 0.5 is not
    // representable as a normalized value, so we need to also multiply by
    // two (we compensate by shifting and adjusting the exponent to renormalize)
    val numEntries = 64 // Must be a power of two
    val entryWidth = log2Up(numEntries)
    val numerator = (numEntries * numEntries * 2)
    val romValues = Array.tabulate[UInt](numEntries)(i =>
      ((numerator / (numEntries + i)) & (numEntries - 1)).U(entryWidth.W))

    val reciprocalRom = VecInit(romValues.toIndexedSeq)

    // Read value out of lookup table
    val fractionNext = reciprocalRom(this.fraction(22, 17))

    // Adjust the exponent. Note we subtract 1-2 extra values out of the exponent
    // to compensate for the normalization shift that occurs below.
    // In the case of zero, there's nothing to normalize.
    val normalizationCorrection = this.fraction(22, 17) === 0.U
    val exponentNext = 253.U - this.exponent + normalizationCorrection.asUInt

    val result = Wire(Float32())
    when (this.isZero || this.isNaN) {
      // Division by zero or NaN = NaN
      result := Float32(false.B, 0xff.U, 0x400000.U)
    }.elsewhen (this.isInf) {
      // Division by +/- inf = 0.0
      result := Float32(this.negative, 0.U, 0.U)
    }.otherwise {
      result := Float32(this.negative, exponentNext, (fractionNext << 17))
    }

    result
  }
}

object Float32 {
  final val exponentWidth = 8
  final val fractionWidth = 23 // As encoded (not including hidden bit)
  final def exponentBias = 127.U(exponentWidth.W) // This is an exponent of zero

  def Zero = Float32(0.U)
  def One = Float32(false.B, exponentBias, 0.U)
  def NaN = Float32(false.B, 0xff.U, 0x400000.U)

  def apply(): Float32 = new Float32()

  def apply(raw: UInt): Float32 = {
    require(raw.getWidth == 32)
    val f = Wire(new Float32())
    f.raw := raw
    f
  }

  def apply(negative: Bool, exponent: UInt, fraction: UInt): Float32 = {
    apply(negative ## exponent.pad(exponentWidth)(exponentWidth-1, 0)
      ## fraction.pad(fractionWidth)(fractionWidth-1, 0))
  }

  /** Create a constant Float32 */
  def apply(fval: Double): Float32 = {
    val bits = java.lang.Float.floatToIntBits(fval.toFloat)
    val raw = (bits.toLong & 0xFFFFFFFFL).U(32.W)
    apply(raw)
  }

  def fromFixedPoint(value: SInt, fractionalBits: Int = 0): Float32 = {
    require(fractionalBits >= 0 && fractionalBits <= 30,
      "fractionalBits must be in range [0, 30]")
    require(value.getWidth == 32, "value must be 32 bits wide") // XXX fixme
    val result = Wire(new Float32)
    when (value === 0.S) {
      result.raw := 0.U
    }.otherwise {
      val absValue = value.abs.asUInt
      val leadingZeros = PriorityEncoder(Reverse(absValue))
      val exponent = (31.U(8.W) - leadingZeros - fractionalBits.U(8.W)) + exponentBias
      val fraction = (absValue << leadingZeros)(30, 8)
      result := Float32(value(31), exponent, fraction)
    }

    result
  }
}
