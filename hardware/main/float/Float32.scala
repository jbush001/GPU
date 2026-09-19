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

  def isNegative = raw(31)
  def exponent = raw(30, 23).asUInt
  def fraction = raw(22, 0).asUInt

  // This adds the leading hidden bit
  def fullFraction = (!this.isZero ## this.fraction).asUInt

  def isNaN = (this.exponent === 0xff.U && this.fraction =/= 0.U)
  def isInf = (this.exponent === 0xff.U && this.fraction === 0.U)

  // Note: this treats subnormal numbers as zero.
  def isZero = this.exponent === 0.U

  def abs = Float32(false.B, this.exponent, this.fraction)
  def neg = Float32(!this.isNegative, this.exponent, this.fraction)

  def absGreaterThan(that: Float32): Bool = {
    this.abs.raw > that.abs.raw
  }

  private def toOrderable: UInt = {
    Cat(!this.isNegative, this.raw(30, 0) ^ Fill(31, this.isNegative))
  }

  def >(that: Float32): Bool = {
    // IEEE754 dictates +0.0 should be equal to -0.0, we need to check
    // explicitly here because the comparison below does not handle that.
    // Also, if either value is NaN, all comparisons should return false.
    val result = Wire(Bool())
    when ((this.isZero && that.isZero) || this.isNaN || that.isNaN) {
      result := false.B
    }.otherwise {
      result := this.toOrderable > that.toOrderable
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
      result := Mux(this.isNegative, Int.MinValue.S, Int.MaxValue.S)
    }.elsewhen (unbiasedExponent < -fractionalBits.S) {
      // Underflow -> 0
      result := 0.S
    }.otherwise {
      val paddedFraction = this.fullFraction ## 0.U(40.W)
      val shiftAmount = (63.S - fractionalBits.S - unbiasedExponent).asUInt
      val shifted = paddedFraction >> shiftAmount

      val magnitude = shifted(31, 0).asSInt
      result := Mux(this.isNegative, -magnitude, magnitude)
    }

    result
  }

  /**
    * Convert to unsigned normalized fixed-point representation
    * This differs from the fixed point conversion in that 1.0 is represented
    * by (2^N - 1). This formally is floor(M * (2^N - 1)), but we can save
    * hardware by approximating it using ((M << N) - M) >> N.
    */
  def toUnorm(width: Int): UInt = {
    require(width >= 0 && width <= 30,
      "width must be in range [0, 30]")

    val result = Wire(UInt(width.W))
    when (this.isNegative || this.exponent < Float32.exponentBias - Float32.fractionWidth.U) {
      // This also handles zero
      result := 0.U
    }.elsewhen (this.exponent >= Float32.exponentBias) {
      // This also handles 1.0 and Infinity (NaN also gets pulled in here)
      result := ~0.U(width.W)
    }.otherwise {
      val scaledProduct = (this.fullFraction << 24) - this.fullFraction
      val denormShift = Float32.exponentBias - this.exponent
      val shifted = scaledProduct >> denormShift
      result := shifted(47, 47 - width)
    }

    result
  }

  def reciprocalEstimate(): Float32 = {
    val fraction = ReciprocalLut(this.fraction(22, 17))
    val exponent = 253.U - this.exponent

    val result = Wire(Float32())
    when (this.isZero) {
      result := Float32(this.isNegative, 0xff.U, 0.U) // Division by zero = +inf
    }.elsewhen (this.isNaN) {
      result := Float32.NaN // Division by NaN = NaN
    }.elsewhen (this.isInf) {
      result := Float32(this.isNegative, 0.U, 0.U) // Division by +/-inf = +/-0.0
    }.otherwise {
      result := Float32(this.isNegative, exponent, (fraction << 17))
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

// Lookup table for an initial reciprocal fraction estimate.
//
// The normalized significand lies in [1.0, 2.0), so its reciprocal lies in
// (0.5, 1.0]. Table entries use a 1.6 fixed-point format with an implied
// leading one, so each reciprocal is multiplied by 2 to fit this format
// and the exponent must be adjusted to compensate.
//
// The exception is a significand of exactly 1.0 (fraction bits all zero),
// whose reciprocal is also 1.0. This is already normalized, so it needs no
// exponent adjustment. Rather than special-case it, we hardcode that table
// entry to 0xff, which introduces 1 part in 256 of error at that entry,
// (but this is only an estimate anyway).
object ReciprocalLut {
  val entryWidth = 6
  val numEntries = 1 << entryWidth

  val numerator = numEntries * numEntries * 2
  val romValues: Seq[Int] =
    ((1 << entryWidth) - 1) +: // special case, as described above
    (1 until numEntries).map(i =>
      ((numerator / (numEntries + i)) & (numEntries - 1)))

  def apply(index: UInt): UInt = {
    require(index.getWidth == entryWidth)
    VecInit(romValues.map(_.U(entryWidth.W)))(index)
  }
}
