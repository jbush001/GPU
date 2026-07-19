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

class RgbaColor extends Bundle {
  val channels = Vec(RgbaColor.numChannels, UInt(RgbaColor.channelBits.W))
  def alpha: UInt = channels(3)


  /** Multiplies all color components by a fixed point scale factor.
    *
    * The factor is treated as a fixed-point value 0-255, so the result
    * is re-normalized by dividing by 255.
    */
  def scale(factor: UInt): RgbaColor = {
    val res = Wire(new RgbaColor)
    for (ch <- 0 until RgbaColor.numChannels) {
      val prod = this.channels(ch) * factor

      // The maximum alpha value is 255. If we simply truncated the
      // product, we'd be dividing by *256*. The following maths
      // cheaply approximate division by 255.
      // TODO: handle rounding properly.
      val sum = (prod + 1.U + (prod >> RgbaColor.channelBits)) >> RgbaColor.channelBits
      res.channels(ch) := sum(RgbaColor.channelBits - 1, 0)
    }

    res
  }

  /** Adds each component of the given color to this color, clamping the
    * results to 255.
    */
  def +|(that: RgbaColor): RgbaColor = {
    val res = Wire(new RgbaColor)
    for (ch <- 0 until RgbaColor.numChannels) {
      val sumExt = this.channels(ch) +& that.channels(ch)
      res.channels(ch) := Mux(sumExt(8), 255.U, sumExt(7, 0))
    }

    res
  }

  // TODO this implicity assumes ARGB format. Make this take a format parameter
  // and swizzle.
  def toPackedBits: Bits = {
    this.channels.asUInt
  }
}

object RgbaColor {
  val numChannels = 4;
  val channelBits = 8;

  def apply() = new RgbaColor

  def fromBits(bits: Bits): RgbaColor = {
    val result = Wire(new RgbaColor) // Use 'new' to fix the bundle tracking bug

    for (ch <- 0 until numChannels) {
      // Slice out the 8-bit chunk directly from the hardware bits vector
      val lowBit  = ch * channelBits
      val highBit = lowBit + channelBits - 1
      result.channels(ch) := bits(highBit, lowBit).asUInt
    }

    result
  }
}
