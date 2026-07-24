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
import chisel3.util.  _

/** Native color representation for all internal data paths.
  */
class Color extends Bundle {
  val channels = Vec(Color.numChannels, UInt(Color.channelBits.W))
  def red: UInt = channels(0)
  def green: UInt = channels(1)
  def blue: UInt = channels(2)
  def alpha: UInt = channels(3)

  /** Multiplies all color components by a fixed point scale factor.
    *
    * We need to normalize after multiplication. The highest scale factor is
    * 2^n - 1, so we can't simply truncate the lower bits, as that would
    * introduce bias. Instead, we can approximate this division by:
    * result = (product + (product >> W)) + (1 << (W - 1))) >> W
    */
  def scale(factor: UInt): Color = {
    val result = Wire(new Color)
    for (ch <- 0 until Color.numChannels) {
      val product = this.channels(ch) * factor
      val norm = (((product +% (product >> Color.channelBits))
        +% (1.U << (Color.channelBits - 1))) >> Color.channelBits)

      result.channels(ch) := norm
    }

    result
  }

  /** Adds each component of the given color to this color, clamping the
    * results to the maximum value (2^N - 1).
    */
  def +|(that: Color): Color = {
    val result = Wire(new Color)
    for (ch <- 0 until Color.numChannels) {
      val sum = this.channels(ch) +& that.channels(ch)
      result.channels(ch) := Mux(sum(Color.channelBits),
        ~0.U(Color.channelBits.W),
        sum(Color.channelBits - 1, 0))
    }

    result
  }

  /** Truncate an internal channel to 8-bits with appropriate rounding.
    */
  def channel8Bit(chidx: Int): UInt = {
    val shift = Color.channelBits - 8
    val roundConst = (1 << (shift - 1)).U

    val rounded = (this.channels(chidx) +& roundConst) >> shift
    val clamped = Mux(rounded > 255.U, 255.U, rounded)

    clamped(7, 0)
  }

  def toPackedArgb32: UInt = {
    Cat(channel8Bit(3), channel8Bit(0), channel8Bit(1), channel8Bit(2))
  }

  // Todo add toPackedSrgb that does gamma correction
}

object Color {
  final val numChannels = 4;
  final val channelBits = 10;
  final val maxChannelValue = ((1 << channelBits) - 1)

  def apply() = new Color

  def fromArgb32(bits: Bits): Color = {
    val result = Wire(new Color)

    // We replicate the high bits into the low bits to
    // ensure 0 = 0 and 0xff = 0x3ff
    // https://computergraphics.stackexchange.com/questions/8173/why-replicating-the-higher-bits-of-rgb565-when-converting-to-rgba8888
    result.alpha := Cat(bits(31, 24), bits(31, 30))
    result.red := Cat(bits(23, 16), bits(23, 22))
    result.green := Cat(bits(15, 8), bits(15, 14))
    result.blue := Cat(bits(7, 0), bits(7, 6))

    result
  }
}
