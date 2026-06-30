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

import spinal.core._

class RgbaColor extends Bundle {
  val numChannels = 4;
  val channelBits = 8;

  val channels = Vec(UInt(channelBits bits), numChannels)
  def alpha: UInt = channels(3)

  // Multiply
  def scale(factor: UInt): RgbaColor = {
    val res = RgbaColor()
    for (ch <- 0 until this.numChannels) {
      val prod = this.channels(ch) * factor

      // Need to divide by 255 for this to be scaled properly.
      val sum = prod + 1 + (prod >> channelBits)
      res.channels(ch) := sum((channelBits * 2 - 1) downto channelBits)
    }

    res
  }

  // Saturated add
  def +|(that: RgbaColor): RgbaColor = {
    val res = RgbaColor()
    for (ch <- 0 until this.numChannels) {
      res.channels(ch) := this.channels(ch) +| that.channels(ch)
    }

    res
  }

  def toPackedBits: Bits = {
    this.channels.asBits
  }

  def fromBits(bits: Bits): RgbaColor = {
    val res = RgbaColor()
    res.channels := Vec(bits.asBools.grouped(channelBits).map(g => Vec(g).asBits.asUInt).toSeq)
    res
  }
}

object RgbaColor {
  def apply(r: Int, g: Int, b: Int, a: Int = 255): RgbaColor = {
    val c = new RgbaColor()
    c.channels(0) := b
    c.channels(1) := g
    c.channels(2) := r
    c.channels(3) := a
    c
  }

  def apply(): RgbaColor = {
    new RgbaColor()
  }
}
