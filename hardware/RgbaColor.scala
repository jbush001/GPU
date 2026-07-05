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
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite

class RgbaColor extends Bundle {
  val channels = Vec(UInt(RgbaColor.channelBits bits), RgbaColor.numChannels)
  def alpha: UInt = channels(3)

  // Multiply
  def scale(factor: UInt): RgbaColor = {
    val res = RgbaColor()
    for (ch <- 0 until RgbaColor.numChannels) {
      val prod = this.channels(ch) * factor

      // The maximum alpha value is 255. If we simply truncated the
      // product, we'd be dividing by *256*. The following maths
      // cheaply approximate division by 255.
      // TODO: handle rounding properly.
      val sum = (prod + 1 + (prod >> RgbaColor.channelBits)) >> RgbaColor.channelBits
      res.channels(ch) := sum(RgbaColor.channelBits - 1 downto 0)
    }

    res
  }

  // Saturated add
  def +|(that: RgbaColor): RgbaColor = {
    val res = RgbaColor()
    for (ch <- 0 until RgbaColor.numChannels) {
      res.channels(ch) := this.channels(ch) +| that.channels(ch)
    }

    res
  }

  // TODO this implicity assumes ARGB format. Make this take a format parameter
  // and swizzle.
  def toPackedBits: Bits = {
    this.channels.asBits
  }
}

object RgbaColor {
  val numChannels = 4;
  val channelBits = 8;

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

  // Same as above
  def fromBits(bits: Bits): RgbaColor = {
    val res = RgbaColor()
    res.channels := Vec(bits.asBools.grouped(channelBits).map(g => Vec(g).asBits.asUInt).toSeq)
    res
  }
}

class RgbaColorTests extends AnyFunSuite {
  test("RgbaColor.scale") {
    TestConfig.testSim.compile(new Component {
      val rawBits = in(Bits(32 bits))
      val scaleFactor = in(UInt(8 bits))
      val color = RgbaColor.fromBits(rawBits)
      val scaled = out(color.scale(scaleFactor).toPackedBits)
    }).doSim(dut => {
      dut.clockDomain.forkStimulus(period = 10)
      dut.rawBits #= 0
      dut.clockDomain.waitSampling() // wait for reset

      // 0 should completely clear
      dut.rawBits #= BigInt("abcdef12", 16)
      dut.scaleFactor #= 0
      dut.clockDomain.waitSampling()
      assert(dut.scaled.toLong == 0L)

      // Likewise 255 should not change it.
      dut.rawBits #= BigInt("abcdef12", 16)
      dut.scaleFactor #= 255
      dut.clockDomain.waitSampling()
      assert(dut.scaled.toLong == 0xabcdef12L)

      // Half way
      dut.rawBits #= BigInt("abcdef12", 16)
      dut.scaleFactor #= 128
      dut.clockDomain.waitSampling()
      assert(dut.scaled.toLong == 0x55667709L)
    })
  }

  test("RgbaColor.satAdd") {
    TestConfig.testSim.compile(new Component {
      val rawBits1 = in(Bits(32 bits))
      val color1 = RgbaColor.fromBits(rawBits1)
      val rawBits2 = in(Bits(32 bits))
      val color2 = RgbaColor.fromBits(rawBits2)

      val result = out((color1 +| color2).toPackedBits)
    }).doSim(dut => {
      dut.clockDomain.forkStimulus(period = 10)
      dut.rawBits1 #= 0
      dut.rawBits2 #= 0
      dut.clockDomain.waitSampling() // wait for reset
      dut.rawBits1 #= BigInt("12345678", 16)
      dut.rawBits2 #= BigInt("aaaaaaaa", 16)
      dut.clockDomain.waitSampling()
      assert(dut.result.toLong == 0xbcdeffffL)
    })
  }
}
