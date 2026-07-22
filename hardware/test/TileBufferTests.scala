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
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.util.Random

case class ColorRef(r: Int, g: Int, b: Int, a: Int) {
  def scale(component: Int, scaleFactor: Int): Int = {
    val product = component * scaleFactor
    (((product + (product >> Color.channelBits))
      + (1 << (Color.channelBits - 1))) >> Color.channelBits)
  }

  def blend(incoming: ColorRef): ColorRef = {
    val maxAlpha = (1 << Color.channelBits) - 1
    val oneMinusAlpha = maxAlpha - incoming.a

    val newR = (incoming.r + this.scale(this.r, oneMinusAlpha)).min(maxAlpha)
    val newG = (incoming.g + this.scale(this.g, oneMinusAlpha)).min(maxAlpha)
    val newB = (incoming.b + this.scale(this.b, oneMinusAlpha)).min(maxAlpha)
    val newA = (incoming.a + this.scale(this.a, oneMinusAlpha)).min(maxAlpha)

    ColorRef(newR, newG, newB, newA)
  }

  private def channel8Bit(ch: Int): Int = {
    val rounded = (ch + 2) >> 2
    rounded.min(255)
  }

  // Pack downsampled 8-bit channels into 32-bit ARGB
  def toPackedArgb32: Int = {
    val a8 = channel8Bit(a)
    val r8 = channel8Bit(r)
    val g8 = channel8Bit(g)
    val b8 = channel8Bit(b)

    (a8 << 24) | (r8 << 16) | (g8 << 8) | b8
  }
}

/** For testing, this tracks ground truth for what should be in the buffer.
  * This works in RGBA8888 space, what is expected to be flushed from the buffer
  * although the internal color space is different.
  */
class TileBufferReference {
  val colors = Array.fill(GpuConfig.tileSizePixels * GpuConfig.tileSizePixels)(ColorRef(0, 0, 0, 0))
  val depths = Array.fill(GpuConfig.tileSizePixels * GpuConfig.tileSizePixels)(0xffffff)

  private def rasterIndex(quadX: Int, quadY: Int, pixel: Int): Int = {
    val offset = quadY * GpuConfig.tileSizePixels + quadX
    pixel match {
      case 0 => offset
      case 1 => offset + 1
      case 2 => offset + GpuConfig.tileSizePixels
      case 3 => offset + 1 + GpuConfig.tileSizePixels
    }
  }

  def writeQuad(quadX: Int, quadY: Int, mask: Int,
                newColors: Seq[ColorRef], newDepths: Seq[Int]) = {
    for (pixel <- 0 until 4) {
      if ((mask & (1 << pixel)) != 0) {
        val idx = rasterIndex(quadX, quadY, pixel)
        if (newDepths(pixel) < depths(idx)) {
          colors(idx) = colors(idx).blend(newColors(pixel))
          depths(idx) = newDepths(pixel)
        }
      }
    }
  }

  def checkBuffer(select: Int, results: Seq[Int]) = {
    for (y <- 0 until GpuConfig.tileSizePixels) {
      for (x <- 0 until GpuConfig.tileSizePixels) {
        val idx = y * GpuConfig.tileSizePixels + x
        val expected = if (select == 0) colors(idx).toPackedArgb32 else depths(idx)
        assert(results(idx) == expected,
          s"Mismatch at ($x, $y): got ${Integer.toHexString(results(idx))}, expected ${Integer.toHexString(expected)}")
      }
    }
  }
}

class TileBufferTests extends AnyFunSuite with ChiselSim with ColorTestHelpers {
  // Push a quad through the DUT pipeline.
  def writeQuad(dut: TileBuffer, x: Int, y: Int, mask: Int, colors: Seq[ColorRef],
    depths: Seq[Int]) = {

    dut.io.quadLoc.x.poke(x)
    dut.io.quadLoc.y.poke(y)
    for (pixel <- 0 until 4) {
      dut.io.colors(pixel).poke(r = colors(pixel).r, g = colors(pixel).g, b = colors(pixel).b, a = colors(pixel).a)
      dut.io.depths(pixel).poke(depths(pixel))
    }

    dut.io.mask.poke(mask)
    dut.io.valid.poke(true)
    dut.clock.step()
    dut.io.valid.poke(false)
    dut.io.mask.poke(0)
  }

  def flush(dut: TileBuffer, select: RenderBufferId.Type): Seq[Int] = {
    val results = ArrayBuffer[Int]()
    dut.io.startFlush.poke(true)
    dut.io.flushBufferSel.poke(select)
    dut.clock.step()
    dut.io.startFlush.poke(false)
    dut.io.flushData.ready.poke(false)
    dut.io.flushData.valid.expect(0)

    val rng = new Random(42)
    val totalPixels = GpuConfig.tileSizePixels * GpuConfig.tileSizePixels
    while (results.length < totalPixels) {
      // Add random delays
      dut.io.flushData.ready.poke(rng.nextBoolean())
      dut.clock.step()
      if (dut.io.flushData.valid.peek().litValue.toLong != 0 &&
        dut.io.flushData.ready.peek().litValue.toLong != 0) {
        results += dut.io.flushData.bits.peek().litValue.toInt
      }
    }

    for (_ <- 0 until 5) {
      dut.clock.step()
      dut.io.flushData.valid.expect(0)
    }

    results.toSeq
  }

  // For test setup, manually force framebuffer to a known state
  def clearBuffers(dut: TileBuffer) = {
    dut.io.clearColor.channels(0).poke(0)
    dut.io.clearColor.channels(1).poke(0)
    dut.io.clearColor.channels(2).poke(0)
    dut.io.clearColor.channels(3).poke(0)
    this.flush(dut, RenderBufferId.Color)

    dut.io.clearDepth.poke(0xffffff)
    this.flush(dut, RenderBufferId.Depth)
  }

  test("TileBuffer alpha blend") {
   simulate(new TileBuffer()) { dut =>
      dut.io.valid.poke(false)
      dut.io.startFlush.poke(false)
      dut.io.enableBlend.poke(true)
      dut.io.enableDepthCheck.poke(true)
      dut.io.enableDepthWrite.poke(true)
      dut.io.mask.poke(0)

      this.clearBuffers(dut)

      writeQuad(dut, 0, 0, 0xf,
        Seq(ColorRef(0x3ff, 0, 0x1ff, 0x3ff), ColorRef(0x3ff, 0, 0x1ff, 0x3ff),
        ColorRef(0x3ff, 0, 0x1ff, 0x3ff), ColorRef(0x3ff, 0, 0x1ff, 0x3ff)),
        Seq(2, 2, 2, 2))

      // Flush pipeline
      dut.clock.step(4)

      // Blend
      writeQuad(dut, 0, 0, 0xf,
        Seq(ColorRef(0, 0, 0, 0), ColorRef(0x1ff, 0x3ff, 0x3ff, 0x3ff),
        ColorRef(0x2ac, 0x334, 0x39c, 0x3ff), ColorRef(0x1ff, 0x1ff, 0x1ff, 0x1ff)),
        Seq(1, 1, 1, 1))

      // Flush pipeline
      dut.clock.step(4)

      val color = this.flush(dut, RenderBufferId.Color)
      assert(color(0) == 0xffff0080) // Alpha was zero, no change
      assert(color(1) == 0xff80ffff) // Midway
      assert(color(GpuConfig.tileSizePixels) == 0xffabcde7) // Alpha is 1.0, take new value
      assert(color(GpuConfig.tileSizePixels + 1) == 0xffff80c0) // Blend
    }
  }

  test("TileBuffer depth check disable") {
    simulate(new TileBuffer()) { dut =>
      this.clearBuffers(dut)
      dut.io.valid.poke(false)
      dut.io.startFlush.poke(false)
      dut.io.enableBlend.poke(true)
      dut.io.enableDepthCheck.poke(false)
      dut.io.enableDepthWrite.poke(true)
      dut.clock.step() // Ensure we are out of reset.

      writeQuad(dut, 0, 0, 0xf,
        Seq(ColorRef(0, 0x1ff, 0x3ff, 0x3ff), ColorRef(0x3ff, 0x3ff, 0x3ff, 0x3ff),
        ColorRef(0x2ac, 0x334, 0x39c, 0x3ff), ColorRef(0, 0x1ff, 0x3ff, 0x3ff)),
        Seq(2, 2, 2, 2))

      // Flush pixels
      dut.clock.step(4)

      // Depth is greater. If checking was enabled, this will not write, but it's
      // not so they do.
      writeQuad(dut, 0, 0, 0xf,
        Seq(ColorRef(0, 0, 0, 0), ColorRef(0x3ff, 0x3ff, 0x3ff, 0x3ff), ColorRef(0x156, 0x19a, 0x1ce, 0x3ff), ColorRef(0x1ff, 0x1ff, 0x1ff, 0x1ff)),
        Seq(3, 3, 3, 3))

      // Flush pipeline
      dut.clock.step(4)

      val depth = this.flush(dut, RenderBufferId.Depth)
      assert(depth(0) == 3)
      assert(depth(1) == 3)
      assert(depth(GpuConfig.tileSizePixels) == 3)
      assert(depth(GpuConfig.tileSizePixels + 1) == 3)

      // The last pixel written should have won.
      val color = this.flush(dut, RenderBufferId.Color)
      assert(color(0) == 0xff0080ff)
      assert(color(1) == 0xffffffff)
      assert(color(GpuConfig.tileSizePixels) == 0xff566774)
      assert(color(GpuConfig.tileSizePixels + 1) == 0xff80c0ff)
    }
  }

  test("TileBuffer depth write disable") {
    simulate(new TileBuffer()) { dut =>
      this.clearBuffers(dut)

      dut.io.valid.poke(false)
      dut.io.startFlush.poke(false)
      dut.io.enableBlend.poke(true)
      dut.io.enableDepthCheck.poke(true)
      dut.io.enableDepthWrite.poke(false)
      dut.clock.step() // Ensure we are out of reset.

      writeQuad(dut, 0, 0, 0xf,
        Seq(ColorRef(0x3ff, 0, 0x154, 0x3ff), ColorRef(0x3ff, 0, 0x176, 0x3ff), ColorRef(0x3ff, 0, 0x198, 0x3ff), ColorRef(0x3ff, 0, 0x1ba, 0x3ff)),
        Seq(2, 2, 2, 2))

      // Flush pipeline
      dut.clock.step(4)

      // Depth is not written
      val depth = this.flush(dut, RenderBufferId.Depth)
      assert(depth(0) == 0xffffff)
      assert(depth(1) == 0xffffff)
      assert(depth(GpuConfig.tileSizePixels) == 0xffffff)
      assert(depth(GpuConfig.tileSizePixels + 1) == 0xffffff)

      // Color is written
      val color = this.flush(dut, RenderBufferId.Color)
      assert(color(0) == 0xffff0055)
      assert(color(1) == 0xffff005e)
      assert(color(GpuConfig.tileSizePixels) == 0xffff0066)
      assert(color(GpuConfig.tileSizePixels + 1) == 0xffff006f)
    }
  }

  test("TileBuffer alpha blend disable") {
    simulate(new TileBuffer()) { dut =>
      this.clearBuffers(dut)
      dut.io.valid.poke(false)
      dut.io.startFlush.poke(false)
      dut.io.enableBlend.poke(false)
      dut.io.enableDepthCheck.poke(true)
      dut.io.enableDepthWrite.poke(true)
      dut.clock.step() // Ensure we are out of reset.

      writeQuad(dut, 0, 0, 0xf,
        Seq(ColorRef(0x3ff, 0, 0x154, 0x3ff), ColorRef(0x3ff, 0, 0x176, 0x3ff), ColorRef(0x3ff, 0, 0x198, 0x3ff), ColorRef(0x3ff, 0, 0x1ba, 0x3ff)),
        Seq(2, 2, 2, 2))

      // Flush pipeline
      dut.clock.step(4)

      // All pixels shoudl be written full intensity
      val color = this.flush(dut, RenderBufferId.Color)
      assert(color(0) == 0xffff0055)
      assert(color(1) == 0xffff005e)
      assert(color(GpuConfig.tileSizePixels) == 0xffff0066)
      assert(color(GpuConfig.tileSizePixels + 1) == 0xffff006f)
    }
  }

  test("TileBuffer random tile write") {
    simulate(new TileBuffer()) { dut =>
      val reference = new TileBufferReference

      this.clearBuffers(dut)

      dut.io.valid.poke(false)
      dut.io.enableBlend.poke(true)
      dut.io.enableDepthCheck.poke(true)
      dut.io.enableDepthWrite.poke(true)
      dut.io.startFlush.poke(false)
      dut.clock.step() // Ensure we are out of reset.

      // Perform some random writes (the sequence is fixed because we use a known seed)
      val rng = new Random(42)
      val coordHistory = Queue[(Int, Int)]()
      for (_ <- 0 until 2000) {
        var quadX = 0
        var quadY = 0

        // We can't generate the same quad within 3 cycles, so ensure that doesn't happen.
        var regenerateCoord = true
        while (regenerateCoord) {
          quadX = rng.nextInt(GpuConfig.tileSizePixels) & ~1
          quadY = rng.nextInt(GpuConfig.tileSizePixels) & ~1
          regenerateCoord = coordHistory.contains((quadX, quadY))
        }

        coordHistory.enqueue((quadX, quadY))
        if (coordHistory.size > 2) {
          coordHistory.dequeue()
        }

        val mask = rng.nextInt(0xf)
        val colors = Seq.fill(4) { ColorRef(rng.nextInt(0x3ff), rng.nextInt(0x3ff), rng.nextInt(0x3ff), rng.nextInt(0x3ff)) }
        val depths = Seq.fill(4) { rng.nextInt(0xffffff) }

        writeQuad(dut, quadX, quadY, mask, colors, depths)
        reference.writeQuad(quadX, quadY, mask, colors, depths)
      }

      // Flush pipeline
      dut.clock.step(4)

      val color = this.flush(dut, RenderBufferId.Color)
      reference.checkBuffer(0, color)
      val depth = this.flush(dut, RenderBufferId.Depth)
      reference.checkBuffer(1, depth)
    }
  }
}

