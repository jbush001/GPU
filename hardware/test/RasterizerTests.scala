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

import scala.util.Random
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funsuite.AnyFunSuite

class RasterizerTests extends AnyFunSuite with ChiselSim {
  def rasterizeTriangle(dut: Rasterizer,
    x0: Int,
    y0: Int,
    x1: Int,
    y1: Int,
    x2: Int,
    y2: Int,
    bbLeft: Int,
    bbTop: Int,
    bbRight: Int,
    bbBottom: Int,
    randomizeReady: Boolean) : String = {

    dut.io.input.valid.poke(false)
    dut.io.output.ready.poke(false)
    dut.clock.step() // Wait for reset to complete

    dut.io.output.ready.poke(true)
    dut.clock.step()

    dut.io.input.bits.boundingBox.left.poke(bbLeft)
    dut.io.input.bits.boundingBox.top.poke(bbTop)
    dut.io.input.bits.boundingBox.right.poke(bbRight)
    dut.io.input.bits.boundingBox.bottom.poke(bbBottom)

    // Edge 0->1
    dut.io.input.bits.xStep(0).poke(y1 - y0)
    dut.io.input.bits.yStep(0).poke(x0 - x1)
    dut.io.input.bits.initialValue(0).poke((bbLeft - x0) * (y1 - y0) - (bbTop - y0) * (x1 - x0))

    // Edge 1->2
    dut.io.input.bits.xStep(1).poke(y2 - y1)
    dut.io.input.bits.yStep(1).poke(x1 - x2)
    dut.io.input.bits.initialValue(1).poke((bbLeft - x1) * (y2 - y1) - (bbTop - y1) * (x2 - x1))

    // Edge 2->0
    dut.io.input.bits.xStep(2).poke(y0 - y2)
    dut.io.input.bits.yStep(2).poke(x2 - x0)
    dut.io.input.bits.initialValue(2).poke((bbLeft - x2) * (y0 - y2) - (bbTop - y2) * (x0 - x2))
    dut.io.input.valid.poke(true)

    while (dut.io.input.ready.peek().litValue.toLong == 0) {
      dut.clock.step()
    }

    dut.clock.step()
    dut.io.input.valid.poke(false)

    val outputBuffer = Array.ofDim[Boolean](bbRight - bbLeft + 2, bbBottom - bbTop + 2);

    val rng = new Random(42)
    dut.io.output.ready.poke(true)
    for (_ <- 0 until 2048) {
      if (randomizeReady) {
        dut.io.output.ready.poke(rng.nextBoolean())
      }

      if (dut.io.output.valid.peek().litValue.toLong != 0
        && dut.io.output.ready.peek().litValue.toLong != 0) {
        dut.io.input.ready.expect(0)
        val x = dut.io.output.bits.location.x.peek().litValue.toInt
        val y = dut.io.output.bits.location.y.peek().litValue.toInt
        assert(x <= (bbRight - bbLeft))
        assert(y <= (bbBottom - bbTop))
        val mask = dut.io.output.bits.mask.peek().litValue.toLong
        if ((mask & 1) != 0) outputBuffer(y)(x) = true
        if ((mask & 2) != 0) outputBuffer(y)(x + 1) = true
        if ((mask & 4) != 0) outputBuffer(y + 1)(x) = true
        if ((mask & 8) != 0) outputBuffer(y + 1)(x + 1) = true
      }

      dut.clock.step()
    }

    dut.io.input.ready.expect(1)
    val sb = new StringBuilder()
    for (y <- 0 until outputBuffer.length) {
      for (x <- 0 until outputBuffer(0).length) {
        sb.append(if (outputBuffer(y)(x)) "X" else ".")
      }
      sb.append("\n") // Newline at the end of each row
    }

    sb.toString()
  }

  test("rasterize") {
    simulate(new Rasterizer()) { dut =>
      val output = rasterizeTriangle(dut, 8, 1, 15, 15, 1, 15, 0, 0, 14, 14, false);
      val expected = """
................
................
........X.......
........X.......
.......XXX......
.......XXX......
......XXXXX.....
......XXXXX.....
.....XXXXXXX....
.....XXXXXXX....
....XXXXXXXXX...
....XXXXXXXXX...
...XXXXXXXXXXX..
...XXXXXXXXXXX..
..XXXXXXXXXXXXX.
................"""

      assert(output.filterNot(_.isWhitespace) == expected.filterNot(_.isWhitespace))
    }
  }

  test("clip right") {
    simulate(new Rasterizer()) { dut =>
      val output = rasterizeTriangle(dut, 8, 1, 15, 15, 1, 15, 0, 0, 6, 6, false);
      val expected = """
........
........
........
........
.......X
.......X
......XX
......XX"""
      assert(output.trim == expected.stripMargin.trim)
    }
  }

  // Test non-zero bounding box offset
  test("clip left") {
    simulate(new Rasterizer()) { dut =>
      val output = rasterizeTriangle(dut, 16, 1, 22, 22, 1, 16, 16, 16, 22, 22, false);
      val expected =  """
XXXXX...
XXXXX...
XXXXX...
XXXXXX..
XXXXXX..
...XXX..
........
........"""

      assert(output.filterNot(_.isWhitespace) == expected.filterNot(_.isWhitespace))
    }
  }

  // Won't display anything
  test("reverse winding") {
    simulate(new Rasterizer()) { dut =>
      val output = rasterizeTriangle(dut, 8, 1, 1, 15, 15, 15, 0, 0, 14, 14, false);
      val expected =  """................
................
................
................
................
................
................
................
................
................
................
................
................
................
................
................"""
      assert(output.trim == expected.stripMargin.trim)
    }
  }

  test("output flow control") {
    simulate(new Rasterizer()) { dut =>
      val output = rasterizeTriangle(dut, 8, 1, 15, 15, 1, 15, 0, 0, 14, 14, true);
      val expected = """................
................
........X.......
........X.......
.......XXX......
.......XXX......
......XXXXX.....
......XXXXX.....
.....XXXXXXX....
.....XXXXXXX....
....XXXXXXXXX...
....XXXXXXXXX...
...XXXXXXXXXXX..
...XXXXXXXXXXX..
..XXXXXXXXXXXXX.
................"""

      assert(output.trim == expected.stripMargin.trim)
    }
  }
}
