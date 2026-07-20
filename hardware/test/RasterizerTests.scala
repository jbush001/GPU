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

// TODO: does not check lambda outputs.

package gpu

import scala.util.Random
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funsuite.AnyFunSuite

class EdgeCoefficients {
  val edges = 3
  val xStep = Array.ofDim[Int](edges)
  val yStep = Array.ofDim[Int](edges)
  val initialValue = Array.ofDim[Int](edges)
}

class RasterizerTests extends AnyFunSuite with ChiselSim {
  def computeEdgeCoefficient(x0: Int, y0: Int, x1: Int, y1: Int, x2: Int, y2: Int,
  startX: Int, startY: Int): EdgeCoefficients = {
    val coeffs = new EdgeCoefficients()
    coeffs.xStep(0) = y1 - y0
    coeffs.yStep(0) = x0 - x1
    coeffs.initialValue(0) = (startX - x0) * (y1 - y0) - (startY - y0) * (x1 - x0)

    coeffs.xStep(1) = y2 - y1
    coeffs.yStep(1) = x1 - x2
    coeffs.initialValue(1) = (startX - x1) * (y2 - y1) - (startY - y1) * (x2 - x1)

    coeffs.xStep(2) = y0 - y2
    coeffs.yStep(2) = x2 - x0
    coeffs.initialValue(2) = (startX - x2) * (y0 - y2) - (startY - y2) * (x0 - x2)

    return coeffs
  }

  def rasterizeTriangle(dut: Rasterizer,
    coeffs: EdgeCoefficients,
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
    dut.io.input.bits.xStep(0).poke(coeffs.xStep(0))
    dut.io.input.bits.yStep(0).poke(coeffs.yStep(0))
    dut.io.input.bits.initialValue(0).poke(coeffs.initialValue(0))

    // Edge 1->2
    dut.io.input.bits.xStep(1).poke(coeffs.xStep(1))
    dut.io.input.bits.yStep(1).poke(coeffs.yStep(1))
    dut.io.input.bits.initialValue(1).poke(coeffs.initialValue(1))

    // Edge 2->0
    dut.io.input.bits.xStep(2).poke(coeffs.xStep(2))
    dut.io.input.bits.yStep(2).poke(coeffs.yStep(2))
    dut.io.input.bits.initialValue(2).poke(coeffs.initialValue(2))
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

  def computeReference(coeffs: EdgeCoefficients, width: Int, height: Int): String = {
    val sb = new StringBuilder()
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        val covered = (0 until 3).forall(i =>
          (coeffs.initialValue(i) +
            coeffs.xStep(i) * x +
            coeffs.yStep(i) * y
          ) < 0
        )

        sb.append(if (covered) "X" else ".")
      }

      sb.append("\n")
    }

    sb.toString()
  }

  test("Rasterizer rasterize") {
    simulate(new Rasterizer()) { dut =>
      val coeffs = computeEdgeCoefficient(8, 1, 15, 15, 1, 15, 0, 0)
      val output = rasterizeTriangle(dut, coeffs, 0, 0, 14, 14, false);
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

  // Fill entire framebuffer
  test("Rasterizer fill") {
    simulate(new Rasterizer()) { dut =>
      val coeffs = computeEdgeCoefficient(-1, -1, 40, -1, -1, 40, 0, 0)
      val output = rasterizeTriangle(dut, coeffs, 0, 0, 14, 14, false);
      assert(output.filterNot(_.isWhitespace) == "X" * 256)
    }
  }

  test("Rasterizer random") {
    simulate(new Rasterizer()) { dut =>
      val rng = new Random(42)
      val blockSize = 32
      for (_ <- 0 until 200) {
        val x0 = rng.nextInt(blockSize * 2)
        val y0 = rng.nextInt(blockSize * 2)
        val x1 = rng.nextInt(blockSize * 2)
        val y1 = rng.nextInt(blockSize * 2)

        // Ensure these are wound clockwise using the cross product.
        var x2 = 0
        var y2 = 0
        while ((x1 - x0) * (y2 - y0) - (y1 - y0) * (x2 - x0) < 0) {
          x2 = rng.nextInt(blockSize * 2)
          y2 = rng.nextInt(blockSize * 2)
        }

        val left = rng.nextInt(blockSize)
        val top = rng.nextInt(blockSize)
        val right = left + blockSize
        val bottom = top + blockSize

        val coeffs = computeEdgeCoefficient(x0, y0, x1, y1, x2, y2, left, top)
        val expected = computeReference(coeffs, right - left, bottom - top)

        val output = rasterizeTriangle(dut,
          coeffs,
          left, top, right - 2, bottom - 2, false);
        assert(output.filterNot(_.isWhitespace) == expected.filterNot(_.isWhitespace))
      }
    }
  }

  // Won't display anything
  test("Rasterizer reverse winding") {
    simulate(new Rasterizer()) { dut =>
      val coeffs = computeEdgeCoefficient(8, 1, 1, 15, 15, 15, 0, 0)
      val output = rasterizeTriangle(dut, coeffs, 0, 0, 14, 14, false);
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

  test("Rasterizer output flow control") {
    simulate(new Rasterizer()) { dut =>
      val coeffs = computeEdgeCoefficient(8, 1, 15, 15, 1, 15, 0, 0)
      val output = rasterizeTriangle(dut, coeffs, 0, 0, 14, 14, true);
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
