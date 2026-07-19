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

import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funsuite.AnyFunSuite

class TriangleSetupTests extends AnyFunSuite with ChiselSim {
  def computeExpectedValues(bbLeft: Int, bbTop: Int,
                            x0: Int, y0: Int, x1: Int, y1: Int, x2: Int, y2: Int) = {
    val xStep0 = y1 - y0
    val yStep0 = x0 - x1
    val initialValue0 = (bbLeft - x0) * (y1 - y0) - (bbTop - y0) * (x1 - x0)

    val xStep1 = y2 - y1
    val yStep1 = x1 - x2
    val initialValue1 = (bbLeft - x1) * (y2 - y1) - (bbTop - y1) * (x2 - x1)

    val xStep2 = y0 - y2
    val yStep2 = x2 - x0
    val initialValue2 = (bbLeft - x2) * (y0 - y2) - (bbTop - y2) * (x0 - x2)

    (xStep0, yStep0, initialValue0,
      xStep1, yStep1, initialValue1,
      xStep2, yStep2, initialValue2)
  }

  // TODO test input/output handshaking, queuing up multiple requests.

  test("triangle setup") {
   simulate(new TriangleSetup()) { dut =>
      dut.io.input.valid.poke(false)
      dut.io.vpi.readData.poke(0)
      dut.clock.step() // wait for reset

      val bbLeft = 32
      val bbTop = 96
      val bbRight = 64
      val bbBottom = 128
      val x0 = 47
      val y0 = 88
      val x1 = 59
      val y1 = 110
      val x2 = 33
      val y2 = 107

      val vpData = Array(
        x0,
        y0,
        x1,
        y1,
        x2,
        y2
      )

      dut.io.input.valid.poke(true)
      dut.io.input.bits.left.poke(bbLeft)
      dut.io.input.bits.top.poke(bbTop)
      dut.io.input.bits.right.poke(bbRight)
      dut.io.input.bits.bottom.poke(bbBottom)

      while (dut.io.input.ready.peek().litValue.toLong == 0) {
        dut.clock.step()
      }

      dut.clock.step()

      // Clear the inputs to ensure it has latched them properly.
      dut.io.input.valid.poke(false)
      dut.io.input.bits.left.poke(0)
      dut.io.input.bits.top.poke(0)
      dut.io.input.bits.right.poke(0)
      dut.io.input.bits.bottom.poke(0)

      while (dut.io.output.valid.peek().litValue.toLong == 0) {
        if (dut.io.vpi.readEn.peek().litValue.toInt != 0) {
          val readData = vpData(dut.io.vpi.readAddress.peek().litValue.toInt)
          dut.clock.step()
          dut.io.vpi.readData.poke(readData)
        } else {
          dut.clock.step()
        }
      }

      val (xStep0, yStep0, initialValue0,
        xStep1, yStep1, initialValue1,
        xStep2, yStep2, initialValue2) = computeExpectedValues(bbLeft, bbTop,
          x0, y0, x1, y1, x2, y2)
      dut.io.output.bits.xStep(0).expect(xStep0)
      dut.io.output.bits.yStep(0).expect(yStep0)
      dut.io.output.bits.initialValue(0).expect(initialValue0)
      dut.io.output.bits.xStep(1).expect(xStep1)
      dut.io.output.bits.yStep(1).expect(yStep1)
      dut.io.output.bits.initialValue(1).expect(initialValue1)
      dut.io.output.bits.xStep(2).expect(xStep2)
      dut.io.output.bits.yStep(2).expect(yStep2)
      dut.io.output.bits.initialValue(2).expect(initialValue2)
      dut.io.output.bits.boundingBox.left.expect(bbLeft)
      dut.io.output.bits.boundingBox.top.expect(bbTop)
      dut.io.output.bits.boundingBox.right.expect(bbRight)
      dut.io.output.bits.boundingBox.bottom.expect(bbBottom)
    }
  }
}
