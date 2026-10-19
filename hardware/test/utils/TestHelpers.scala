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
import chisel3.experimental.SourceInfo

/**
  * This mixin allows tests to directly poke/peek/expect custom [[Bundle]]
  * types. The implicit classes here wrap these types and create new
  * overridden versions of these functions.
  */
trait TestHelpers { this: ChiselSim =>
  implicit class ColorOps(c: Color) {
    def poke(r: Int, g: Int, b: Int, a: Int): Unit = {
      c.red.poke(r.U(Color.channelBits.W))
      c.green.poke(g.U(Color.channelBits.W))
      c.blue.poke(b.U(Color.channelBits.W))
      c.alpha.poke(a.U(Color.channelBits.W))
    }

    // sourceInfo ensures that the error message from expect will display the correct line
    // number and file information.
    def expect(r: Int, g: Int, b: Int, a: Int)(implicit sourceInfo: SourceInfo): Unit = {
      c.red.expect(r.U(Color.channelBits.W))
      c.green.expect(g.U(Color.channelBits.W))
      c.blue.expect(b.U(Color.channelBits.W))
      c.alpha.expect(a.U(Color.channelBits.W))
    }
  }

  implicit class Float32Ops(private val f: Float32) {
    def poke(value: Float): Unit = {
      f.raw.poke((java.lang.Float.floatToRawIntBits(value).toLong & 0xffffffffL).U)
    }

    def peek(): Float = java.lang.Float.intBitsToFloat(f.raw.peek().litValue.toInt)
    def expect(value: Float, epsilon: Float = 1e-6f)(implicit sourceInfo: SourceInfo): Unit = {
      val actual = f.peek()
      chisel3.assert(
        (actual - value).abs <= epsilon,
        s"Expected $value, got $actual (line ${sourceInfo.makeMessage(x => x)})"
      )
    }
  }

  implicit class Point2DOps(c: Point2D) {
    def poke(x: Int, y: Int): Unit = {
      c.x.poke(x.S)
      c.y.poke(y.S)
    }

    def expect(x: Int, y: Int)(implicit sourceInfo: SourceInfo): Unit = {
      c.x.expect(x.S)
      c.y.expect(y.S)
    }
  }
}
