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
import chisel3.simulator.scalatest.ChiselSim // or your test framework trait
import chisel3.experimental.SourceInfo

/** This mixin allows tests to directly poke or expect a Color bundle. */
trait ColorTestHelpers { this: ChiselSim =>
  implicit class ColorTestOps(c: Color) {
    def poke(r: Int, g: Int, b: Int, a: Int): Unit = {
      c.red.poke(r.U(Color.channelBits.W))
      c.green.poke(g.U(Color.channelBits.W))
      c.blue.poke(b.U(Color.channelBits.W))
      c.alpha.poke(a.U(Color.channelBits.W))
    }

    def expect(r: Int, g: Int, b: Int, a: Int)(implicit sourceInfo: SourceInfo): Unit = {
      c.red.expect(r.U(Color.channelBits.W))
      c.green.expect(g.U(Color.channelBits.W))
      c.blue.expect(b.U(Color.channelBits.W))
      c.alpha.expect(a.U(Color.channelBits.W))
    }
  }
}
