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

class FixedReciprocalTests extends AnyFunSuite with ChiselSim {
  implicit val cfg: GpuConfig = GpuConfig()

  test("FixedReciprocal") {
    simulate(new FixedReciprocal) { dut =>
      val divisors = Seq[Long](
        1L, 2L, 3L, 17L, 64L, 255L, 1024L, 65535L, 1000000L,
        (1L << 24) - 1, 1L << 24, (1L << 32) - 1, (1L << 39) - 1
      )
      val numerator = (BigInt(1) << 48) - (BigInt(1) << 24)

      dut.io.divisor.valid.poke(false.B)
      for (d <- divisors) {
        dut.io.divisor.bits.poke(d.U)
        dut.io.divisor.valid.poke(true.B)
        dut.clock.step()
        dut.io.divisor.valid.poke(false.B)
        dut.clock.step(FixedReciprocal.latency - 1)
        val actual = dut.io.result.peek().litValue
        val expected = numerator / d
        val maxError = (expected >> 23) + 2
        assert(
          (actual - expected).abs <= maxError,
          s"reciprocal mismatch for $d: expected $expected, got $actual"
        )
      }
    }
  }
}
