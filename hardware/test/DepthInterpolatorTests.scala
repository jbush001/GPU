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

class DepthInterpolatorTests extends AnyFunSuite with ChiselSim {
  implicit val cfg: GpuConfig = GpuConfig()

  val oneRawBits = java.lang.Float.floatToRawIntBits(1.0f)

  test("DepthInterpolator basic operation") {
    simulate(new DepthInterpolator) { dut =>
      dut.io.interpolatedQuad.ready.poke(true)
      dut.io.rasterizedQuad.valid.poke(true)
      dut.io.rasterizedQuad.bits.location.x.poke(3)
      dut.io.rasterizedQuad.bits.location.y.poke(4)
      dut.io.rasterizedQuad.bits.mask.poke(0xb)
      dut.io.rasterizedQuad.bits.primitiveId.poke(2)
      for (i <- 0 until Consts.pixelsPerQuad) {
        for (j <- 0 until 2) {
          dut.io.rasterizedQuad.bits.lambda(i)(j).raw.poke(i * 2 + j)
        }
      }
      dut.io.rasterizedQuad.ready.expect(true)
      dut.clock.step()
      dut.io.rasterizedQuad.valid.poke(false)

      dut.io.interpolatedQuad.valid.expect(true)
      dut.io.interpolatedQuad.bits.quad.location.x.expect(3)
      dut.io.interpolatedQuad.bits.quad.location.y.expect(4)
      dut.io.interpolatedQuad.bits.quad.mask.expect(0xb)
      dut.io.interpolatedQuad.bits.quad.primitiveId.expect(2)
      for (i <- 0 until Consts.pixelsPerQuad) {
        for (j <- 0 until 2) {
          dut.io.interpolatedQuad.bits.quad.lambda(i)(j).raw.expect(i * 2 + j)
        }
      }
      for (i <- 0 until Consts.pixelsPerQuad) {
        dut.io.interpolatedQuad.bits.depths(i).raw.expect(oneRawBits)
      }
    }
  }

  test("DepthInterpolator handshaking") {
    simulate(new DepthInterpolator) { dut =>
      dut.io.interpolatedQuad.ready.poke(false)
      dut.io.rasterizedQuad.valid.poke(true)
      dut.io.rasterizedQuad.bits.location.x.poke(1)
      dut.io.rasterizedQuad.bits.location.y.poke(1)
      dut.io.rasterizedQuad.bits.mask.poke(0xf)
      dut.io.rasterizedQuad.bits.primitiveId.poke(0)
      for (i <- 0 until Consts.pixelsPerQuad) {
        for (j <- 0 until 2) {
          dut.io.rasterizedQuad.bits.lambda(i)(j).raw.poke(0)
        }
      }
      dut.io.rasterizedQuad.ready.expect(true)
      dut.clock.step()

      // Upstream must stall.
      dut.io.rasterizedQuad.ready.expect(false)
      dut.io.interpolatedQuad.valid.expect(true)
      dut.io.rasterizedQuad.valid.poke(false)
      dut.clock.step()

      // Downstream is ready, buffered quad drains and upstream reopens.
      dut.io.interpolatedQuad.ready.poke(true)
      dut.io.interpolatedQuad.valid.expect(true)
      dut.io.interpolatedQuad.bits.quad.location.x.expect(1)
      dut.clock.step()
      dut.io.rasterizedQuad.ready.expect(true)
      dut.io.interpolatedQuad.valid.expect(false)
    }
  }
}
