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

class TileWriterTests extends AnyFunSuite with ChiselSim with TestHelpers {
  implicit val cfg: GpuConfig = GpuConfig(tileSizePixels = 4)

  def writeControl(dut: TileWriter, address: UInt, data: Long): Unit = {
    dut.io.controlBus.regWrite.bits.address.poke(address)
    dut.io.controlBus.regWrite.bits.data.poke(data)
    dut.io.controlBus.regWrite.valid.poke(true.B)
    dut.clock.step()
    dut.io.controlBus.regWrite.valid.poke(false.B)
  }

  def pokeColor(dut: TileWriter, argb: Int): Unit = {
    def expand(channel: Int): Int = (channel << 2) | (channel >> 6)

    dut.io.flushData.bits.color.red.poke(expand((argb >> 16) & 0xff))
    dut.io.flushData.bits.color.green.poke(expand((argb >> 8) & 0xff))
    dut.io.flushData.bits.color.blue.poke(expand(argb & 0xff))
    dut.io.flushData.bits.color.alpha.poke(expand((argb >>> 24) & 0xff))
    dut.io.flushData.bits.depth.poke(0.0f)
  }

  test("TileWriter flush") {
    simulate(new TileWriter) { dut =>
      dut.io.controlBus.regWrite.valid.poke(false.B)
      dut.io.flushData.valid.poke(false.B)
      dut.io.writePort.data.ready.poke(true.B)

      writeControl(dut, ControlRegisterIndex.TileWriteAddressL, 0x100)
      writeControl(dut, ControlRegisterIndex.TileWriteAddressH, 0x2)
      writeControl(dut, ControlRegisterIndex.TileWriteStride, 24)
      writeControl(dut, ControlRegisterIndex.TileWriteStart, RenderBufferId.Color.litValue.toLong)

      for (row <- 0 until cfg.tileSizePixels) {
        dut.io.writePort.burst.valid.expect(true.B)
        dut.io.writePort.burst.bits.address.expect(0x200000100L + row * 24)
        dut.io.writePort.burst.bits.length.expect(1.U)
        dut.clock.step()

        for (word <- 0 until cfg.tileSizePixels / 2) {
          val lowPixel = 0xff000000 | (row * 4 + word * 2)
          val highPixel = 0xff000000 | (row * 4 + word * 2 + 1)
          pokeColor(dut, lowPixel)
          dut.io.flushData.valid.poke(true.B)
          while (!dut.io.flushData.ready.peek().litToBoolean) {
            dut.clock.step()
          }
          dut.clock.step()

          pokeColor(dut, highPixel)
          while (!dut.io.flushData.ready.peek().litToBoolean) {
            dut.clock.step()
          }

          dut.io.writePort.data.valid.expect(true.B)
          val expected = (BigInt(highPixel & 0xffffffffL) << 32) |
            BigInt(lowPixel & 0xffffffffL)
          dut.io.writePort.data.bits.expect(expected)
          dut.clock.step()
        }
      }

      dut.io.writePort.burst.valid.expect(false.B)
      dut.io.flushData.ready.expect(false.B)
      dut.io.done.expect(true.B)
    }
  }

  test("TileWriter clear") {
    simulate(new TileWriter) { dut =>
      dut.io.controlBus.regWrite.valid.poke(false.B)
      dut.io.flushData.valid.poke(false.B)
      dut.io.writePort.data.ready.poke(true.B)

      writeControl(dut, ControlRegisterIndex.TileClearStart, RenderBufferId.Depth.litValue.toLong)
      dut.io.writePort.burst.valid.expect(false.B)
      dut.io.flushData.valid.poke(true.B)
      pokeColor(dut, 0)
      for (_ <- 0 until cfg.totalTilePixels) {
        while (!dut.io.flushData.ready.peek().litToBoolean) {
          dut.clock.step()
        }

        dut.clock.step()
        dut.io.writePort.data.valid.expect(false.B)
      }

      dut.io.flushData.ready.expect(false.B)
      dut.io.done.expect(true.B)
    }
  }
}
