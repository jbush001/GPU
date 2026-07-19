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


class RgbaColorTests extends AnyFunSuite with ChiselSim {
  test("RgbaColor.scale") {
    simulate(new Module {
      var io = IO(new Bundle {
        val rawBits = Input(Bits(32.W))
        val scaleFactor = Input(UInt(8.W))
        val scaled = Output(Bits(32.W))
      })

      val color = RgbaColor.fromBits(io.rawBits)
      io.scaled := color.scale(io.scaleFactor).toPackedBits
    }) { dut =>
      dut.io.rawBits.poke(0)
      dut.clock.step() // wait for reset

      // 0 should completely clear
      dut.io.rawBits.poke(BigInt("abcdef12", 16))
      dut.io.scaleFactor.poke(0)
      dut.clock.step()
      dut.io.scaled.expect(0.U)

      // Likewise 255 should not change it.
      dut.io.rawBits.poke(BigInt("abcdef12", 16))
      dut.io.scaleFactor.poke(255)
      dut.clock.step()
      dut.io.scaled.expect(0xabcdef12L.U)

      // Half way
      dut.io.rawBits.poke(BigInt("abcdef12", 16))
      dut.io.scaleFactor.poke(128)
      dut.clock.step()
      dut.io.scaled.expect(0x55667709L.U)
    }
  }

  test("RgbaColor.satAdd") {
    simulate(new Module {
      var io = IO(new Bundle {
        val rawBits1 = Input(Bits(32.W))
        val rawBits2 = Input(Bits(32.W))
        val result = Output(Bits(32.W))
      })
      val color1 = RgbaColor.fromBits(io.rawBits1)
      val color2 = RgbaColor.fromBits(io.rawBits2)
      io.result := (color1 +| color2).toPackedBits
    }) { dut =>
      dut.io.rawBits1.poke(0)
      dut.io.rawBits2.poke(0)
      dut.clock.step() // wait for reset
      dut.io.rawBits1.poke(BigInt("12345678", 16))
      dut.io.rawBits2.poke(BigInt("aaaaaaaa", 16))
      dut.clock.step()
      dut.io.result.expect(0xbcdeffffL.U)
    }
  }
}

