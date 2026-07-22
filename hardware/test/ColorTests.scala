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


class ColorTests extends AnyFunSuite with ChiselSim with ColorTestHelpers {
  test("Color scale") {
    simulate(new Module {
      var io = IO(new Bundle {
        val inColor = Input(new Color)
        val scaleFactor = Input(UInt(10.W))
        val scaled = Output(new Color)
      })

      io.scaled := io.inColor.scale(io.scaleFactor)
    }) { dut =>
      // 0 should completely clear
      dut.io.inColor.poke(r = 1023, g = 512, b = 256, a = 1023)
      dut.io.scaleFactor.poke(0.U)
      dut.clock.step()
      dut.io.scaled.expect(r = 0, g = 0, b = 0, a = 0)

      // 255 should not change the value
      dut.io.inColor.poke(r = 1023, g = 512, b = 256, a = 1023)
      dut.io.scaleFactor.poke(1023.U)
      dut.clock.step()
      dut.io.scaled.expect(r = 1023, g = 512, b = 256, a = 1023)

      // Half way
      dut.io.inColor.poke(r = 1023, g = 512, b = 256, a = 1023)
      dut.io.scaleFactor.poke(512.U)
      dut.clock.step()
      dut.io.scaled.expect(r = 512, g = 256, b = 128, a = 512)
    }
  }

  test("Color satAdd") {
    simulate(new Module {
      var io = IO(new Bundle {
        val inColor1 = Input(new Color)
        val inColor2 = Input(new Color)
        val result = Output(new Color)
      })
      io.result := io.inColor1 +| io.inColor2
    }) { dut =>
      dut.io.inColor1.poke(r = 0, g = 256, b = 512, a = 1023)
      dut.io.inColor2.poke(r = 256, g = 512, b = 1023, a = 1023)
      dut.clock.step()
      dut.io.result.expect(r = 256, g = 768, b = 1023, a = 1023)
    }
  }

  test("Color toPackedArgb32") {
    simulate(new Module {
      var io = IO(new Bundle {
        val inColor = Input(new Color)
        val result = Output(UInt(32.W))
      })
      io.result := io.inColor.toPackedArgb32
    }) { dut =>
      dut.io.inColor.poke(r = 0, g = 256, b = 512, a = 1023)
      dut.clock.step()
      dut.io.result.expect(0xff004080L)
    }
  }

  test("Color fromArgb32"){
    simulate(new Module {
      var io = IO(new Bundle {
        val inColor = Input(UInt(32.W))
        val result = Output(new Color)
      })
      io.result := Color.fromArgb32(io.inColor)
    }) { dut =>
      dut.io.inColor.poke(0xff004080L)
      dut.clock.step()
      dut.io.result.expect(r = 0, g = 257, b = 514, a = 1023)
    }
  }

}

