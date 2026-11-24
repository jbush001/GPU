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

class HostInterfaceTests extends AnyFunSuite with ChiselSim with TestHelpers {
  implicit val cfg: GpuConfig = GpuConfig()

  test("read/write") {
    simulate(new HostInterface) { dut =>
      //
      // Write word: Address phase one cycle before data phase
      //

      // Address
      dut.io.axiSubordinate.writeRequest.valid.poke(true)
      dut.io.axiSubordinate.writeRequest.bits.address.poke(0xfffffff0L)
      dut.io.axiSubordinate.writeData.valid.poke(false)
      dut.io.axiSubordinate.writeData.bits.data.poke(0)
      dut.io.axiSubordinate.writeRequest.ready.expect(false)
      dut.io.axiSubordinate.writeData.ready.expect(false)
      dut.io.axiSubordinate.writeResponse.ready.expect(false)
      dut.clock.step()

      // Data
      dut.io.axiSubordinate.writeData.valid.poke(true)
      dut.io.axiSubordinate.writeData.bits.data.poke(0x12345678)
      dut.io.axiSubordinate.writeRequest.ready.expect(true)
      dut.io.axiSubordinate.writeData.ready.expect(true)
      dut.io.axiSubordinate.writeResponse.ready.expect(false)
      dut.clock.step()

      // Response
      dut.io.axiSubordinate.writeData.valid.poke(false)
      dut.io.axiSubordinate.writeRequest.valid.poke(false)
      dut.io.axiSubordinate.writeResponse.ready.poke(true)
      dut.io.axiSubordinate.writeResponse.valid.expect(true)
      dut.io.axiSubordinate.writeRequest.ready.expect(false)
      dut.clock.step()

      dut.io.axiSubordinate.writeResponse.valid.expect(false)

      // Read back value
      dut.io.axiSubordinate.readRequest.valid.poke(true)
      dut.io.axiSubordinate.readRequest.bits.address.poke(0xfffffff0L)
      dut.io.axiSubordinate.readData.ready.poke(true)
      dut.clock.step()
      dut.io.axiSubordinate.readData.valid.expect(true)
      dut.io.axiSubordinate.readData.bits.data.expect(0x12345678)
      dut.io.axiSubordinate.readRequest.valid.poke(false)

      //
      // Write word:Data phase one cycle before address phase
      //

      // Data
      dut.io.axiSubordinate.writeData.valid.poke(true)
      dut.io.axiSubordinate.writeData.bits.data.poke(0x66554433)
      dut.io.axiSubordinate.writeRequest.ready.expect(false)
      dut.io.axiSubordinate.writeData.ready.expect(false)
      dut.io.axiSubordinate.writeResponse.valid.expect(false)
      dut.clock.step()

      // Address
      dut.io.axiSubordinate.writeRequest.valid.poke(true)
      dut.io.axiSubordinate.writeRequest.bits.address.poke(0xfffffff0L)
      dut.io.axiSubordinate.writeRequest.ready.expect(true)
      dut.io.axiSubordinate.writeData.ready.expect(true)
      dut.io.axiSubordinate.writeResponse.valid.expect(false)
      dut.clock.step()

      // Response
      dut.io.axiSubordinate.writeData.valid.poke(false)
      dut.io.axiSubordinate.writeRequest.valid.poke(false)
      dut.io.axiSubordinate.writeResponse.ready.poke(true)
      dut.io.axiSubordinate.writeResponse.valid.expect(true)
      dut.io.axiSubordinate.writeRequest.ready.expect(false)
      dut.clock.step()

      // Read back value
      dut.io.axiSubordinate.readRequest.valid.poke(true)
      dut.io.axiSubordinate.readRequest.bits.address.poke(0xfffffff0L)
      dut.io.axiSubordinate.readData.ready.poke(true)
      dut.clock.step()
      dut.io.axiSubordinate.readData.valid.expect(true)
      dut.io.axiSubordinate.readData.bits.data.expect(0x66554433)
      dut.io.axiSubordinate.readRequest.valid.poke(false)

      //
      // Write data: Address and data simultaneously
      //

      dut.io.axiSubordinate.writeRequest.valid.poke(true)
      dut.io.axiSubordinate.writeRequest.bits.address.poke(0xfffffff4L)
      dut.io.axiSubordinate.writeData.valid.poke(true)
      dut.io.axiSubordinate.writeData.bits.data.poke(0xdeadbeef)
      dut.io.axiSubordinate.writeRequest.ready.expect(true)
      dut.io.axiSubordinate.writeData.ready.expect(true)
      dut.io.axiSubordinate.writeResponse.valid.expect(false)
      dut.clock.step()

      // Response
      dut.io.axiSubordinate.writeData.valid.poke(false)
      dut.io.axiSubordinate.writeRequest.valid.poke(false)
      dut.io.axiSubordinate.writeResponse.ready.poke(true)
      dut.io.axiSubordinate.writeResponse.valid.expect(true)
      dut.io.axiSubordinate.writeRequest.ready.expect(false)
      dut.clock.step()

      // Read back value
      dut.io.axiSubordinate.readRequest.valid.poke(true)
      dut.io.axiSubordinate.readRequest.bits.address.poke(0xfffffff4L)
      dut.io.axiSubordinate.readData.ready.poke(true)
      dut.clock.step()
      dut.io.axiSubordinate.readData.valid.expect(true)
      dut.io.axiSubordinate.readData.bits.data.expect(0xdeadbeefL)

      // Check the scratchpad value
      dut.io.readScratchpadAddr.poke(1)
      dut.io.readScratchpadData.expect(0xdeadbeefL)

      dut.io.writeScratchpad.bits.addr.poke(0)
      dut.io.writeScratchpad.bits.data.poke(0x12344321L)
      dut.io.writeScratchpad.valid.poke(true)
      dut.clock.step()
      dut.io.writeScratchpad.valid.poke(false)

      dut.io.readScratchpadAddr.poke(0)
      dut.io.readScratchpadData.expect(0x12344321L)
    }
  }
}
