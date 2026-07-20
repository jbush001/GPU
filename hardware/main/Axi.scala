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
import chisel3.util._

object AxiConsts {
    val addressBits = 32
    val dataBits = 32
    val burstLengthBits = 8
}

/** AMBA AXI bus interface.
  * @see ARM IHI 0022, Issue L
  * [[https://developer.arm.com/documentation/ihi0022/latest]]
  */
class AxiBus extends Bundle {
  // Write request channel (B1.1.1)
  val writeRequest = Decoupled(new Bundle {
    val address = UInt(AxiConsts.addressBits.W) // AWADDR Address to write to
    val length = UInt(AxiConsts.burstLengthBits.W) // AWLEN Number of data transfers in burst
  })

  // Write data channel (B1.1.2)
  val writeData = Decoupled(new Bundle {
    val data = UInt(AxiConsts.dataBits.W) // WDATA Data to write
    val last = Bool() // WLAST True if this is the last transfer in burst
  })

  // Write response channel (B1.1.3)
  val writeResponse = Flipped(Decoupled(new Bundle {}))

  // Read request channel (B1.2.1)
  val readRequest = Decoupled(new Bundle {
    val address = UInt(AxiConsts.addressBits.W) // ARADDR Address to read from
    val length = UInt(AxiConsts.burstLengthBits.W) // ARLEN Number of data transfers in burst
  })

  // Read data channel (B1.2.2)
  val readData = Flipped(Decoupled(new Bundle {
    val data = UInt(AxiConsts.dataBits.W) // RDATA Data read
  }))
}
