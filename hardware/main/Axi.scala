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

/** AXI 4 high speed bus interface. The GPU is a manager that reads and writes
  * system memory. This is used to transfer commands, texture data, and
  * rendered pixels.
  *
  * @see AMBA AXI Protocol Specification, Issue L
  * [[https://developer.arm.com/documentation/ihi0022/latest]]
  */
class Axi4IO(implicit cfg: GpuConfig) extends Bundle {
  // Write request channel (B1.1.1)
  val writeRequest = Irrevocable(new Bundle {
    val address = UInt(cfg.busAddressBits.W) // AWADDR Address to write to
    val length = UInt(cfg.busBurstLengthBits.W) // AWLEN Number of data transfers in burst
  })

  // Write data channel (B1.1.2)
  val writeData = Irrevocable(new Bundle {
    val data = UInt(cfg.busDataBits.W) // WDATA Data to write
    val last = Bool() // WLAST True if this is the last transfer in burst
  })

  // Write response channel (B1.1.3)
  val writeResponse = Flipped(Irrevocable(new Bundle {}))

  // Read request channel (B1.2.1)
  val readRequest = Irrevocable(new Bundle {
    val address = UInt(cfg.busAddressBits.W) // ARADDR Address to read from
    val length = UInt(cfg.busBurstLengthBits.W) // ARLEN Number of data transfers in burst
  })

  // Read data channel (B1.2.2)
  val readData = Flipped(Irrevocable(new Bundle {
    val data = UInt(cfg.busDataBits.W) // RDATA Data read
  }))
}

/** The GPU is a subordinate on this bus, which allows the host to read and
  * write memory mapped control registers.
  */
class Axi4LiteIO extends Bundle {
  val writeRequest = Irrevocable(new Bundle {
    val address = UInt(32.W) // AWADDR (e.g., 12 bits)
  })

  val writeData = Irrevocable(new Bundle {
    val data = UInt(32.W) // WDATA (32-bit CSRs)
  })

  val writeResponse = Flipped(Irrevocable(new Bundle {})) // BVALID

  val readRequest = Irrevocable(new Bundle {
    val address = UInt(32.W) // ARADDR
  })

  val readData = Flipped(Irrevocable(new Bundle {
    val data = UInt(32.W) // RDATA
  }))
}

