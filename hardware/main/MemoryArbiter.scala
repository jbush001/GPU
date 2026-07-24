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

/** A simple burst-oriented memory read port.
  *
  * Protocol:
  *
  *   - On the next clock edge after `valid` goes high, the `address` and
  *     `length` are latched by the arbiter. The client MAY issue the next
  *     request while a burst is active, but MUST NOT issue more than one
  *     outstanding request ahead of the active burst.
  *   - `length` is the number of transfer cycles in the burst minus one
  *     (0 = one transfer).
  *   - A data word is consumed iff both `data.valid` and `data.ready` are
  *     true; the arbiter advances to the next word only after a word is
  *     consumed.
  *   - After the arbiter has asserted `data.valid`, it MUST NOT deassert
  *     it or change the contents of `data.bits`.
  *   - A burst completes on the clock edge the last word is consumed.
  *   - `data.valid` and `data.ready` MUST NOT be combinationally dependent
  *     on each other.
  */
 class MemReadPort extends Bundle {
  val valid = Output(Bool())
  val address = Output(UInt(AxiBus.addressBits.W))
  val length = Output(UInt(AxiBus.burstLengthBits.W))
  val data = Flipped(Decoupled(UInt(AxiBus.dataBits.W)))
}

/** A simple burst-oriented memory write port. Same protocol as
  * [[MemReadPort]], except anywhere where the data is referenced
  * the roles of the client and arbiter are reversed.
  */
class MemWritePort extends Bundle {
  val valid = Output(Bool())
  val address = Output(UInt(AxiBus.addressBits.W))
  val length = Output(UInt(AxiBus.burstLengthBits.W))
  val data = Decoupled(UInt(AxiBus.dataBits.W))
}

/** Multiplexes [[numReadPorts]] read clients and [[numWritePorts]] write
  * clients onto a single memory, using the burst protocol documented on
  * [[MemReadPort]] and [[MemWritePort]].
  */
class MemoryArbiter(
  val numReadPorts: Int = 2,
  val numWritePorts: Int = 2
) extends Module {
  val io = IO(new Bundle {
    val readPorts  = Vec(numReadPorts, Flipped(new MemReadPort))
    val writePorts = Vec(numWritePorts, Flipped(new MemWritePort))
    val axiBus     = new AxiBus
  })

  //
  // Read channel logic
  //
  {
    val readRequests = VecInit(io.readPorts.map { port =>
      val request = Wire(Decoupled(chiselTypeOf(io.axiBus.readRequest.bits)))

      request.valid := port.valid
      request.bits.address := port.address
      request.bits.length := port.length

      // Latch requests from clients
      val queue = Queue(request, entries = 1)
      assert(request.ready || !port.valid, "Client initiated overlapping transaction")
      queue
    })

    val readArb = Module(new RRArbiter(chiselTypeOf(readRequests(0).bits), numReadPorts))
    readArb.io.in <> readRequests

    val readBurstActive = RegInit(false.B)
    val readActiveClient = RegInit(0.U(log2Ceil(numReadPorts).W))
    val readBurstCount = RegInit(0.U(io.axiBus.readRequest.bits.length.getWidth.W))

    io.axiBus.readRequest.valid := readArb.io.out.valid && !readBurstActive
    io.axiBus.readRequest.bits.address := readArb.io.out.bits.address
    io.axiBus.readRequest.bits.length := readArb.io.out.bits.length

    // This ensures we only have one request active at a time. We could optimize
    // this by issuing requests whenever the AXI bus is ready, but that would
    // require more complex tracking logic
    readArb.io.out.ready := io.axiBus.readRequest.ready && !readBurstActive

    assert(
      !(io.axiBus.readRequest.fire && io.axiBus.readData.fire),
      "AXI read request and read data fired on the same clock cycle"
    )

    when (io.axiBus.readRequest.fire) {
      readBurstActive := true.B
      readActiveClient := readArb.io.chosen
      readBurstCount := readArb.io.out.bits.length
    }.elsewhen (io.axiBus.readData.fire) {
      when (readBurstCount === 0.U) {
        readBurstActive := false.B
      } .otherwise {
        readBurstCount := readBurstCount - 1.U
      }
    }

    for (i <- 0 until numReadPorts) {
      val port = io.readPorts(i)
      port.data.valid := io.axiBus.readData.valid && readBurstActive && (readActiveClient === i.U)
      port.data.bits  := io.axiBus.readData.bits.data
    }

    io.axiBus.readData.ready := readBurstActive && io.readPorts(readActiveClient).data.ready
  }

  //
  // Write channel logic
  //
  {
    val writeRequests = VecInit(io.writePorts.map { port =>
      val request = Wire(Decoupled(chiselTypeOf(io.axiBus.writeRequest.bits)))

      request.valid := port.valid
      request.bits.address := port.address
      request.bits.length := port.length

      // Latch requests from clients
      val queue = Queue(request, entries = 1)
      assert(request.ready || !port.valid, "Client initiated overlapping transaction")
      queue
    })

    val writeArb = Module(new RRArbiter(chiselTypeOf(writeRequests(0).bits), numWritePorts))
    writeArb.io.in <> writeRequests

    val writeBurstActive = RegInit(false.B)
    val writeActiveClient = RegInit(0.U(log2Ceil(numWritePorts).W))
    val writeBurstCount = RegInit(0.U(io.axiBus.writeRequest.bits.length.getWidth.W))

    io.axiBus.writeRequest.valid := writeArb.io.out.valid && !writeBurstActive
    io.axiBus.writeRequest.bits.address := writeArb.io.out.bits.address
    io.axiBus.writeRequest.bits.length := writeArb.io.out.bits.length

    writeArb.io.out.ready := io.axiBus.writeRequest.ready && !writeBurstActive

    assert(
      !(io.axiBus.writeRequest.fire && io.axiBus.writeData.fire),
      "AXI write request and write data fired on the same clock cycle"
    )

    when (io.axiBus.writeRequest.fire) {
      writeBurstActive := true.B
      writeActiveClient := writeArb.io.chosen
      writeBurstCount := writeArb.io.out.bits.length
    }.elsewhen (io.axiBus.writeData.fire) {
      when (writeBurstCount =/= 0.U) {
        writeBurstCount := writeBurstCount - 1.U
      }
    }.elsewhen (io.axiBus.writeResponse.fire) {
      writeBurstActive := false.B
    }

    for (i <- 0 until numWritePorts) {
      io.writePorts(i).data.ready := (io.axiBus.writeData.ready && writeBurstActive &&
        (writeActiveClient === i.U))
    }

    io.axiBus.writeData.valid := writeBurstActive && io.writePorts(writeActiveClient).data.valid
    io.axiBus.writeData.bits.data := io.writePorts(writeActiveClient).data.bits
    io.axiBus.writeData.bits.last := writeBurstCount === 0.U

    io.axiBus.writeResponse.ready := writeBurstActive
  }
}

