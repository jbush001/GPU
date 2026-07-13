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
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funsuite.AnyFunSuite

object MemoryConsts {
  val addrBits: Int = 32
  val dataBits: Int = 32
  val lengthBits: Int = 8
}

/** A simple burst-oriented memory read port.
  *
  * Protocol:
  *
  *   - On the next clock edge after `request` goes high, a burst becomes
  *     active and the arbiter latches `address` and `length`. The client
  *     MAY change `address`/`length` after the burst starts, but changes
  *     are ignored until the next burst begins.
  *   - `length` is the number of beats in the burst minus one (0 == one
  *     transfer).
  *   - A data word is consumed iff both `data.valid` and `data.ready` are
  *     true; the arbiter advances to the next word only after a word is
  *     consumed.
  *   - After the arbiter has asserted `data.valid`, it MUST NOT deassert
  *     it or change the contents of `data.bits`.
  *   - A burst completes on the clock edge the last word is consumed.
  *   - The client MUST NOT raise `request` while a burst is active.
  *   - `data.valid` and `data.ready` MUST NOT be combinationally dependent
  *     on each other.
  *   - The arbiter MAY raise `data.valid` before a burst is active; the
  *     client MAY consume this data early.
  */
 class MemReadPort extends Bundle {
  val request = Output(Bool())
  val address = Output(UInt(MemoryConsts.addrBits.W))
  val length = Output(UInt(MemoryConsts.lengthBits.W)) 
  val data = Flipped(Decoupled(UInt(MemoryConsts.dataBits.W)))
}


/** A simple burst-oriented memory write port. Same protocol as
  * [[MemReadPort]], except anywhere where the data is referenced
  * the roles of the client and arbiter are reversed.
  */
class MemWritePort extends Bundle {
  val request = Output(Bool())
  val address = Output(UInt(MemoryConsts.addrBits.W))
  val length = Output(UInt(MemoryConsts.lengthBits.W))
  val data = Decoupled(UInt(MemoryConsts.dataBits.W))
}

/** Multiplexes [[numReadPorts]] read clients and [[numWritePorts]] write
  * clients onto a single memory, using the burst protocol documented on
  * [[MemReadPort]] and [[MemWritePort]].
  *
  * @todo The implementation of this is currently a stub impementation,
  * with memory stored inside the block rather than connecting to 
  * an external bus.
  */  
class MemoryArbiter(
  var numReadPorts: Int = 2,
  var numWritePorts: Int = 2
) extends Module {
  val io = IO(new Bundle {
    val readPorts = Vec(numReadPorts, Flipped(new MemReadPort))
    val writePorts = Vec(numWritePorts, Flipped(new MemWritePort))
  })

  val memorySize = 0x10000
  val mem = SyncReadMem(memorySize, UInt(MemoryConsts.dataBits.W))

  for (i <- 0 until numReadPorts) {
    val nextAddress = Wire(UInt(log2Up(memorySize).W))
    val address = RegNext(nextAddress)
    val count = Reg(UInt(5.W)) // How many remain to be consumed
    val burstActive = RegInit(false.B)

    assert(!(burstActive && io.readPorts(i).request), 
      "PROTOCOL VIOLATION: Reader raised 'request' while a burst was already active.")

    io.readPorts(i).data.valid := burstActive
    io.readPorts(i).data.bits := mem.read(nextAddress)

    nextAddress := address
    when (burstActive) {
      when (io.readPorts(i).data.fire) {
        when (count === 0.U) {
          burstActive := false.B
        }.otherwise {
          nextAddress := address + 1.U
          count := count - 1.U
        }
      }
    }.elsewhen (io.readPorts(i).request) {
      burstActive := true.B
      nextAddress := io.readPorts(i).address
      count := io.readPorts(i).length
    }
  }

  for (i <- 0 until numWritePorts) {
    val nextAddress = Wire(UInt(log2Up(memorySize).W))
    val address = RegNext(nextAddress)
    val count = Reg(UInt(MemoryConsts.lengthBits.W)) // How many remain to be consumed
    val burstActive = RegInit(false.B)

    assert(!(burstActive && io.writePorts(i).request), 
      "PROTOCOL VIOLATION: Writer raised 'request' while a burst was already active.")

    io.writePorts(i).data.ready := burstActive

    nextAddress := address
    when (burstActive) {
      when (io.writePorts(i).data.fire) {
        nextAddress := address + 1.U
        when (count === 0.U) {
          burstActive := false.B
        }.otherwise {
          count := count - 1.U
        }
      }
    }
    
    when (io.writePorts(i).request) {
      burstActive := true.B
      nextAddress := io.writePorts(i).address
      count := io.writePorts(i).length
    }

    when (io.writePorts(i).data.fire) {
      mem.write(address, io.writePorts(i).data.bits)
    }
  }
}

class MemoryArbiterTests extends AnyFunSuite with ChiselSim {
  def testDataForAddr(i: Int): Long = ((1L << (i % 32)) ^ i) & 0xffffffffL

  test("memory arbiter round trip") {
    simulate(new MemoryArbiter(1, 1)) { dut =>
      val rng = new scala.util.Random(42)

      // Write burst, fill with a known pattern
      val writeAddress = 8
      val writeBurstLength = 64
      dut.io.writePorts(0).address.poke(writeAddress.U)
      dut.io.writePorts(0).length.poke((writeBurstLength - 1).U)
      dut.io.writePorts(0).request.poke(true.B)
      dut.clock.step()
      dut.io.writePorts(0).request.poke(false.B)
      dut.io.writePorts(0).data.valid.poke(true.B)

      // TODO toggle valid to ensure it is respected
      for (i <- 0 until writeBurstLength) {
        dut.io.writePorts(0).data.bits.poke(testDataForAddr(writeAddress + i).U)
        dut.clock.step()
      }

      dut.io.writePorts(0).data.valid.poke(false.B)

      val readAddress1 = 8
      val readBurstLength1 = 8
      dut.io.readPorts(0).address.poke(readAddress1.U)
      dut.io.readPorts(0).length.poke((readBurstLength1 - 1).U) 
      dut.io.readPorts(0).request.poke(true.B)
      dut.clock.step()
      dut.io.readPorts(0).request.poke(false.B) // De-assert request immediately

      var wordsRead = 0
      while (wordsRead < readBurstLength1) {
        val ready = rng.nextBoolean()
        dut.io.readPorts(0).data.ready.poke(ready.B)
        
        if (dut.io.readPorts(0).data.valid.peek().litToBoolean && ready) {
          val expected = testDataForAddr(readAddress1 + wordsRead)
          dut.io.readPorts(0).data.bits.expect(expected.U)
          wordsRead += 1
        }
        dut.clock.step()
      }

      dut.io.readPorts(0).data.valid.expect(false.B)

      // Second burst. Ensure the arbiter can start a second burst successfully and
      // that it works with different offsets/length
      val burstAddress2 = 16
      val burstLength2 = 12
      dut.io.readPorts(0).address.poke(burstAddress2.U)
      dut.io.readPorts(0).length.poke((burstLength2 - 1).U)
      dut.io.readPorts(0).request.poke(true.B)
      dut.clock.step()
      dut.io.readPorts(0).request.poke(false.B)

      wordsRead = 0
      while (wordsRead < burstLength2) {
        val ready = rng.nextBoolean().B
        dut.io.readPorts(0).data.ready.poke(ready)
        
        if (dut.io.readPorts(0).data.valid.peek().litToBoolean && ready.litToBoolean) {
          val expected = testDataForAddr(burstAddress2 + wordsRead)
          dut.io.readPorts(0).data.bits.expect(expected.U)
          wordsRead += 1
        }
        dut.clock.step()
      }

      dut.io.readPorts(0).data.valid.expect(false.B)
    }
  }
}
