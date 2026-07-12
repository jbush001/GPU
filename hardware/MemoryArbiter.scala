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
  val addrWidth: Int = 32
  val dataWidth: Int = 32
}

// length is number of words to transfer minus one (zero based)

class MemReadPort extends Bundle {
  val request = Output(Bool())
  val address = Output(UInt(MemoryConsts.addrWidth.W))
  val length = Output(UInt(5.W)) 
  val data = Flipped(Decoupled(UInt(MemoryConsts.dataWidth.W)))
}

class MemWritePort extends Bundle {
  val request = Output(Bool())
  val address = Output(UInt(MemoryConsts.addrWidth.W))
  val length = Output(UInt(5.W))
  val data = Decoupled(UInt(MemoryConsts.dataWidth.W))
}

class MemoryArbiter(
  var numReadPorts: Int = 2,
  var numWritePorts: Int = 2
) extends Module {
  val io = IO(new Bundle {
    val readPorts = Vec(numReadPorts, Flipped(new MemReadPort))
    val writePorts = Vec(numReadPorts, Flipped(new MemWritePort))
  })

  // TODO: for now, this is a stub implementation with the memory inside 
  // this block, but the proper implementation will interface with an external
  // bus.
  val memorySize = 0x10000
  val mem = SyncReadMem(memorySize, UInt(MemoryConsts.dataWidth.W))

  for (i <- 0 until numReadPorts) {
    val nextAddress = Wire(UInt(log2Up(memorySize).W))
    val address = RegNext(nextAddress)
    val count = Reg(UInt(5.W)) // How many remain to be consumed
    val burstActive = RegInit(false.B)

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
    val count = Reg(UInt(5.W)) // How many remain to be consumed
    val burstActive = RegInit(false.B)

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
