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

class DirectAccessPort(implicit cfg: GpuConfig) extends Bundle {
  val readEn = Input(Bool())
  val readAddress = Input(UInt(cfg.busAddressBits.W))
  val readData = Output(UInt(cfg.busDataBits.W))
  val writeEn = Input(Bool())
  val writeAddress = Input(UInt(cfg.busAddressBits.W))
  val writeData = Input(UInt(cfg.busDataBits.W))
}

/**
  * Simulates an AXI memory interface for testing.
  * memorySize is in busDataWidth words (expected to be 64-bits)
  */
class SimAxiMemory(memorySize: Int)(implicit cfg: GpuConfig) extends Module {
  val io = IO(Flipped(new AxiBus))

  val dap = IO(new DirectAccessPort)

  val memory = SyncReadMem(memorySize, UInt(cfg.busDataBits.W))

  object State extends ChiselEnum {
    val Idle, Write, WriteAck, ReadSetup, ReadData = Value
  }

  val stateReg = RegInit(State.Idle)
  val writeValidReg = RegInit(false.B)
  val writeAddressReg = RegInit(0.U(cfg.busAddressBits.W))
  val writeLengthReg = RegInit(0.U(cfg.busBurstLengthBits.W))
  val readValidReg = RegInit(false.B)
  val readAddressReg = RegInit(0.U(cfg.busAddressBits.W))
  val readLengthReg = RegInit(0.U(cfg.busBurstLengthBits.W))
  val burstAddressReg = RegInit(0.U(cfg.busAddressBits.W))
  val burstLengthReg = RegInit(0.U(cfg.busBurstLengthBits.W))
  val readAddressNext = Mux(io.readData.fire, burstAddressReg + 1.U, burstAddressReg)

  when (!writeValidReg && io.writeRequest.valid) {
    writeValidReg := true.B
    writeAddressReg := io.writeRequest.bits.address >> 3.U
    writeLengthReg := io.writeRequest.bits.length
  }

  when (!readValidReg && io.readRequest.valid) {
    readValidReg := true.B
    readAddressReg := io.readRequest.bits.address >> 3.U
    readLengthReg := io.readRequest.bits.length
  }

  io.readData.bits.data := memory.read(readAddressNext)
  io.writeRequest.ready := !writeValidReg
  io.writeData.ready := (stateReg === State.Write)
  io.writeResponse.valid := (stateReg === State.WriteAck)
  io.readRequest.ready := !readValidReg
  io.readData.valid := (stateReg === State.ReadData)

  switch (stateReg) {
    is (State.Idle) {
      when (writeValidReg) {
        burstAddressReg := writeAddressReg
        burstLengthReg := writeLengthReg
        writeValidReg := false.B
        stateReg := State.Write
      } .elsewhen (readValidReg) {
        burstAddressReg := readAddressReg
        burstLengthReg := readLengthReg
        readValidReg := false.B
        stateReg := State.ReadSetup
      }

      // A2.3.2.2
      // The Subordinate must wait for both ARVALID and ARREADY to be asserted
      // before it asserts RVALID to indicate that valid data is available.
      assert(!io.readData.valid)
    }

    is (State.Write) {
      when (io.writeData.valid) {
        memory.write(burstAddressReg, io.writeData.bits.data)
        when (burstLengthReg === 0.U) {
          assert(io.writeData.bits.last)
          stateReg := State.WriteAck
        } .otherwise {
          burstAddressReg := burstAddressReg + 1.U
          burstLengthReg := burstLengthReg - 1.U
        }
      }
    }

    is (State.WriteAck) {
      when (io.writeResponse.ready) {
        stateReg := State.Idle
      }
    }

    is (State.ReadSetup) {
      stateReg := State.ReadData
    }

    is (State.ReadData) {
      when (io.readData.ready) {
        when (burstLengthReg === 0.U) {
          stateReg := State.Idle
        } .otherwise {
          burstAddressReg := burstAddressReg + 1.U
          burstLengthReg := burstLengthReg - 1.U
        }
      }
    }
  }

  // Simulation
  when (dap.readEn) {
    dap.readData := memory.read(dap.readAddress >> 3.U)
  } .otherwise {
    dap.readData := 0.U
  }

  when (dap.writeEn) {
    memory.write(dap.writeAddress >> 3.U, dap.writeData)
  }
}

object SimMemAccess {
  import chisel3.simulator.PeekPokeAPI._

  val bytesPerWord = 8

  def write(clock: Clock, dap: DirectAccessPort, address: Long, data: Seq[Long]): Unit = {
    dap.writeEn.poke(true.B)
    for (i <- data.indices) {
      dap.writeAddress.poke((address + i * bytesPerWord))

      // Fucking James Gosling...
      dap.writeData.poke(BigInt(java.lang.Long.toUnsignedString(data(i))))
      clock.step()
    }

    dap.writeEn.poke(false.B)
  }

  def read(clock: Clock, dap: DirectAccessPort, address: Long, length: Int): Seq[Long] = {
    val result = scala.collection.mutable.ArrayBuffer[Long]()
    dap.readEn.poke(true.B)
    for (i <- 0 until length) {
      dap.readAddress.poke((address + i * bytesPerWord))
      clock.step()
      result += dap.readData.peek().litValue.toLong
    }

    dap.readEn.poke(false.B)
    result.toSeq
  }
}

class SimAxiMemoryTest extends AnyFunSuite with ChiselSim {
  implicit val cfg: GpuConfig = GpuConfig()

  def readBurst(dut: SimAxiMemory, rng: scala.util.Random, address: Int, length: Int): Seq[Long] = {
    dut.io.readRequest.bits.address.poke(address)
    dut.io.readRequest.bits.length.poke((length - 1))
    dut.io.readRequest.valid.poke(true.B)
    var ready = rng.nextBoolean()
    dut.io.readData.ready.poke(ready.B)
    dut.clock.step()
    dut.io.readRequest.valid.poke(false.B)

    val result = scala.collection.mutable.ArrayBuffer[Long]()
    while (result.length < length) {
      ready = rng.nextBoolean()
      dut.io.readData.ready.poke(ready.B)

      if (ready && dut.io.readData.valid.peek().litToBoolean) {
        result += dut.io.readData.bits.data.peek().litValue.toLong
      }

      dut.clock.step()
    }

    result.toSeq
  }

  def writeBurst(dut: SimAxiMemory, rng: scala.util.Random, address: Int, data: Seq[Long]): Unit = {
    dut.io.writeRequest.bits.address.poke(address)
    dut.io.writeRequest.bits.length.poke((data.length - 1))
    dut.io.writeRequest.valid.poke(true.B)
    dut.clock.step()
    dut.io.writeRequest.valid.poke(false.B)

    var written = 0
    while (written < data.length) {
      dut.io.writeData.bits.data.poke(data(written))
      dut.io.writeData.bits.last.poke(written == data.length - 1)
      val valid = rng.nextBoolean()
      dut.io.writeData.valid.poke(valid)
      if (valid && dut.io.writeData.ready.peek().litToBoolean) {
        written += 1
      }

      dut.clock.step()
    }

    dut.io.writeData.valid.poke(false.B)
    dut.io.writeResponse.ready.poke(false.B)
    for (_ <- 0 until rng.nextInt(3)) {
      dut.clock.step()
    }

    dut.io.writeResponse.ready.poke(true.B)

    while (!dut.io.writeResponse.valid.peek().litToBoolean) {
      dut.clock.step()
    }

    dut.clock.step()
  }

  test("SimAxiMemory read") {
    val memorySize = 256
    simulate(new SimAxiMemory(memorySize)) { dut =>
      dut.io.writeRequest.valid.poke(false.B)
      dut.io.writeData.valid.poke(false.B)
      dut.io.writeResponse.ready.poke(false.B)
      dut.io.readRequest.valid.poke(false.B)
      dut.io.readData.ready.poke(false.B)

      val maxBurst = 64
      val rng = new scala.util.Random(42)
      val testData: Seq[Long] = (1 to memorySize).map(i => i + 1024)
      SimMemAccess.write(dut.clock, dut.dap, 0, testData)

      for (_ <- 0 until 1000) {
        val burstLengthReg = rng.nextInt(maxBurst - 1) + 1
        val address = rng.nextInt(memorySize - maxBurst) * 8
        val readData = this.readBurst(dut, rng, address, burstLengthReg)
        assert(readData == testData.slice(address / 8, address / 8 + burstLengthReg))
      }
    }
  }

  test("SimAxiMemory write") {
    val memorySize = 0x1000
    simulate(new SimAxiMemory(memorySize)) { dut =>
      dut.io.writeRequest.valid.poke(false.B)
      dut.io.writeData.valid.poke(false.B)
      dut.io.writeResponse.ready.poke(false.B)
      dut.io.readRequest.valid.poke(false.B)
      dut.io.readData.ready.poke(false.B)

      val maxBurst = 32
      val rng = new scala.util.Random(42)
      val testData: Seq[Long] = (1 to memorySize).map(i => i + 1024)
      var offset = 0
      while (offset < memorySize) {
        val burstLengthReg = math.min(memorySize - offset, rng.nextInt(maxBurst - 1) + 1)
        this.writeBurst(dut, rng, offset * 8, testData.slice(offset, offset + burstLengthReg))
        offset += burstLengthReg
      }

      val readData = SimMemAccess.read(dut.clock, dut.dap, 0, memorySize)
      assert(readData == testData)
    }
  }
}
