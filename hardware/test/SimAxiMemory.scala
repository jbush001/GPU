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

class DirectAccessPort extends Bundle {
  val readEn = Input(Bool())
  val readAddress = Input(UInt(AxiConsts.addressBits.W))
  val readData = Output(UInt(AxiConsts.dataBits.W))
  val writeEn = Input(Bool())
  val writeAddress = Input(UInt(AxiConsts.addressBits.W))
  val writeData = Input(UInt(AxiConsts.dataBits.W))
}

class SimAxiMemory(memorySize: Int) extends Module {
  val io = IO(Flipped(new AxiBus))

  val dap = IO(new DirectAccessPort)

  val memory = SyncReadMem(memorySize, UInt(AxiConsts.dataBits.W))

  object State extends ChiselEnum {
    val Idle, Write, WriteAck, ReadSetup, ReadData = Value
  }

  val state = RegInit(State.Idle)
  val writeLatched = RegInit(false.B)
  val writeAddress = RegInit(0.U(AxiConsts.addressBits.W))
  val writeLength = RegInit(0.U(AxiConsts.burstLengthBits.W))
  val readLatched = RegInit(false.B)
  val readAddress = RegInit(0.U(AxiConsts.addressBits.W))
  val readLength = RegInit(0.U(AxiConsts.burstLengthBits.W))
  val burstAddress = RegInit(0.U(AxiConsts.addressBits.W))
  val burstLength = RegInit(0.U(AxiConsts.burstLengthBits.W))
  val readAddressNext = Mux(io.readData.fire, burstAddress + 1.U, burstAddress)

  when (!writeLatched && io.writeRequest.valid) {
    writeLatched := true.B
    writeAddress := io.writeRequest.bits.address >> 2.U
    writeLength := io.writeRequest.bits.length
  }

  when (!readLatched && io.readRequest.valid) {
    readLatched := true.B
    readAddress := io.readRequest.bits.address >> 2.U
    readLength := io.readRequest.bits.length
  }

  io.readData.bits.data := memory.read(readAddressNext)
  io.writeRequest.ready := !writeLatched
  io.writeData.ready := (state === State.Write)
  io.writeResponse.valid := (state === State.WriteAck)
  io.readRequest.ready := !readLatched
  io.readData.valid := (state === State.ReadData)

  switch (state) {
    is (State.Idle) {
      when (writeLatched) {
        burstAddress := writeAddress
        burstLength := writeLength
        writeLatched := false.B
        state := State.Write
      } .elsewhen (readLatched) {
        burstAddress := readAddress
        burstLength := readLength
        readLatched := false.B
        state := State.ReadSetup
      }

      // A2.3.2.2
      // The Subordinate must wait for both ARVALID and ARREADY to be asserted
      // before it asserts RVALID to indicate that valid data is available.
      assert(!io.readData.valid)
    }

    is (State.Write) {
      when (io.writeData.valid) {
        memory.write(burstAddress, io.writeData.bits.data)
        when (burstLength === 0.U) {
          assert(io.writeData.bits.last)
          state := State.WriteAck
        } .otherwise {
          burstAddress := burstAddress + 1.U
          burstLength := burstLength - 1.U
        }
      }
    }

    is (State.WriteAck) {
      when (io.writeResponse.ready) {
        state := State.Idle
      }
    }

    is (State.ReadSetup) {
      state := State.ReadData
    }

    is (State.ReadData) {
      when (io.readData.ready) {
        when (burstLength === 0.U) {
          state := State.Idle
        } .otherwise {
          burstAddress := burstAddress + 1.U
          burstLength := burstLength - 1.U
        }
      }
    }
  }

  // Simulation
  when (dap.readEn) {
    dap.readData := memory.read(dap.readAddress >> 2.U)
  } .otherwise {
    dap.readData := 0.U
  }

  when (dap.writeEn) {
    memory.write(dap.writeAddress >> 2.U, dap.writeData)
  }
}

object SimMemAccess {
  import chisel3.simulator.PeekPokeAPI._

  def write(clock: Clock, dap: DirectAccessPort, address: Long, data: Seq[Long]): Unit = {
    dap.writeEn.poke(true.B)
    for (i <- data.indices) {
      dap.writeAddress.poke((address + i * 4).U)
      dap.writeData.poke((data(i) & 0xffffffffL).U)
      clock.step()
    }

    dap.writeEn.poke(false.B)
  }

  def read(clock: Clock, dap: DirectAccessPort, address: Long, length: Int): Seq[Long] = {
    val result = scala.collection.mutable.ArrayBuffer[Long]()
    dap.readEn.poke(true.B)
    for (i <- 0 until length) {
      dap.readAddress.poke((address + i * 4).U)
      clock.step()
      result += dap.readData.peek().litValue.toLong & 0xffffffffL
    }

    dap.readEn.poke(false.B)
    result.toSeq
  }
}

class SimAxiMemoryTest extends AnyFunSuite with ChiselSim {
  def readBurst(dut: SimAxiMemory, rng: scala.util.Random, address: Int, length: Int): Seq[Long] = {
    dut.io.readRequest.bits.address.poke(address.U)
    dut.io.readRequest.bits.length.poke((length - 1).U)
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
        result += dut.io.readData.bits.data.peek().litValue.toLong & 0xffffffffL
      }

      dut.clock.step()
    }

    result.toSeq
  }

  def writeBurst(dut: SimAxiMemory, rng: scala.util.Random, address: Int, data: Seq[Long]): Unit = {
    dut.io.writeRequest.bits.address.poke(address.U)
    dut.io.writeRequest.bits.length.poke((data.length - 1).U)
    dut.io.writeRequest.valid.poke(true.B)
    dut.clock.step()
    dut.io.writeRequest.valid.poke(false.B)

    var written = 0
    while (written < data.length) {
      dut.io.writeData.bits.data.poke(data(written).U)
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
        val burstLength = rng.nextInt(maxBurst - 1) + 1
        val address = rng.nextInt(memorySize - maxBurst) * 4
        val readData = this.readBurst(dut, rng, address, burstLength)
        assert(readData == testData.slice(address / 4, address / 4 + burstLength))
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
        val burstLength = math.min(memorySize - offset, rng.nextInt(maxBurst - 1) + 1)
        this.writeBurst(dut, rng, offset * 4, testData.slice(offset, offset + burstLength))
        offset += burstLength
      }

      val readData = SimMemAccess.read(dut.clock, dut.dap, 0, memorySize)
      assert(readData == testData)
    }
  }
}
