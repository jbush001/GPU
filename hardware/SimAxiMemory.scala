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

class SimAxiMemory(memorySize: Int) extends Module {
  val io = IO(new Bundle {
      val axi = Flipped(new AxiBus)

      // Simulator accessors for testing
      val simReadEn = Input(Bool())
      val simReadAddress = Input(UInt(AxiConsts.addressBits.W))
      val simReadData = Output(UInt(AxiConsts.dataBits.W))
      val simWriteEn = Input(Bool())
      val simWriteAddress = Input(UInt(AxiConsts.addressBits.W))
      val simWriteData = Input(UInt(AxiConsts.dataBits.W))
  })

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
  val readAddressNext = Mux(io.axi.readData.fire, burstAddress + 1.U, burstAddress)

  when (!writeLatched && io.axi.writeRequest.valid) {
    writeLatched := true.B
    writeAddress := io.axi.writeRequest.bits.address >> 2.U
    writeLength := io.axi.writeRequest.bits.length
  }

  when (!readLatched && io.axi.readRequest.valid) {
    readLatched := true.B
    readAddress := io.axi.readRequest.bits.address >> 2.U
    readLength := io.axi.readRequest.bits.length
  }

  io.axi.readData.bits.data := memory.read(readAddressNext)
  io.axi.writeRequest.ready := !writeLatched
  io.axi.writeData.ready := (state === State.Write)
  io.axi.writeResponse.valid := (state === State.WriteAck)
  io.axi.readRequest.ready := !readLatched
  io.axi.readData.valid := (state === State.ReadData)

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
    }

    is (State.Write) {
      when (io.axi.writeData.valid) {
        memory.write(burstAddress, io.axi.writeData.bits.data)

        when (burstLength === 0.U) {
          state := State.WriteAck
        } .otherwise {
          burstAddress := burstAddress + 1.U
          burstLength := burstLength - 1.U
        }
      }
    }

    is (State.WriteAck) {
      when (io.axi.writeResponse.ready) {
        state := State.Idle
      }
    }

    is (State.ReadSetup) {
      state := State.ReadData
    }

    is (State.ReadData) {
      when (io.axi.readData.ready) {
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
  when (io.simReadEn) {
    io.simReadData := memory.read(io.simReadAddress >> 2.U)
  } .otherwise {
    io.simReadData := 0.U
  }

  when (io.simWriteEn) {
    memory.write(io.simWriteAddress >> 2.U, io.simWriteData)
  }
}

object SimMemAccess {
  import chisel3.simulator.PeekPokeAPI._

  def write(dut: SimAxiMemory, address: Long, data: Seq[Long]): Unit = {
    dut.io.simWriteEn.poke(true.B)
    for (i <- data.indices) {
      dut.io.simWriteAddress.poke((address + i * 4).U)
      dut.io.simWriteData.poke((data(i) & 0xffffffffL).U)
      dut.clock.step()
    }

    dut.io.simWriteEn.poke(false.B)
  }

  def read(dut: SimAxiMemory, address: Long, length: Int): Seq[Long] = {
    val result = scala.collection.mutable.ArrayBuffer[Long]()
    dut.io.simReadEn.poke(true.B)
    for (i <- 0 until length) {
      dut.io.simReadAddress.poke((address + i * 4).U)
      dut.clock.step()
      result += dut.io.simReadData.peek().litValue.toLong & 0xffffffffL
    }

    dut.io.simReadEn.poke(false.B)
    result.toSeq
  }
}

class SimAxiMemoryTest extends AnyFunSuite with ChiselSim {
  def readBurst(dut: SimAxiMemory, rng: scala.util.Random, address: Int, length: Int): Seq[Long] = {
    dut.io.axi.readRequest.bits.address.poke(address.U)
    dut.io.axi.readRequest.bits.length.poke((length - 1).U)
    dut.io.axi.readRequest.valid.poke(true.B)
    var ready = rng.nextBoolean()
    dut.io.axi.readData.ready.poke(ready.B)
    dut.clock.step()
    dut.io.axi.readRequest.valid.poke(false.B)

    val result = scala.collection.mutable.ArrayBuffer[Long]()
    while (result.length < length) {
      ready = rng.nextBoolean()
      dut.io.axi.readData.ready.poke(ready.B)

      if (ready && dut.io.axi.readData.valid.peek().litToBoolean) {
        result += dut.io.axi.readData.bits.data.peek().litValue.toLong & 0xffffffffL
      }

      dut.clock.step()
    }

    result.toSeq
  }

  def writeBurst(dut: SimAxiMemory, rng: scala.util.Random, address: Int, data: Seq[Long]): Unit = {
    dut.io.axi.writeRequest.bits.address.poke(address.U)
    dut.io.axi.writeRequest.bits.length.poke((data.length - 1).U)
    dut.io.axi.writeRequest.valid.poke(true.B)
    dut.clock.step()
    dut.io.axi.writeRequest.valid.poke(false.B)

    var written = 0
    while (written < data.length) {
      dut.io.axi.writeData.bits.data.poke(data(written).U)
      dut.io.axi.writeData.bits.last.poke(written == data.length - 1)
      val valid = rng.nextBoolean()
      dut.io.axi.writeData.valid.poke(valid)
      if (valid && dut.io.axi.writeData.ready.peek().litToBoolean) {
        written += 1
      }

      dut.clock.step()
    }

    dut.io.axi.writeData.valid.poke(false.B)
    dut.io.axi.writeResponse.ready.poke(false.B)
    for (_ <- 0 until rng.nextInt(3)) {
      dut.clock.step()
    }

    dut.io.axi.writeResponse.ready.poke(true.B)

    while (!dut.io.axi.writeResponse.valid.peek().litToBoolean) {
      dut.clock.step()
    }

    dut.clock.step()
  }

  test("sim axi read") {
    val memorySize = 256
    simulate(new SimAxiMemory(memorySize)) { dut =>
      dut.io.axi.writeRequest.valid.poke(false.B)
      dut.io.axi.writeData.valid.poke(false.B)
      dut.io.axi.writeResponse.ready.poke(false.B)
      dut.io.axi.readRequest.valid.poke(false.B)
      dut.io.axi.readData.ready.poke(false.B)

      val maxBurst = 64
      val rng = new scala.util.Random(42)
      val testData: Seq[Long] = (1 to memorySize).map(i => i + 1024)
      SimMemAccess.write(dut, 0, testData)

      for (_ <- 0 until 1000) {
        val burstLength = rng.nextInt(maxBurst - 1) + 1
        val address = rng.nextInt(memorySize - maxBurst) * 4
        val readData = this.readBurst(dut, rng, address, burstLength)
        assert(readData == testData.slice(address / 4, address / 4 + burstLength))
      }
    }
  }

  test("sim axi write") {
    val memorySize = 0x1000
    simulate(new SimAxiMemory(memorySize)) { dut =>
      dut.io.axi.writeRequest.valid.poke(false.B)
      dut.io.axi.writeData.valid.poke(false.B)
      dut.io.axi.writeResponse.ready.poke(false.B)
      dut.io.axi.readRequest.valid.poke(false.B)
      dut.io.axi.readData.ready.poke(false.B)

      val maxBurst = 32
      val rng = new scala.util.Random(42)
      val testData: Seq[Long] = (1 to memorySize).map(i => i + 1024)
      var offset = 0
      while (offset < memorySize) {
        val burstLength = math.min(memorySize - offset, rng.nextInt(maxBurst - 1) + 1)
        this.writeBurst(dut, rng, offset * 4, testData.slice(offset, offset + burstLength))
        offset += burstLength
      }

      val readData = SimMemAccess.read(dut, 0, memorySize)
      assert(readData == testData)
    }
  }
}
