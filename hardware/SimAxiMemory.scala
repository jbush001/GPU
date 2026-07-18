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
  val writeRequest = RegInit(0.U(AxiConsts.addressBits.W))
  val writeLength = RegInit(0.U(AxiConsts.burstLengthBits.W))
  val readLatched = RegInit(false.B)
  val readRequest = RegInit(0.U(AxiConsts.addressBits.W))
  val readLength = RegInit(0.U(AxiConsts.burstLengthBits.W))
  val burstAddress = RegInit(0.U(AxiConsts.addressBits.W))
  val burstLength = RegInit(0.U(AxiConsts.burstLengthBits.W))

  when (!writeLatched && io.axi.writeRequest.valid) {
      writeLatched := true.B
      writeRequest := io.axi.writeRequest.bits.address >> 2.U
      writeLength := io.axi.writeRequest.bits.length
  }

  when (!readLatched && io.axi.readRequest.valid) {
      readLatched := true.B
      readRequest := io.axi.readRequest.bits.address >> 2.U
      readLength := io.axi.readRequest.bits.length
  }

  io.axi.readData.bits.data := memory.read(burstAddress)
  io.axi.writeRequest.ready := !writeLatched
  io.axi.writeData.ready := (state === State.Write)
  io.axi.writeResponse.valid := (state === State.WriteAck)
  io.axi.readRequest.ready := !readLatched
  io.axi.readData.valid := (state === State.ReadData)

  switch (state) {
    is (State.Idle) {
      when (writeLatched) {
        burstAddress := writeRequest
        burstLength := writeLength
        writeLatched := false.B
        state := State.Write
      } .elsewhen (readLatched) {
        burstAddress := readRequest
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
      io.axi.writeResponse.valid := true.B
      when (io.axi.writeResponse.ready) {
        state := State.Idle
      }
    }

    is (State.ReadSetup) {
      state := State.ReadData
      when (io.axi.readData.ready) {
        burstAddress := burstAddress + 1.U
      }
    }

    is (State.ReadData) {
      io.axi.readData.valid := true.B
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
  def readBurst(dut: SimAxiMemory, address: Int, length: Int): Seq[Long] = {
    dut.io.axi.readRequest.bits.address.poke(address.U)
    dut.io.axi.readRequest.bits.length.poke((length - 1).U)
    dut.io.axi.readRequest.valid.poke(true.B)
    dut.clock.step()
    dut.io.axi.readRequest.valid.poke(false.B)
    dut.io.axi.readData.ready.poke(true.B)

    val result = scala.collection.mutable.ArrayBuffer[Long]()
    for (_ <- 0 until length) {
      while (!dut.io.axi.readData.valid.peek().litToBoolean) {
        dut.clock.step()
      }

      result += dut.io.axi.readData.bits.data.peek().litValue.toLong & 0xffffffffL
      dut.clock.step()
    }

    result.toSeq
  }

  def writeBurst(dut: SimAxiMemory, address: Int, data: Seq[Long]): Unit = {
    dut.io.axi.writeRequest.bits.address.poke(address.U)
    dut.io.axi.writeRequest.bits.length.poke((data.length - 1).U)
    dut.io.axi.writeRequest.valid.poke(true.B)
    dut.clock.step()
    dut.io.axi.writeRequest.valid.poke(false.B)

    for (i <- data.indices) {
      while (!dut.io.axi.writeData.ready.peek().litToBoolean) {
        dut.clock.step()
      }

      dut.io.axi.writeData.bits.data.poke(data(i).U)
      dut.io.axi.writeData.bits.last.poke(i == data.length - 1)
      dut.io.axi.writeData.valid.poke(true.B)
      dut.clock.step()
      dut.io.axi.writeData.valid.poke(false.B)
    }

    while (!dut.io.axi.writeResponse.valid.peek().litToBoolean) {
      dut.clock.step()
    }

    dut.io.axi.writeResponse.ready.poke(true.B)
    dut.clock.step()
    dut.io.axi.writeResponse.ready.poke(false.B)
  }

  test("sim axi read") {
    simulate(new SimAxiMemory(128)) { dut =>
      dut.io.axi.writeRequest.valid.poke(false.B)
      dut.io.axi.writeData.valid.poke(false.B)
      dut.io.axi.writeResponse.ready.poke(false.B)
      dut.io.axi.readRequest.valid.poke(false.B)
      dut.io.axi.readData.ready.poke(false.B)

      val testData = Seq[Long](1, 2, 3, 4, 5)
      SimMemAccess.write(dut, 0, testData)
      val readData = this.readBurst(dut, 0, testData.length)
      assert(readData == testData)
    }
  }

  test("sim axi write") {
    simulate(new SimAxiMemory(128)) { dut =>
      dut.io.axi.writeRequest.valid.poke(false.B)
      dut.io.axi.writeData.valid.poke(false.B)
      dut.io.axi.writeResponse.ready.poke(false.B)
      dut.io.axi.readRequest.valid.poke(false.B)
      dut.io.axi.readData.ready.poke(false.B)

      val testData = Seq[Long](10, 20, 30, 40, 50, 60, 70, 80)
      val baseAddress = 10
      this.writeBurst(dut, baseAddress, testData)
      val readData = SimMemAccess.read(dut, baseAddress, testData.length)
      assert(readData == testData)
    }
  }
}
