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
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

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
  *   - The client MUST NOT raise `valid` while a burst is active.
  *   - `data.valid` and `data.ready` MUST NOT be combinationally dependent
  *     on each other.
  *   - The arbiter MAY raise `data.valid` before a burst is active; the
  *     client MAY consume this data early.
  */
 class MemReadPort extends Bundle {
  val valid = Output(Bool())
  val address = Output(UInt(AxiConsts.addressBits.W))
  val length = Output(UInt(AxiConsts.burstLengthBits.W))
  val data = Flipped(Decoupled(UInt(AxiConsts.dataBits.W)))
}

/** A simple burst-oriented memory write port. Same protocol as
  * [[MemReadPort]], except anywhere where the data is referenced
  * the roles of the client and arbiter are reversed.
  */
class MemWritePort extends Bundle {
  val valid = Output(Bool())
  val address = Output(UInt(AxiConsts.addressBits.W))
  val length = Output(UInt(AxiConsts.burstLengthBits.W))
  val data = Decoupled(UInt(AxiConsts.dataBits.W))
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
    val readActiveClient = Reg(UInt(log2Ceil(numReadPorts).W))
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
    val writeActiveClient = Reg(UInt(log2Ceil(numWritePorts).W))
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

class MemoryArbiterTests extends AnyFunSuite with ChiselSim {
  // Single read burst
  test("MemoryArbiter read burst") {
    simulate(new MemoryArbiter(1, 1)) { dut =>
      dut.io.readPorts(0).valid.poke(true.B)

      val address = 33
      val testData = Seq(10, 20, 30, 40, 50, 60, 70)

      dut.io.readPorts(0).address.poke(address)
      dut.io.readPorts(0).length.poke(testData.length - 1)
      dut.io.axiBus.readRequest.ready.poke(false.B)
      dut.io.axiBus.readData.valid.poke(false.B)
      dut.clock.step()
      dut.io.readPorts(0).valid.poke(false.B)

      dut.io.axiBus.readRequest.valid.expect(true.B)
      dut.io.axiBus.readRequest.bits.address.expect(address.U)
      dut.io.axiBus.readRequest.bits.length.expect((testData.length - 1).U)

      // Drive ready high to accept the address request
      dut.io.axiBus.readRequest.ready.poke(true.B)
      dut.clock.step()

      // On the next cycle, readRequest.valid must drop to false because burstActive is now true
      dut.io.axiBus.readRequest.valid.expect(false.B)
      dut.io.axiBus.readRequest.ready.poke(false.B)

      dut.io.readPorts(0).data.ready.poke(true.B)

      dut.io.axiBus.readData.valid.poke(true.B)
      for (i <- 0 until testData.length) {
        dut.io.axiBus.readData.bits.data.poke(testData(i).U)
        dut.io.readPorts(0).data.valid.expect(true.B)
        dut.io.readPorts(0).data.bits.expect(testData(i).U)
        dut.clock.step()
      }

      dut.io.axiBus.readData.valid.poke(false.B)
      dut.io.readPorts(0).data.valid.expect(false.B)
    }
  }

  test("MemoryArbiter write burst") {
    simulate(new MemoryArbiter(1, 1)) { dut =>
      // Initialize inputs
      dut.io.writePorts(0).valid.poke(true.B)

      val address = 33
      val testData = Seq(10, 20, 30, 40, 50, 60, 70)

      dut.io.writePorts(0).address.poke(address)
      dut.io.writePorts(0).length.poke(testData.length - 1)
      dut.io.axiBus.writeRequest.ready.poke(false.B)
      dut.io.axiBus.writeData.ready.poke(false.B)
      dut.clock.step()
      dut.io.writePorts(0).valid.poke(false.B)

      dut.io.axiBus.writeRequest.valid.expect(true.B)
      dut.io.axiBus.writeRequest.bits.address.expect(address)
      dut.io.axiBus.writeRequest.bits.length.expect(testData.length - 1)

      dut.io.axiBus.writeRequest.ready.poke(true.B)
      dut.clock.step()

      dut.io.axiBus.writeRequest.valid.expect(false.B)
      dut.io.axiBus.writeRequest.ready.poke(false.B)

      dut.io.writePorts(0).data.valid.poke(true.B)

      for (i <- 0 until 7) {
        dut.io.writePorts(0).data.valid.poke(true.B)
        dut.io.writePorts(0).data.bits.poke(testData(i).U)
        dut.io.axiBus.writeData.ready.poke(true.B)
        dut.io.axiBus.writeData.bits.data.expect(testData(i).U)
        dut.clock.step()
      }

      dut.io.axiBus.writeData.ready.poke(false.B)

      dut.clock.step()

      dut.io.axiBus.writeResponse.ready.expect(true.B)
      dut.io.axiBus.writeResponse.valid.poke(true.B)
      dut.clock.step()

      dut.io.axiBus.writeData.ready.poke(false.B)
    }
  }

  //
  // Stress test
  // TODO: this does not exercise queuing the next address before the previous
  // transaction has completed.
  //
  test("MemoryArbiter stress") {
    val numReaders = 4
    val numWriters = 4
    val memorySize = 1024

    simulate(new Module {
      val io = IO(new Bundle {
        val readPorts  = Vec(numReaders, Flipped(new MemReadPort))
        val writePorts = Vec(numWriters, Flipped(new MemWritePort))
        val dap = new DirectAccessPort
      })
      val arbiter = Module(new MemoryArbiter(numReaders, numWriters))
      val memory  = Module(new SimAxiMemory(memorySize))

      arbiter.io.readPorts  <> io.readPorts
      arbiter.io.writePorts <> io.writePorts
      arbiter.io.axiBus <> memory.io.axi
      memory.dap <> io.dap
    }) { dut =>
      val maxBurstLength = 8

      // Initialize memory with a known pattern.
      val reference = Array.fill(memorySize)(Random.nextLong() & 0xffffffffL)

      SimMemAccess.write(dut.clock, dut.io.dap, 0, reference.toSeq)

      val activeRanges = ArrayBuffer[(Int, Int)]()
      val rng = new Random(42)

      // Initialize DUT inputs to safe defaults
      for (i <- 0 until numReaders) {
        dut.io.readPorts(i).valid.poke(false.B)
        dut.io.readPorts(i).address.poke(0.U)
        dut.io.readPorts(i).length.poke(0.U)
        dut.io.readPorts(i).data.ready.poke(false.B)
      }
      for (i <- 0 until numWriters) {
        dut.io.writePorts(i).valid.poke(false.B)
        dut.io.writePorts(i).address.poke(0.U)
        dut.io.writePorts(i).length.poke(0.U)
        dut.io.writePorts(i).data.valid.poke(false.B)
        dut.io.writePorts(i).data.bits.poke(0.U)
      }

      def getRandomRange(): (Int, Int) = {
        var start = 0
        var end = 0
        var found = false
        while (!found) {
          start = rng.nextInt(memorySize - maxBurstLength - 2)
          end = start + rng.nextInt(maxBurstLength) + 1
          if (!activeRanges.exists { case (es, ee) => es <= end && ee >= start }) {
            found = true
          }
        }
        val range = (start, end)
        activeRanges += range
        range
      }

      def unlockRange(range: (Int, Int)) = {
        activeRanges -= range
      }

      def makeReader(portNum: Int): () => Unit = {
        val port = dut.io.readPorts(portNum)
        var currentRange = (0, 0)
        var burstActive = false
        var currentAddr = 0
        var wordsRemaining = 0

        () => {
          port.valid.poke(false.B) // Default value
          if (burstActive) {
            val ready = rng.nextBoolean()
            port.data.ready.poke(ready.B)

            if (ready && port.data.valid.peek().litToBoolean) {
              val expectedData = reference(currentAddr)
              val actualData = port.data.bits.peek().litValue.toLong & 0xffffffffL
              assert(actualData == expectedData, f"Reader $portNum mismatched at addr $currentAddr")
              currentAddr += 1
              wordsRemaining -= 1

              if (wordsRemaining == 0) {
                burstActive = false
                unlockRange(currentRange)
              }
            }
          } else if (rng.nextInt(10) < 3) { // 30% chance to start new burst
            // Start new burst
            currentRange = getRandomRange()
            val len = currentRange._2 - currentRange._1 + 1
            currentAddr = currentRange._1
            wordsRemaining = len
            burstActive = true

            port.valid.poke(true.B)
            port.address.poke((currentAddr * 4).U)
            port.length.poke((len - 1).U)
          }
        }
      }

      var finishSimulation = false

      def makeWriter(portNum: Int): () => Unit = {
        val port = dut.io.writePorts(portNum)
        var currentRange = (0, 0)
        var burstActive = false
        var currentAddr = 0
        var wordsRemaining = 0

        () => {
          // Drive data transmission
          port.valid.poke(false.B)
          if (burstActive) {
            val valid = rng.nextBoolean() || finishSimulation
            port.data.valid.poke(valid.B)

            // We updated the data in the reference below when we started the burst.
            port.data.bits.poke(reference(currentAddr).U)

            if (valid && port.data.ready.peek().litToBoolean) {
              currentAddr += 1
              wordsRemaining -= 1

              if (wordsRemaining == 0) {
                burstActive = false
                unlockRange(currentRange)
              }
            }
          } else if (!finishSimulation && rng.nextInt(10) < 3) {
            currentRange = getRandomRange()
            val len = currentRange._2 - currentRange._1 + 1
            currentAddr = currentRange._1
            wordsRemaining = len
            burstActive = true

            // Write new data here. This range is locked, so the reader won't get the wrong
            // data until the burst finishes.
            for (_ <- currentRange._1 to currentRange._2) {
              //reference(i) = rng.nextLong() & 0xffffffffL
            }

            port.valid.poke(true.B)
            port.address.poke((currentAddr * 4).U)
            port.length.poke((len - 1).U)
          }
        }
      }

      val readers = (0 until numReaders).map(makeReader)
      val writers = (0 until numWriters).map(makeWriter)

      for (_ <- 0 until 5000) {
        readers.foreach(r => r())
        writers.foreach(w => w())
        dut.clock.step()
      }

      // Flush all pending writes so memory is consistent
      finishSimulation = true
      for (_ <- 0 until 200) {
        writers.foreach(w => w())
        dut.clock.step()
      }

      val finalMem = SimMemAccess.read(dut.clock, dut.io.dap, 0, memorySize)
      assert(finalMem == reference.toSeq)
    }
  }
}