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

package gpu.shader

import chisel3._
import chisel3.util._
import gpu._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funsuite.AnyFunSuite
import scala.util.Random

class ICacheFillTests extends AnyFunSuite with ChiselSim {
  implicit val cfg: GpuConfig = GpuConfig()

  test("ICacheFillUnit single miss") {
    simulate(new ICacheFillUnit()) { dut =>
      val baseAddress = 0x1000
      dut.io.fillRequest.valid.poke(true.B)
      dut.io.fillRequest.bits.address.raw.poke(baseAddress.U)
      dut.io.fillRequest.bits.thread.poke(2.U)
      dut.clock.step()
      dut.io.fillRequest.valid.poke(false.B)
      dut.io.readPort.burst.valid.expect(true.B, "Should request burst from memory")
      dut.io.readPort.burst.bits.address.expect(baseAddress.U,
        "Memory address should match the miss address")
      dut.clock.step()

      // Simulate memory returning data
      var offset = 0
      dut.io.readPort.data.valid.poke(true.B)
      while (offset < cfg.cacheLineSizeBytes / (cfg.busDataBits / 8)) {
        dut.io.readPort.burst.valid.expect(false.B, "Should not initiate new burst")
        dut.io.readPort.data.bits.poke((offset + 1).U)

        dut.io.updateCache.valid.peek().litToBoolean match {
          case true =>
            dut.io.updateCache.bits.address.raw.expect((baseAddress + offset * 8).U,
              "Cache address should match the miss address")
            // Check data
            val expectedData = (offset + 1).U
            dut.io.updateCache.bits.data.expect(expectedData, "Cache data should match the data returned from memory")
            if (offset == cfg.cacheLineSizeBytes / (cfg.busDataBits / 8) - 1) {
              dut.io.updateCache.bits.last.expect(true.B,
                "Cache update done should be asserted when the last beat is received")
              dut.io.wakeThreadBitmap.expect(4.U,
                "Wake thread bitmap should indicate the waiting thread")
            } else {
              dut.io.updateCache.bits.last.expect(false.B,
                "Cache update done should not be asserted until the last beat is received")
            }

            offset += 1
          case false =>
            // Cache update not yet asserted
        }

        dut.clock.step(1)
      }

      for (_ <- 0 until 5) {
        dut.clock.step(1)
        dut.io.wakeThreadBitmap.expect(0.U, "Wake thread bitmap should be cleared after waking threads")
        dut.io.updateCache.valid.expect(false.B, "Cache update should be deasserted after one cycle")
        dut.io.readPort.burst.valid.expect(false.B, "Should not request burst from memory")
      }
    }
  }

  // Miss on a cache line while there is already a burst in progress.
  test("ICacheFillUnit duplicate miss") {
    simulate(new ICacheFillUnit()) { dut =>
      val baseAddress = 0x1000
      dut.io.fillRequest.valid.poke(true.B)
      dut.io.fillRequest.bits.address.raw.poke(baseAddress.U)
      dut.io.fillRequest.bits.thread.poke(2.U)
      dut.clock.step()
      dut.io.readPort.burst.valid.expect(true.B, "Should request burst from memory")
      dut.io.readPort.burst.bits.address.expect(baseAddress.U,
        "Memory address should match the miss address")
      dut.clock.step()

      // Second miss collides with the first
      dut.io.fillRequest.bits.thread.poke(1.U)
      dut.clock.step()
      dut.io.fillRequest.valid.poke(false.B)

      // Simulate memory returning data
      var offset = 0
      dut.io.readPort.data.valid.poke(true.B)
      while (offset < cfg.cacheLineSizeBytes / (cfg.busDataBits / 8)) {
        dut.io.readPort.burst.valid.expect(false.B, "Should not initiate new burst")
        dut.io.readPort.data.bits.poke((offset + 1).U)

        dut.io.updateCache.valid.peek().litToBoolean match {
          case true =>
            dut.io.updateCache.bits.address.raw.expect((baseAddress + offset * 8).U,
              "Cache address should match the miss address")
            // Check data
            val expectedData = (offset + 1).U
            dut.io.updateCache.bits.data.expect(expectedData, "Cache data should match the data returned from memory")
            if (offset == cfg.cacheLineSizeBytes / (cfg.busDataBits / 8) - 1) {
              dut.io.updateCache.bits.last.expect(true.B,
                "Cache update done should be asserted when the last beat is received")
              dut.io.wakeThreadBitmap.expect(6.U,
                "Wake thread bitmap should indicate the waiting thread")
            } else {
              dut.io.updateCache.bits.last.expect(false.B,
                "Cache update done should not be asserted until the last beat is received")
            }

            offset += 1
          case false =>
            // Cache update not yet asserted
        }

        dut.clock.step(1)
      }

      for (_ <- 0 until 5) {
        dut.clock.step(1)
        dut.io.wakeThreadBitmap.expect(0.U, "Wake thread bitmap should be cleared after waking threads")
        dut.io.updateCache.valid.expect(false.B, "Cache update should be deasserted after one cycle")
        dut.io.readPort.burst.valid.expect(false.B, "Should not request burst from memory")
      }
    }
  }

  test("ICacheFillUnit random") {
    // 32 cache lines. We use 8 threads, so there is a 25% change of a collision each miss.
    val memorySize = 2048

    simulate(new Module {
      val io = IO(new Bundle {
        val dap = new DirectAccessPort
        val fillRequest = Flipped(Valid(new CacheFillRequest))
        val updateCache = Output(Valid(new CacheUpdateRequest))
        val wakeThreadBitmap = Output(UInt(cfg.shaderThreads.W))
      })

      val icacheFill = Module(new ICacheFillUnit())
      icacheFill.io.fillRequest := io.fillRequest

      io.updateCache := icacheFill.io.updateCache
      io.wakeThreadBitmap := icacheFill.io.wakeThreadBitmap

      val arbiter = Module(new MemoryArbiter(numReadPorts = 1, numWritePorts = 1))
      arbiter.io.readPorts(0) <> icacheFill.io.readPort
      arbiter.io.writePorts <> DontCare

      val memory  = Module(new SimAxiMemory(memorySize))
      arbiter.io.axiBus <> memory.io
      memory.dap <> io.dap
    }) { dut =>
      val reference = Array.tabulate(memorySize)(i => i.toLong)
      SimMemAccess.write(dut.clock, dut.io.dap, 0, reference.toSeq)

      dut.io.fillRequest.valid.poke(false.B)
      dut.io.fillRequest.bits.address.raw.poke(0.U)
      dut.io.fillRequest.bits.thread.poke(0.U)

      val rng = new Random(42)

      class MissState(
        var active: Boolean = false,
        var address: Int = 0,
        var issueCycle: Int = 0,
      )

      val missState = Array.fill(cfg.shaderThreads)(new MissState)
      val maxLatency = 200

      val totalCycles = 100000
      for (cycle <- 0 until totalCycles) {
        // Don't start a new burst during the flush period
        if (rng.nextDouble() < 0.1 && cycle < totalCycles - maxLatency) {
          val threadIdx = rng.nextInt(cfg.shaderThreads)
          val state = missState(threadIdx)
          if (!state.active) {
            val address = rng.nextInt(memorySize / cfg.cacheLineSizeBytes) * cfg.cacheLineSizeBytes
            // Don't start a new transaction if the cache fill is finishing for
            // the same line (cache near miss)
            if (dut.io.updateCache.valid.peek().litToBoolean &&
                (dut.io.updateCache.bits.address.peek().litValue.toInt & ~(cfg.cacheLineSizeBytes - 1)) == address) {
              // Skip this miss, it would be a near miss
            } else {
              state.active = true
              state.address = address
              state.issueCycle = cycle
              dut.io.fillRequest.valid.poke(true.B)
              dut.io.fillRequest.bits.address.raw.poke(address.U)
              dut.io.fillRequest.bits.thread.poke(threadIdx.U)
            }
          } else {
            dut.io.fillRequest.valid.poke(false.B)
          }
        } else {
          dut.io.fillRequest.valid.poke(false.B)
        }

        if (dut.io.updateCache.valid.peek().litToBoolean) {
          val address = dut.io.updateCache.bits.address.peek().litValue.toInt
          val data = dut.io.updateCache.bits.data.peek().litValue.toLong
          val expectedData = reference(address / 8)
          assert(data == expectedData, s"Data mismatch at address $address: expected $expectedData, got $data")
        }

        val wakeBitmap = dut.io.wakeThreadBitmap.peek().litValue.toInt
        if (wakeBitmap != 0) {
          for (thread <- 0 until cfg.shaderThreads) {
            if ((wakeBitmap & (1 << thread)) != 0) {
              val state = missState(thread)
              assert(state.active, s"Cycle $cycle: Thread $thread was woken but has no outstanding miss")
              state.active = false
            }
          }
        }

        for (thread <- 0 until cfg.shaderThreads) {
          val state = missState(thread)
          if (state.active) {
            val waiting = cycle - state.issueCycle
            assert(waiting < maxLatency,
              s"Thread $thread has been waiting $waiting cycles for miss on address ${state.address} (issued cycle ${state.issueCycle})")
          }
        }

        dut.clock.step()
      }

      // Final check: everything should have drained within the timeout
      for (thread <- 0 until cfg.shaderThreads) {
        assert(!missState(thread).active,
          s"Thread $thread still has outstanding miss after flush " +
          s"(address ${missState(thread).address}, issued cycle ${missState(thread).issueCycle})")
      }
    }
  }
}

