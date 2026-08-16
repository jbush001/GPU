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

import gpu._

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funsuite.AnyFunSuite

class FetchSelectTests extends AnyFunSuite with ChiselSim {
  implicit val cfg: GpuConfig = new GpuConfig

  // Should equal rawLatency in FetchSelectStage.scala.
  val backToBackLatency = 2

  test("FetchSelectStage single thread") {
    simulate(new FetchSelectStage()) { dut =>
      // Allocate a new job
      dut.io.startJob.ready.expect(true.B)
      dut.io.startJob.valid.poke(true.B)
      dut.io.startJob.bits.startPc.poke(0x1000.U)
      dut.clock.step()
      dut.io.startJob.valid.poke(false.B)

      // Record the thread that was allocated.
      val allocatedThread = dut.io.fetchRequest.bits.thread.peek().litValue

      // Issue a few more fetch requests
      for (i <- 1 until 5) {
        // Wait for RAW delay
        for (_ <- 0 until backToBackLatency) {
          dut.clock.step()
          dut.io.fetchRequest.valid.expect(false.B)
          dut.io.resetThread.valid.expect(false.B)
        }

        dut.clock.step()
        dut.io.resetThread.valid.expect(false.B)
        dut.io.fetchRequest.valid.expect(true.B)
        assert(dut.io.fetchRequest.valid.peek().litToBoolean, "Fetch request should be valid")
        dut.io.fetchRequest.bits.thread.expect(allocatedThread.U)
        dut.io.fetchRequest.bits.pc.raw.expect((0x1000 + (i * 4)).U)
      }
    }
  }

  // Ensure this resets the thread state appropriately.
  test("FetchSelectStage thread reset") {
    simulate(new FetchSelectStage()) { dut =>
      // Allocate a new job
      dut.io.startJob.ready.expect(true.B)
      dut.io.startJob.valid.poke(true.B)
      dut.io.startJob.bits.startPc.poke(0x1000.U)
      dut.io.resetThread.valid.expect(true.B)
      val resetThread = dut.io.resetThread.bits.peek().litValue
      dut.clock.step()
      dut.io.startJob.valid.poke(false.B)

      // Record the thread that was allocated.
      val allocatedThread = dut.io.fetchRequest.bits.thread.peek().litValue
      assert(allocatedThread == resetThread, s"Allocated thread $allocatedThread does not match reset thread $resetThread")
    }
  }


  test("FetchSelectStage rollback") {
    simulate(new FetchSelectStage()) { dut =>
      // Start a job at 0x2000
      dut.io.startJob.valid.poke(true.B)
      dut.io.startJob.bits.startPc.poke(0x2000.U)
      dut.clock.step()
      dut.io.startJob.valid.poke(false.B)

      // Record the thread that was allocated.
      val allocatedThread = dut.io.fetchRequest.bits.thread.peek().litValue

      // Issue a few cycles
      for (i <- 1 until 3) {
        for (_ <- 0 until backToBackLatency) {
          dut.clock.step()
          dut.io.fetchRequest.valid.expect(false.B)
        }

        dut.clock.step()
        dut.io.fetchRequest.valid.expect(true.B)
        dut.io.fetchRequest.bits.thread.expect(allocatedThread.U)
        dut.io.fetchRequest.bits.pc.raw.expect((0x2000 + (i * 4)).U)
      }

      dut.clock.step()

      // Trigger a rollback for the issued thread to 0x0500
      dut.io.rollback.valid.poke(true.B)
      dut.io.rollback.bits.thread.poke(allocatedThread.U)
      dut.io.rollback.bits.pc.poke(0x0500.U)

      // Ensure we're executing from the new location
      for (i <- 0 until 3) {
        dut.clock.step()
        dut.io.rollback.valid.poke(false.B)
        dut.io.fetchRequest.valid.expect(true.B)
        dut.io.fetchRequest.bits.thread.expect(allocatedThread.U)
        dut.io.fetchRequest.bits.pc.raw.expect((0x500 + (i * 4)).U)

        for (_ <- 0 until backToBackLatency) {
          dut.clock.step()
          dut.io.fetchRequest.valid.expect(false.B)
          dut.io.rollback.valid.poke(false.B)
        }
      }
    }
  }

  test("FetchSelectStage stall/resume") {
    simulate(new FetchSelectStage()) { dut =>
      // Start job
      dut.io.startJob.valid.poke(true.B)
      dut.io.startJob.bits.startPc.poke(0x1000.U)
      dut.clock.step()
      dut.io.startJob.valid.poke(false.B)

      // Record the thread that was allocated.
      val allocatedThread = dut.io.fetchRequest.bits.thread.peek().litValue

      // Issue a few cycles
      for (i <- 1 until 8) {
        for (_ <- 0 until backToBackLatency) {
          dut.clock.step()
          dut.io.fetchRequest.valid.expect(false.B)
        }

        dut.clock.step()
        dut.io.fetchRequest.valid.expect(true.B)
        dut.io.fetchRequest.bits.thread.expect(allocatedThread.U)
        dut.io.fetchRequest.bits.pc.raw.expect((0x1000 + (i * 4)).U)
      }

      // Stall Thread on icache miss
      dut.io.icacheMiss.poke(true.B)
      dut.io.icacheMissThread.poke(allocatedThread.U)

      // Thread should no longer issue
      for (_ <- 0 until 3) {
        dut.clock.step()
        dut.io.fetchRequest.valid.expect(false.B)
        dut.io.icacheMiss.poke(false.B)
      }

      // Resume Thread
      dut.io.wakeThreadBitmap.poke((1 << allocatedThread.toInt).U)
      dut.clock.step()
      dut.io.wakeThreadBitmap.poke(0.U)

      // Should resume issuing from the prior location.
      for (i <- 7 until 12) {
        for (_ <- 0 until backToBackLatency) {
          dut.clock.step()
          dut.io.fetchRequest.valid.expect(false.B)
        }

        dut.clock.step()
        dut.io.fetchRequest.valid.expect(true.B)
        dut.io.fetchRequest.bits.thread.expect(allocatedThread.U)
        dut.io.fetchRequest.bits.pc.raw.expect((0x1000 + (i * 4)).U)
      }
    }
  }

  test("FetchSelectStage near miss") {
    simulate(new FetchSelectStage()) { dut =>
      // Start job
      dut.io.startJob.valid.poke(true.B)
      dut.io.startJob.bits.startPc.poke(0x1000.U)
      dut.clock.step()
      dut.io.startJob.valid.poke(false.B)

      // Record the thread that was allocated.
      val allocatedThread = dut.io.fetchRequest.bits.thread.peek().litValue

      // Issue a few cycles
      for (i <- 1 until 8) {
        for (_ <- 0 until backToBackLatency) {
          dut.clock.step()
          dut.io.fetchRequest.valid.expect(false.B)
        }

        dut.clock.step()
        if (i == 7) {
          // The last instruction should be a near miss, which will not stall the thread.
          dut.io.icacheNearMiss.poke(true.B)
          dut.io.icacheMissThread.poke(allocatedThread.U)
          dut.io.fetchRequest.valid.expect(false.B)
        } else {
          dut.io.fetchRequest.valid.expect(true.B)
          dut.io.fetchRequest.bits.thread.expect(allocatedThread.U)
          dut.io.fetchRequest.bits.pc.raw.expect((0x1000 + (i * 4)).U)
        }
      }

      dut.clock.step()
      dut.io.icacheNearMiss.poke(false.B)

      // Should resume issuing from the prior location.
      for (i <- 7 until 12) {
        dut.io.fetchRequest.valid.expect(true.B)
        dut.io.fetchRequest.bits.thread.expect(allocatedThread.U)
        dut.io.fetchRequest.bits.pc.raw.expect((0x1000 + (i * 4)).U)

        for (_ <- 0 until backToBackLatency) {
          dut.clock.step()
          dut.io.icacheNearMiss.poke(false.B)
          dut.io.fetchRequest.valid.expect(false.B)
        }

        dut.clock.step()
      }
    }
  }

  test("FetchSelectStage halt") {
    simulate(new FetchSelectStage()) { dut =>
      // Start job on Thread 0
      dut.io.startJob.valid.poke(true.B)
      dut.io.startJob.bits.startPc.poke(0x1000.U)
      dut.clock.step()
      dut.io.startJob.valid.poke(false.B)

      // Record the thread that was allocated.
      val allocatedThread = dut.io.fetchRequest.bits.thread.peek().litValue

      // Issue a few cycles
      for (i <- 1 until 3) {
        for (_ <- 0 until backToBackLatency) {
          dut.clock.step()
          dut.io.fetchRequest.valid.expect(false.B)
        }

        dut.clock.step()
        dut.io.fetchRequest.valid.expect(true.B)
        dut.io.fetchRequest.bits.thread.expect(allocatedThread.U)
        dut.io.fetchRequest.bits.pc.raw.expect((0x1000 + (i * 4)).U)
      }

      // Halt Thread
      dut.io.haltRequest.valid.poke(true.B)
      dut.io.haltRequest.bits.poke(allocatedThread.U)
      dut.clock.step()
      dut.io.haltRequest.valid.poke(false.B)

      // Thread should no longer issue
      for (_ <- 0 until 3) {
        dut.io.fetchRequest.valid.expect(false.B)
        dut.clock.step()
      }
    }
  }

  test("FetchSelectStage multiple thread") {
    simulate(new FetchSelectStage()) { dut =>
      // Start jobs on 4 threads
      val numThreads = 4
      val startPcs = Seq(0x1000.U, 0x2000.U, 0x3000.U, 0x4000.U)

      for (pc <- startPcs) {
        dut.io.startJob.ready.expect(true.B)
        dut.io.startJob.valid.poke(true.B)
        dut.io.startJob.bits.startPc.poke(pc)
        dut.clock.step()
      }

      dut.io.startJob.valid.poke(false.B)

      // Collect the sequence of issuing thread IDs over consecutive cycles
      val issuedSequence = (0 until numThreads).map { _ =>
        dut.io.fetchRequest.valid.expect(true.B)
        val tId = dut.io.fetchRequest.bits.thread.peek().litValue.toInt
        dut.clock.step()
        tId
      }

      // Verify all 4 threads issued exactly once without repeating
      assert(issuedSequence.distinct.length == numThreads,
             s"Expected $numThreads distinct threads, got $issuedSequence")

      // Verify the round-robin cycle repeats in the same order for a few more cycles
      for (_ <- 0 until 3) {
        for (expectedThread <- issuedSequence) {
          dut.io.fetchRequest.valid.expect(true.B)
          dut.io.fetchRequest.bits.thread.expect(expectedThread.U)
          dut.clock.step()
        }
      }
    }
  }
}



