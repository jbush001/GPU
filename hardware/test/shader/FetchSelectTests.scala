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

  def startJob(dut: FetchSelectStage, startPc: UInt, jobId: UInt = 1.U): Int = {
    dut.io.startJob.ready.expect(true.B)
    dut.io.startJob.valid.poke(true.B)
    dut.io.startJob.bits.startPc.poke(startPc)
    dut.io.startJob.bits.jobId.poke(jobId)
    dut.clock.step()
    dut.io.startJob.valid.poke(false.B)

    // Record the thread that was allocated.
    val allocatedThread = dut.io.fetchRequest.bits.thread.peek().litValue
    allocatedThread.toInt
  }

  // Should equal rawLatency in FetchSelectStage.scala.
  val backToBackLatency = 4

  test("FetchSelectStage single thread") {
    simulate(new FetchSelectStage()) { dut =>
      val allocatedThread = startJob(dut, 0x1000.U)
      // Issue a few more fetch requests
      for (i <- 1 until 5) {
        // Wait for RAW delay
        for (_ <- 0 until backToBackLatency) {
          dut.clock.step()
          dut.io.fetchRequest.valid.expect(false.B)
        }

        dut.clock.step()
        dut.io.fetchRequest.valid.expect(true.B)
        assert(dut.io.fetchRequest.valid.peek().litToBoolean, "Fetch request should be valid")
        dut.io.fetchRequest.bits.thread.expect(allocatedThread.U)
        dut.io.fetchRequest.bits.pc.raw.expect((0x1000 + (i * 4)).U)
      }
    }
  }

  // Ensure everything works properly when the unit is entirely idle.
  test("FetchSelectStage no threads ready") {
    simulate(new FetchSelectStage()) { dut =>
      dut.io.startJob.valid.poke(false.B)
      for (_ <- 0 until 5) {
        dut.io.fetchRequest.valid.expect(false.B)
        dut.clock.step()
      }
    }
  }

  test("FetchSelectStage rollback") {
    simulate(new FetchSelectStage()) { dut =>
      val allocatedThread = startJob(dut, 0x2000.U)

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

  test("FetchSelectStage icache miss") {
    simulate(new FetchSelectStage()) { dut =>
      val allocatedThread = startJob(dut, 0x1000.U)

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
      dut.io.icacheWakeThreads.poke((1 << allocatedThread.toInt).U)
      dut.clock.step()
      dut.io.icacheWakeThreads.poke(0.U)

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
      val allocatedThread = startJob(dut, 0x1000.U)

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

  test("FetchSelectStage io wait") {
    // Similar to icache miss, except uses ioWait
    simulate(new FetchSelectStage()) { dut =>
      var pc2 = 0x1000
      val thread1 = startJob(dut, 0x2000.U, 1.U)
      val thread2 = startJob(dut, pc2.U, 2.U)

      // Issue a few cycles
      for (_ <- 1 until 20) {
        if (dut.io.fetchRequest.valid.peek().litToBoolean) {
          val fetchThread = dut.io.fetchRequest.bits.thread.peek().litValue.toInt
          val fetchPc = dut.io.fetchRequest.bits.pc.raw.peek().litValue.toInt
          if (fetchThread == thread2) {
            assert(fetchPc == pc2)
            pc2 = fetchPc + 4
          }
        }

        dut.clock.step()
      }

      // Pause thread 2
      dut.clock.step()
      dut.io.ioWaitThread.valid.poke(true.B)
      dut.io.ioWaitThread.bits.poke(thread2.U)
      dut.clock.step()
      dut.io.ioWaitThread.valid.poke(false.B)
      pc2 -= 4

      // Ensure thread 1 continues to issue
      // (Need to wait at least 5 cycles, given that's the back-to-back latency,
      // double it to be certain)
      var thread1Issued = false
      for (_ <- 1 until 20) {
        if (dut.io.fetchRequest.valid.peek().litToBoolean) {
          val fetchThread = dut.io.fetchRequest.bits.thread.peek().litValue.toInt
          assert(fetchThread == thread1, s"Invalid thread $fetchThread issued")
          thread1Issued = true
        }

        dut.clock.step()
      }

      assert(thread1Issued, "Thread 1 did not issue during io wait")

      // Resume thread 2
      dut.io.ioWakeJob.valid.poke(true.B)
      dut.io.ioWakeJob.bits.poke(2.U)
      dut.clock.step()
      dut.io.ioWakeJob.valid.poke(false.B)

      // Ensure thread 1 resumes issuing and thread 2 continues
      // Also check that the PC is properly rolled back for thread 1.
      thread1Issued = false
      var thread2Issued = false
      for (_ <- 1 until 20) {
        if (dut.io.fetchRequest.valid.peek().litToBoolean) {
          val fetchThread = dut.io.fetchRequest.bits.thread.peek().litValue.toInt
          val fetchPc = dut.io.fetchRequest.bits.pc.raw.peek().litValue.toInt
          if (fetchThread == thread2) {
            assert(fetchPc == pc2)
            pc2 = fetchPc + 4
            thread2Issued = true
          } else if (fetchThread == thread1) {
            thread1Issued = true
          }
        }

        dut.clock.step()
      }

      assert(thread1Issued, "Thread 1 did not resume issuing")
      assert(thread2Issued, "Thread 2 did not continue issuing")
    }
  }

  test("FetchSelectStage halt") {
    simulate(new FetchSelectStage()) { dut =>
      val allocatedThread = startJob(dut, 0x1000.U)

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
      dut.io.halt.valid.poke(true.B)
      dut.io.halt.bits.poke(allocatedThread.U)
      dut.clock.step()
      dut.io.halt.valid.poke(false.B)

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
      val numThreads = 8
      (0 until numThreads).map(i => startJob(dut, ((i + 1) * 0x1000).U))

      // Collect the sequence of issuing thread IDs over consecutive cycles
      val issuedSequence = (0 until numThreads).map { _ =>
        dut.io.fetchRequest.valid.expect(true.B)
        val tId = dut.io.fetchRequest.bits.thread.peek().litValue.toInt
        dut.clock.step()
        tId
      }

      // Verify all threads issued exactly once without repeating
      assert(issuedSequence.distinct.length == numThreads,
             "Did not see all threads")

      // Verify the round-robin cycle repeats in the same order for a few more cycles
      for (_ <- 0 until numThreads * 2) {
        for (expectedThread <- issuedSequence) {
          dut.io.fetchRequest.valid.expect(true.B)
          dut.io.fetchRequest.bits.thread.expect(expectedThread.U)
          dut.clock.step()
        }
      }
    }
  }

  test("FetchSelectStage halt does not affect other threads") {
    simulate(new FetchSelectStage()) { dut =>
      val allocatedThreads = (0 until 8).map(i => startJob(dut, (0x1000 + i * 0x1000).U))

      // Issue a few cycles to ensure all threads are active
      for (_ <- 0 until 20) {
        dut.io.fetchRequest.valid.expect(true.B)
        dut.clock.step()
      }

      // Halt one of the threads
      val threadToHalt = allocatedThreads.head
      dut.io.halt.valid.poke(true.B)
      dut.io.halt.bits.poke(threadToHalt.U)
      dut.clock.step()
      dut.io.halt.valid.poke(false.B)

      // Ensure the halted thread does not issue anymore, but others continue
      for (_ <- 0 until 8) {
        dut.io.fetchRequest.valid.expect(true.B)
        val currentThread = dut.io.fetchRequest.bits.thread.peek().litValue.toInt
        assert(currentThread != threadToHalt, "Halted thread should not issue")
        dut.clock.step()
      }
    }
  }

  test("FetchSelectStage one thread stalled does not block others") {
    simulate(new FetchSelectStage()) { dut =>
      val allocatedThreads = (0 until 8).map(i => startJob(dut, (0x1000 + i * 0x1000).U))

      // Issue a few cycles
      for (_ <- 0 until 20) {
        dut.io.fetchRequest.valid.expect(true.B)
        dut.clock.step()
      }

      // Stall one of the threads
      val stalledThread = allocatedThreads.head
      dut.io.icacheMiss.poke(true.B)
      dut.io.icacheMissThread.poke(stalledThread.U)
      dut.clock.step()
      dut.io.icacheMiss.poke(false.B)

      // Ensure the stalled thread does not issue anymore, but others continue
      for (_ <- 0 until 8) {
        dut.io.fetchRequest.valid.expect(true.B)
        val currentThread = dut.io.fetchRequest.bits.thread.peek().litValue.toInt
        assert(currentThread != stalledThread, "Stalled thread should not issue")
        dut.clock.step()
      }

      // Resume the stalled thread
      dut.io.icacheWakeThreads.poke((1 << stalledThread).U)
      dut.clock.step()
      dut.io.icacheWakeThreads.poke(0.U)

      // Ensure the previously stalled thread can now issue again
      var foundStalledThread = false
      for (_ <- 0 until 8) {
        dut.io.fetchRequest.valid.expect(true.B)
        val currentThread = dut.io.fetchRequest.bits.thread.peek().litValue.toInt
        if (currentThread == stalledThread) {
          foundStalledThread = true
        }
        dut.clock.step()
      }
      assert(foundStalledThread, "Previously stalled thread did not issue after being resumed")
    }
   }

  test("FetchSelectStage startJob deasserts ready when no threads are free") {
    simulate(new FetchSelectStage()) { dut =>
      // Start jobs on all available threads
      val numThreads = 8
      for (i <- 0 until numThreads) {
        dut.io.startJob.ready.expect(true.B, "startJob should be ready")
        startJob(dut, (0x1000 + i * 0x1000).U)
      }

      // Now, all threads are allocated. The next startJob should not be ready.
      dut.io.startJob.ready.expect(false.B)
    }
  }
}
