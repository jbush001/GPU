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

/**
  * This is the first stage in the instruction pipeline, responsible for:
  * - Maintaining the program counter for each thread.
  * - Selecting which address to send to the instruction cache each cycle.
  * - Allocating/deallocating threads for jobs.
  * - Suspending threads that are waiting on instruction cache misses.
  * - Handling rollbacks for branches or other blocking conditions.
  */
class FetchSelectStage(implicit val cfg: GpuConfig) extends Module {
  val io  = IO(new Bundle {
    // From external fixed function units.
    val startJob = Flipped(Decoupled(new Bundle {
      val startPc = UInt(cfg.busAddressBits.W)
    }))

    // Output to the instruction cache.
    val fetchRequest = Valid(new FetchRequest)

    val haltRequest = Flipped(Valid(UInt(log2Up(cfg.shaderThreads).W)))
    val wakeThreadBitmap = Input(UInt(cfg.shaderThreads.W))
    val stallThreadBitmap = Input(UInt(cfg.shaderThreads.W))

    // We an rollback a thread in response to a branch instruction or if
    // it stalls for some reason.
    val rollback = Flipped(Valid(new Bundle {
      val thread = UInt(log2Ceil(cfg.shaderThreads).W)
      val pc = UInt(cfg.busAddressBits.W)
    }))
  })

  val programCounters = RegInit(VecInit(Seq.fill(cfg.shaderThreads)(0.U(cfg.busAddressBits.W))))
  val threadHalted = RegInit(VecInit(Seq.fill(cfg.shaderThreads)(true.B)))
  val threadStalled = RegInit(VecInit(Seq.fill(cfg.shaderThreads)(false.B)))

  // Threads start upon request and run to completion, halting when they
  // reach a HALT instruction. This logic tracks which threads are active
  // and assigns new threads on request. This unit can only start one new
  // thread per cycle.
  val nextFreeThread = PriorityEncoder(threadHalted.asUInt)
  io.startJob.ready := threadHalted.asUInt.orR
  when (io.startJob.fire) {
    threadHalted(nextFreeThread) := false.B
    programCounters(nextFreeThread) := io.startJob.bits.startPc
  }

  when (io.haltRequest.valid) {
    assert(!threadHalted(io.haltRequest.bits), "Cannot halt a thread that is already halted")
    threadHalted(io.haltRequest.bits) := true.B
  }

  // This handles stalling threads that are waiting on instruction cache misses.
  for (i <- 0 until cfg.shaderThreads) {
    assert(!(io.wakeThreadBitmap(i) && io.stallThreadBitmap(i)), "Cannot wake and stall a thread at the same time")
    assert(!threadStalled(i) || !io.stallThreadBitmap(i), "Cannot stall a thread that is already stalled")
    assert(threadStalled(i) || !io.wakeThreadBitmap(i), "Cannot wake a thread that is not stalled")
    assert(!threadHalted(i) || !io.stallThreadBitmap(i), "Cannot stall a thread that is halted")

    // TODO There is actually an edge case where this can happen: if an instruction cache miss occurs
    // while fetching the next instruction and a previously fetched instruction is HALT, the
    // wakeup can occur later. Need to handle this case explicitly.
    assert(!threadHalted(i) || !io.wakeThreadBitmap(i), "Cannot wake a thread that is halted")

    when (io.wakeThreadBitmap(i)) {
      threadStalled(i) := false.B
    }

    when (io.stallThreadBitmap(i)) {
      threadStalled(i) := true.B
    }
  }

  // We don't issue instructions from the same thread more than once every
  // n cycles to avoid potential raw hazards in the execute stages.
  // e.g.
  //
  //    add r1, r2, r3
  //    add r4, r1, r5
  //
  // The second instruction reads r1 before the first instruction writes it.
  // Not every instruction will have this hazard, but we enforce a 3-cycle delay
  // to simplify the pipeline.
  val rawLatency = 2 // Will issue every nth cycle, where n = rawLatency + 1
  val issueRawDelay = RegInit(VecInit(Seq.fill(cfg.shaderThreads)(0.U(3.W))))
  val inRawWait = Wire(Vec(cfg.shaderThreads, Bool()))
  for (i <- 0 until cfg.shaderThreads) {
    when (io.rollback.valid && io.rollback.bits.thread === i.U) {
      // If a thread is rolled back, it is safe to issue the instruction again
      // immediately, since the previous instruction was not executed.
      issueRawDelay(i) := 0.U
    } .elsewhen (io.fetchRequest.valid && io.fetchRequest.bits.thread === i.U) {
      // Issue an instruction on this thread
      assert(issueRawDelay(i) === 0.U, "Cannot issue an instruction on a thread that is still in RAW wait")
      issueRawDelay(i) := rawLatency.U
    } .elsewhen (issueRawDelay(i) =/= 0.U) {
      issueRawDelay(i) := issueRawDelay(i) - 1.U
    }

    inRawWait(i) := issueRawDelay(i) =/= 0.U
  }

  // Select the thread to issue.
  val threadIssueArbiter = Module(new RRArbiter(UInt(cfg.busAddressBits.W), cfg.shaderThreads))
  for (i <- 0 until cfg.shaderThreads) {
    threadIssueArbiter.io.in(i).valid := !threadHalted(i) && !threadStalled(i) && !inRawWait(i)
    threadIssueArbiter.io.in(i).bits := programCounters(i)
  }

  threadIssueArbiter.io.out.ready := true.B

  io.fetchRequest.valid := (threadIssueArbiter.io.out.valid
    && !(io.stallThreadBitmap(threadIssueArbiter.io.chosen)))
  io.fetchRequest.bits.thread := threadIssueArbiter.io.chosen

  // Note: the rollback signal can end up being a critical timing path. An
  // alternative would be to set valid to false this cycle and skip issuing
  // the instruction, but should do timing analysis to see if this is
  // necessary.
  val nextIssuePc = Mux(io.rollback.valid && io.rollback.bits.thread === threadIssueArbiter.io.chosen,
                        io.rollback.bits.pc,
                        programCounters(threadIssueArbiter.io.chosen))
  io.fetchRequest.bits.pc.raw := nextIssuePc

  // Rollback a thread to a previous PC.
  when (io.rollback.valid) {
    programCounters(io.rollback.bits.thread) := io.rollback.bits.pc
  }

  // Advance the selected program counter.
  // NOTE: if the thread is rolled back the same cycle it is issued, this
  // should take precendence, since it factors in the increment.
  when (io.fetchRequest.valid) {
    programCounters(threadIssueArbiter.io.chosen) := nextIssuePc + 4.U
  }
}
