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

/** This module select between hardware thread to issue to the pipeline. It also
  * tracks current runnable state of each thread and handles assigning new jobs to
  * threads.
  */
class ThreadScheduleStage(implicit val cfg: GpuConfig) extends Module {
  val io  = IO(new Bundle {
    // From external fixed function units.
    val startJob = Flipped(Decoupled(new Bundle {
      val startPc = UInt(cfg.busAddressBits.W)
    }))

    // Output to the instruction cache.
    val nextIssue = Valid(new Bundle {
      val threadId = UInt(log2Ceil(cfg.shaderThreads).W)
      val pc = UInt(cfg.busAddressBits.W)
    })

    val haltRequest = Flipped(Valid(UInt(log2Up(cfg.shaderThreads).W)))
    val wakeThreadBitmap = Input(UInt(cfg.shaderThreads.W))
    val stallThreadBitmap = Input(UInt(cfg.shaderThreads.W))

    // We an rollback a thread in response to a branch instruction or if
    // it stalls for some reason.
    val rollback = Flipped(Valid(new Bundle {
      val threadId = UInt(log2Ceil(cfg.shaderThreads).W)
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

  // The pipeline itself never stalls, but threads can stall if they are waiting for
  // external soruces. This manages the stall state for each thread.
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

  // Select the thread to issue.
  val threadIssueArbiter = Module(new RRArbiter(UInt(cfg.busAddressBits.W), cfg.shaderThreads))
  for (i <- 0 until cfg.shaderThreads) {
    threadIssueArbiter.io.in(i).valid := !threadHalted(i) && !threadStalled(i)
    threadIssueArbiter.io.in(i).bits := programCounters(i)
  }

  threadIssueArbiter.io.out.ready := true.B

  io.nextIssue.valid := (threadIssueArbiter.io.out.valid
    && !(io.stallThreadBitmap(threadIssueArbiter.io.chosen)))
  io.nextIssue.bits.threadId := threadIssueArbiter.io.chosen

  // Note: the rollback signal can end up being a critical timing path. An
  // alternative would be to set valid to false this cycle and skip issuing
  // the instruction, but should do timing analysis to see if this is
  // necessary.
  val nextIssuePc = Mux(io.rollback.valid && io.rollback.bits.threadId === threadIssueArbiter.io.chosen,
                        io.rollback.bits.pc,
                        programCounters(threadIssueArbiter.io.chosen))
  io.nextIssue.bits.pc := nextIssuePc

  // Rollback a thread to a previous PC.
  when (io.rollback.valid) {
    programCounters(io.rollback.bits.threadId) := io.rollback.bits.pc
  }

  // Advance the selected program counter.
  // NOTE: if the thread is rolled back the same cycle it is issued, this
  // should take precendence, since it factors in the increment.
  when (io.nextIssue.valid) {
    programCounters(threadIssueArbiter.io.chosen) := nextIssuePc + 4.U
  }
}
