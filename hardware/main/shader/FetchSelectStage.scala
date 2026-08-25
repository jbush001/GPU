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
  * - Allocating/deallocating threads for jobs.
  * - Maintaining the program counter for each thread.
  * - Selecting which thread to issue to the instruction cache each cycle.
  * - Suspending/resuming threads that are waiting on instruction cache misses.
  * - Handling rollbacks for branches or other blocking conditions.
  */
class FetchSelectStage(implicit val cfg: GpuConfig) extends Module {
  val io  = IO(new Bundle {
    // From external fixed function units. Request to start a new shader job.
    val startJob = Flipped(Decoupled(new Bundle {
      val startPc = UInt(cfg.busAddressBits.W)
      val tag = UInt(cfg.shaderTagBits.W)
    }))

    // To InstructionDecodeStage. Initialize state when a new job starts.
    val resetThread = Valid(new Bundle {
      val thread = UInt(log2Up(cfg.shaderThreads).W)
      val tag = UInt(cfg.shaderTagBits.W)
    })

    // To InstructionFetchStage. Request an instruction fetch for a thread.
    val fetchRequest = Valid(new FetchRequest)

    // From ExecuteStage.
    val halt = Flipped(Valid(UInt(log2Up(cfg.shaderThreads).W)))

    // From ICacheFillUnit. Wake up threads that were stalled on an
    // instruction cache miss.
    val wakeThreads = Input(UInt(cfg.shaderThreads.W))

    // From InstructionFetchStage. Indicate that a thread is stalled on an
    // instruction cache miss.
    val icacheMiss = Input(Bool())
    val icacheNearMiss = Input(Bool())
    val icacheMissThread = Input(UInt(log2Up(cfg.shaderThreads).W))

    // From ExecuteStage. Rollback a thread in response to a branch
    // instruction.
    val rollback = Flipped(Valid(new Bundle {
      val thread = UInt(log2Ceil(cfg.shaderThreads).W)
      val pc = UInt(cfg.busAddressBits.W)
    }))
  })

  val programCounters = RegInit(VecInit(Seq.fill(cfg.shaderThreads)(0.U(cfg.busAddressBits.W))))
  val threadHalted = RegInit(VecInit(Seq.fill(cfg.shaderThreads)(true.B)))
  val threadStalled = RegInit(VecInit(Seq.fill(cfg.shaderThreads)(false.B)))

  // Threads start upon request and run to completion, stopping when they
  // reach a HALT instruction. This logic tracks which threads are active
  // and assigns new threads on request. This unit can only start one new
  // thread per cycle.
  val nextFreeThread = PriorityEncoder(threadHalted.asUInt)
  io.startJob.ready := threadHalted.asUInt.orR
  when (io.startJob.fire) {
    threadHalted(nextFreeThread) := false.B
    programCounters(nextFreeThread) := io.startJob.bits.startPc
    io.resetThread.valid := true.B
  } .otherwise {
    io.resetThread.valid := false.B
  }

  io.resetThread.bits.thread := nextFreeThread
  io.resetThread.bits.tag := io.startJob.bits.tag

  when (io.halt.valid) {
    assert(!threadHalted(io.halt.bits), "Cannot halt a thread that is already halted")
    threadHalted(io.halt.bits) := true.B
  }

  // This handles stalling threads that are waiting on instruction cache misses.
  for (thid <- 0 until cfg.shaderThreads) {
    assert(!(io.wakeThreads(thid) && io.icacheMiss && io.icacheMissThread === thid.U),
      "Cannot wake and stall a thread at the same time")
    assert(!(threadStalled(thid) && io.icacheMiss && io.icacheMissThread === thid.U),
      "Cannot stall a thread that is already stalled")
    assert(threadStalled(thid) || !io.wakeThreads(thid),
      "Cannot wake a thread that is not stalled")
    assert(!threadHalted(thid) || !(io.icacheMiss && io.icacheMissThread === thid.U),
      "Cannot stall a thread that is halted")

    // TODO There is actually an edge case where this can happen: if an instruction cache miss occurs
    // while fetching the next instruction and a previously fetched instruction is HALT, the
    // wakeup can occur later. Need to handle this case explicitly.
    assert(!threadHalted(thid) || !io.wakeThreads(thid), "Cannot wake a thread that is halted")

    when (io.wakeThreads(thid)) {
      threadStalled(thid) := false.B
    }

    when (io.icacheMiss && io.icacheMissThread === thid.U) {
      threadStalled(thid) := true.B
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
  // TODO an alternate approach would be to make the virtual vector register width
  // wider than the number of physical execution units and issue the same
  // instruction multiple times with a "chime" index.
  val rawLatency = 4 // Will issue every nth cycle, where n = rawLatency + 1
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
  for (thid <- 0 until cfg.shaderThreads) {
    threadIssueArbiter.io.in(thid).valid := (!threadHalted(thid) && !threadStalled(thid) && !inRawWait(thid)
      && !(io.rollback.valid && io.rollback.bits.thread === thid.U)
      && !((io.icacheMiss || io.icacheNearMiss) && io.icacheMissThread === thid.U))
    threadIssueArbiter.io.in(thid).bits := programCounters(thid)
  }

  threadIssueArbiter.io.out.ready := true.B

  io.fetchRequest.valid := threadIssueArbiter.io.out.valid && !(io.halt.valid && io.halt.bits === threadIssueArbiter.io.chosen)
  io.fetchRequest.bits.thread := threadIssueArbiter.io.chosen
  io.fetchRequest.bits.pc.raw := programCounters(threadIssueArbiter.io.chosen)

  // Program counter update logic.
  for (thid <- 0 until cfg.shaderThreads) {
    when (io.rollback.valid && io.rollback.bits.thread === thid.U) {
      // Rollback a thread, due to a branch or other blocking condition.
      programCounters(thid) := io.rollback.bits.pc
    }.elsewhen (io.icacheMiss && io.icacheMissThread === thid.U) {
      // Back up to previously missed instruction
      programCounters(thid) := programCounters(thid) - 4.U
    }.elsewhen (io.fetchRequest.valid && io.fetchRequest.bits.thread === thid.U) {
      // Advance the selected program counter. Note, because of the thread ready logic,
      // this will never occur when one of the above conditions is true.
      programCounters(thid) := programCounters(thid) + 4.U
    }
  }
}
