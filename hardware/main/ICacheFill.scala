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

/** Handles reading cache lines from external memory and queuing pending misses.
  */
class ICacheFill(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    // To memory arbiter
    val readPort = new MemReadPort

    // From instruction fetch. Enqueue a new miss request.
    val cacheMiss = Input(Bool())
    val missAddress = Input(UInt(cfg.busAddressBits.W))
    val missThread = Input(UInt(log2Up(cfg.shaderHarts).W))

    // To instruction fetch. Write data back to the cache.
    val updateCacheEn = Output(Bool())
    val updateCacheAddress = Output(UInt(cfg.busAddressBits.W))
    val updateCacheData = Output(UInt((cfg.busDataBits).W))
    val updateCacheDone = Output(Bool())

    // Each bit corresponds to a hardware thread that should
    // be woken up because its cache miss has been filled.
    val wakeThreadBitmap = Output(UInt(cfg.shaderHarts.W))
  })

  def cacheLineAlign(address: UInt): UInt = Cat(address(31, 6), 0.U(6.W))

  // There is one pending miss entry per hardware thread, but if a thread misses
  // on a cache line that is already being fetched, it will be added to the waiting
  // list for that line.
  class PendingMiss extends Bundle {
    val valid = Bool()
    val waitingThreadBitmap = UInt(cfg.shaderHarts.W)
    val address = UInt(cfg.busAddressBits.W)
  }

  val pendingMisses = RegInit(VecInit(Seq.fill(cfg.shaderHarts)(0.U.asTypeOf(new PendingMiss))))
  val camMatchOh = VecInit(pendingMisses.map(r => r.valid && r.address === cacheLineAlign(io.missAddress)))
  val camMatchIndex = PriorityEncoder(camMatchOh)

  // Determine if we should combine this with a pending load
  when (io.cacheMiss) {
    when (camMatchOh.asUInt.orR) {
      // Combine with existing request
      pendingMisses(camMatchIndex).waitingThreadBitmap := pendingMisses(camMatchIndex).waitingThreadBitmap | (1.U << io.missThread)
    } .otherwise {
      // Set up new request
      pendingMisses(io.missThread).valid := true.B
      pendingMisses(io.missThread).address := cacheLineAlign(io.missAddress)
      pendingMisses(io.missThread).waitingThreadBitmap := 1.U << io.missThread
    }
  }

  // The arbiter selects the next pending miss.
  val nextRequest = Module(new RRArbiter(UInt(cfg.busAddressBits.W), cfg.shaderHarts))
  for (i <- 0 until cfg.shaderHarts) {
    nextRequest.io.in(i).valid := pendingMisses(i).valid
    nextRequest.io.in(i).bits := pendingMisses(i).address
  }

  val burstActive = RegInit(false.B)
  val burstAddress = RegInit(0.U(cfg.busAddressBits.W))
  val burstChosen = RegInit(0.U(log2Up(cfg.shaderHarts).W))

  // Accumulate burst data into a cache line buffer. When the burst is complete, write it back to the cache
  val cacheLineBeats = cfg.cacheLineSizeBytes * 8 / cfg.busDataBits
  val burstCounter = RegInit(0.U(log2Up(cacheLineBeats).W))

  io.updateCacheData := io.readPort.data.bits
  io.readPort.address := nextRequest.io.out.bits
  io.readPort.length := cacheLineBeats.U
  io.readPort.data.ready := true.B

  io.wakeThreadBitmap := 0.U
  io.updateCacheDone := false.B
  io.updateCacheEn := false.B
  when (burstActive && io.readPort.data.valid) {
    io.updateCacheEn := true.B
    when (burstCounter === (cacheLineBeats - 1).U) {
      // Burst complete
      burstActive := false.B
      io.wakeThreadBitmap := pendingMisses(burstChosen).waitingThreadBitmap
      pendingMisses(burstChosen).valid := false.B
      io.updateCacheDone := true.B
    }.otherwise {
      burstCounter := burstCounter + 1.U
      burstAddress := burstAddress + 8.U
    }
  }

  nextRequest.io.out.ready := false.B
  when (!burstActive && nextRequest.io.out.valid) {
    // Start a new burst, issue address to memory arbiter
    nextRequest.io.out.ready := true.B
    burstActive := true.B
    burstCounter := 0.U
    burstChosen := nextRequest.io.chosen
    burstAddress := nextRequest.io.out.bits
    io.readPort.valid := true.B
  }.otherwise {
    io.readPort.valid := false.B
  }

  io.updateCacheAddress := burstAddress

  // Invariant 1: If burstActive is true, then the nextRequest must be valid.
  assert(!burstActive || nextRequest.io.out.valid,
    "burstActive is true but nextRequest is not valid")

  // Invariant 2: if updateCacheEn is true, wakeThreadBitmap must be non-zero.
  assert(!io.updateCacheDone || io.wakeThreadBitmap.orR,
    "updateCacheDone is true but wakeThreadBitmap is zero")

  for (i <- 0 until cfg.shaderHarts) {
    // Invariant 3: If a pending miss is valid, it must have at least one
    // waiting thread.
    assert(!pendingMisses(i).valid || pendingMisses(i).waitingThreadBitmap.orR,
      s"Pending miss ${i} is valid but has no waiting threads")

    for (j <- i + 1 until cfg.shaderHarts) {
      // Invariant 4: No two pending misses can have the same address.
      assert(!(pendingMisses(i).valid && pendingMisses(j).valid
        && pendingMisses(i).address === pendingMisses(j).address),
        "Two pending misses for the same address")

      // Invariant 5: No thread should be waiting on more than one pending miss.
      assert(!(pendingMisses(i).waitingThreadBitmap(j)
        && pendingMisses(j).waitingThreadBitmap(i)
        && pendingMisses(i).valid && pendingMisses(j).valid),
        "Two pending misses are waiting on the same thread")
    }
  }

  // Invariant 6: the caller should never queue a miss to the same line the same cycle
  // a previous one is finishing (and waking the thread). It can detect if an update
  //is occuring to the cache line.
  assert(!(io.cacheMiss && io.updateCacheDone
    && cacheLineAlign(io.missAddress) === cacheLineAlign(burstAddress)),
    "Cache miss was queued for a line that is being updated")
}



