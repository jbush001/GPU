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

class ICacheAddress(implicit cfg: GpuConfig) extends Bundle {
  val raw = UInt(cfg.busAddressBits.W)
  def tag = raw(cfg.busAddressBits - 1, cfg.busAddressBits - cfg.tagBits)
  def index = raw(cfg.cacheLineOffsetBits + cfg.indexBits - 1, cfg.cacheLineOffsetBits)
  def cacheLineOffset = raw(cfg.cacheLineOffsetBits - 1, 0)

  def cacheLineAligned = WireInit(new ICacheAddress,
    Cat(tag, index, 0.U(cfg.cacheLineOffsetBits.W)).asTypeOf(new ICacheAddress))

  def +(bytes: UInt)= WireInit(new ICacheAddress,
    (raw + bytes).asTypeOf(new ICacheAddress))
}

class CacheFillRequest(implicit cfg: GpuConfig) extends Bundle {
  val address = new ICacheAddress
  val thread = UInt(log2Up(cfg.shaderThreads).W)
}

class CacheUpdateRequest (implicit cfg: GpuConfig) extends Bundle {
  val address = new ICacheAddress
  val data = UInt(cfg.busDataBits.W)
  val last = Bool()
}

/** Handles reading cache lines from external memory and queuing pending misses.
  */
class ICacheFill(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    // To memory arbiter
    val readPort = new MemReadPort

    // From instruction fetch. Enqueue a new miss request.
    val fillRequest = Flipped(Valid(new CacheFillRequest))

    // To instruction fetch. Write data back to the cache.
    val updateCache = Output(Valid(new CacheUpdateRequest))

    // Each bit corresponds to a hardware thread that should
    // be woken up because its cache miss has been filled.
    val wakeThreadBitmap = Output(UInt(cfg.shaderThreads.W))
  })

  // There is one pending miss entry per hardware thread, but if a thread misses
  // on a cache line that is already being fetched, it will be added to the waiting
  // list for that line.
  class PendingMiss extends Bundle {
    val valid = Bool()
    val waitingThreadBitmap = UInt(cfg.shaderThreads.W)
    val address = new ICacheAddress
  }

  val pendingMisses = RegInit(VecInit(Seq.fill(cfg.shaderThreads)(
    0.U.asTypeOf(new PendingMiss))))
  val camMatchOh = VecInit(pendingMisses.map(r => r.valid
    && r.address.cacheLineAligned === io.fillRequest.bits.address.cacheLineAligned))
  val camMatchIndex = PriorityEncoder(camMatchOh)

  // Determine if we should combine this with a pending load
  when (io.fillRequest.valid) {
    when (camMatchOh.asUInt.orR) {
      // Combine with existing request
      pendingMisses(camMatchIndex).waitingThreadBitmap :=
        pendingMisses(camMatchIndex).waitingThreadBitmap | UIntToOH(io.fillRequest.bits.thread)
    } .otherwise {
      // Set up new request
      pendingMisses(io.fillRequest.bits.thread).valid := true.B
      pendingMisses(io.fillRequest.bits.thread).address := io.fillRequest.bits.address.cacheLineAligned
      pendingMisses(io.fillRequest.bits.thread).waitingThreadBitmap := UIntToOH(io.fillRequest.bits.thread)
    }
  }

  // The arbiter selects the next pending miss.
  val nextRequest = Module(new RRArbiter(new ICacheAddress, cfg.shaderThreads))
  for (i <- 0 until cfg.shaderThreads) {
    nextRequest.io.in(i).valid := pendingMisses(i).valid
    nextRequest.io.in(i).bits := pendingMisses(i).address
  }

  val cacheLineBeats = cfg.cacheLineSizeBytes * 8 / cfg.busDataBits

  val burstActive = RegInit(false.B)
  val burstThread = RegInit(0.U(log2Up(cfg.shaderThreads).W))
  val burstAddress = Reg(new ICacheAddress)
  val burstCounter = RegInit(0.U(log2Up(cacheLineBeats).W))

  io.updateCache.bits.address := burstAddress
  io.updateCache.bits.data := io.readPort.data.bits
  io.updateCache.bits.last := burstCounter === (cacheLineBeats - 1).U
  io.readPort.address := nextRequest.io.out.bits.raw
  io.readPort.length := cacheLineBeats.U
  io.readPort.data.ready := true.B

  io.wakeThreadBitmap := 0.U
  io.updateCache.valid := false.B
  when (burstActive && io.readPort.data.valid) {
    io.updateCache.valid := true.B
    when (burstCounter === (cacheLineBeats - 1).U) {
      // Burst complete
      burstActive := false.B
      io.wakeThreadBitmap := pendingMisses(burstThread).waitingThreadBitmap
      pendingMisses(burstThread).valid := false.B
    }.otherwise {
      burstCounter := burstCounter + 1.U
      burstAddress := burstAddress + (cfg.busDataBits / 8).U
    }
  }

  nextRequest.io.out.ready := false.B
  io.readPort.valid := false.B
  when (!burstActive && nextRequest.io.out.valid) {
    // Start a new burst, issue address to memory arbiter
    nextRequest.io.out.ready := true.B
    burstActive := true.B
    burstCounter := 0.U
    burstThread := nextRequest.io.chosen
    burstAddress := nextRequest.io.out.bits
    io.readPort.valid := true.B
  }

  // Invariant 1: If burstActive is true, then the nextRequest must be valid.
  assert(!burstActive || nextRequest.io.out.valid,
    "burstActive is true but nextRequest is not valid")

  // Invariant 2: if a cache line update is complete, wakeThreadBitmap must be non-zero.
  assert(!(io.updateCache.valid && io.updateCache.bits.last) || io.wakeThreadBitmap.orR,
    "updateCache is valid but wakeThreadBitmap is zero")

  // Invariant 3: if updateCache.valid is true, then burstActive must also be true
  assert(!io.updateCache.valid || burstActive,
    "updateCache is valid but burstActive is false")

  for (i <- 0 until cfg.shaderThreads) {
    // Invariant 4: If a pending miss is valid, it must have at least one
    // waiting thread.
    assert(!pendingMisses(i).valid || pendingMisses(i).waitingThreadBitmap.orR,
      s"Pending miss ${i} is valid but has no waiting threads")

    for (j <- i + 1 until cfg.shaderThreads) {
      // Invariant 5: No two pending misses can have the same address.
      assert(!(pendingMisses(i).valid && pendingMisses(j).valid
        && pendingMisses(i).address === pendingMisses(j).address),
        "Two pending misses for the same address")
       // Invariant 6: No thread should be waiting on more than one pending miss.
      assert(!(pendingMisses(i).waitingThreadBitmap(j)
        && pendingMisses(j).waitingThreadBitmap(i)
        && pendingMisses(i).valid && pendingMisses(j).valid),
        "Two pending misses are waiting on the same thread")
    }
  }

  // Invariant 7 (external): the caller should never queue a miss to the same line the same cycle
  // a previous one is finishing (and waking the thread). It can detect if an update
  //is occuring to the cache line.
  assert(!(io.fillRequest.valid && io.updateCache.valid && io.updateCache.bits.last
    && io.fillRequest.bits.address.cacheLineAligned === burstAddress.cacheLineAligned),
    "Cache miss was queued for a line that is being updated")
}



