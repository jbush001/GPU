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
import gpu._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funsuite.AnyFunSuite

class InstructionFetchTests extends AnyFunSuite with ChiselSim {
  implicit val cfg: GpuConfig = GpuConfig()

  def simulateCacheMiss(dut: InstructionFetchStage, address: Long): Unit = {
    dut.io.fetchRequest.valid.poke(true.B)
    dut.io.fetchRequest.bits.pc.raw.poke(address.U)
    dut.io.fetchRequest.bits.thread.poke(0.U)
    dut.clock.step(2)
    dut.io.fillRequest.valid.expect(true.B)
    dut.io.fillRequest.bits.address.raw.expect(address.U)
    dut.io.fetchRequest.valid.poke(false.B)
  }

  // Note data here is 32-bit instructions (although memory is actually 64-bits wide)
  def simulateCacheFill(dut: InstructionFetchStage, address: Long, data: Seq[Long]): Unit = {
    // Fill cache line
    for (i <- 0 until 8) {
      dut.io.updateCache.valid.poke(true.B)
      dut.io.updateCache.bits.address.raw.poke((address + (i * 8)).U)
      dut.io.updateCache.bits.data.poke((data(i * 2) + (data(i * 2 + 1) << 32)).U)
      dut.io.updateCache.bits.last.poke((i == 7).B)
      dut.clock.step()
    }

    dut.io.updateCache.valid.poke(false.B)
    dut.io.updateCache.bits.last.poke(false.B)
    dut.clock.step(2)
  }

  test("InstructionFetchStage basic operation") {
    simulate(new InstructionFetchStage()) { dut =>
      val address = 0x1000
      simulateCacheMiss(dut, address)
      simulateCacheFill(dut, address, Seq.tabulate(16)(i => i + 1000L))

      // Cache hit
      val latency = 2
      for (cycle <- 0 until 16) {
        if (cycle < 16) {
          dut.io.fetchRequest.valid.poke(true.B)
          dut.io.fetchRequest.bits.pc.raw.poke((address + (cycle * 4)).U)
          dut.io.fetchRequest.bits.thread.poke(1.U)
        } else {
          dut.io.fetchRequest.valid.poke(false.B)
        }

        if (cycle >= latency) {
          dut.io.nearMiss.expect(false.B)
          dut.io.output.valid.expect(true.B)
          dut.io.output.bits.pc.expect((address + ((cycle - latency) * 4)).U)
          dut.io.output.bits.thread.expect(1.U)

          // We're using a little endian convention, so this is the low 32 bits of the 64 bit bus value.
          dut.io.output.bits.instruction.expect((1000 + (cycle - latency)).U(32.W))
        } else {
          dut.io.output.valid.expect(false.B)
        }

        dut.clock.step()
      }
    }
  }

  test("InstructionFetchStage near miss") {
    simulate(new InstructionFetchStage()) { dut =>
      val address = 0x1000

      simulateCacheMiss(dut, address)

      // Begin filling cache line.
      dut.io.updateCache.valid.poke(true.B)
      for (i <- 0 until 6) {
        dut.io.updateCache.bits.address.raw.poke((address + (i * 8)).U)
        dut.clock.step()
      }

      // Cycle 7: fetch request enters pipeline, fill penultimate beat.
      dut.io.fetchRequest.valid.poke(true.B)
      dut.io.fetchRequest.bits.pc.raw.poke(address.U)
      dut.io.fetchRequest.bits.thread.poke(0.U)
      dut.io.updateCache.bits.address.raw.poke((address + (6 * 8)).U)
      dut.clock.step()

      // Cycle 8: fill completes, fetch request enters second stage of pipeline.
      // This looks like a cache miss from the tag check perspective.
      dut.io.fetchRequest.valid.poke(false.B)
      dut.io.updateCache.bits.address.raw.poke((address + (7 * 8)).U)
      dut.io.updateCache.bits.last.poke(true.B)
      dut.clock.step()

      dut.io.nearMiss.expect(true.B)
      dut.io.output.valid.expect(false.B)
    }
  }

  test("InstructionFetchStage fill complete on first fetch cycle") {
    simulate(new InstructionFetchStage()) { dut =>
      val address = 0x1000

      simulateCacheMiss(dut, address)

      // Begin filling cache line.
      dut.io.updateCache.valid.poke(true.B)
      for (i <- 0 until 7) {
        dut.io.updateCache.bits.address.raw.poke((address + (i * 8)).U)
        dut.io.updateCache.bits.data.poke(((i + 1) * 0x100000002L).U)
        dut.clock.step()
      }

      // First stage of fetch, fill completes. The tag won't match,
      // so we need a bypass to check this condition explicitly.
      dut.io.fetchRequest.valid.poke(true.B)
      dut.io.fetchRequest.bits.pc.raw.poke(address.U)
      dut.io.fetchRequest.bits.thread.poke(0.U)
      dut.io.updateCache.bits.address.raw.poke((address + (7 * 8)).U)
      dut.io.updateCache.bits.data.poke(((7 + 1) * 0x100000002L).U)
      dut.io.updateCache.bits.last.poke(true.B)
      dut.clock.step()
      dut.io.fetchRequest.valid.poke(false.B)
      dut.io.updateCache.valid.poke(false.B)
      dut.clock.step()

      dut.io.nearMiss.expect(false.B)
      dut.io.output.valid.expect(true.B)
      dut.io.output.bits.pc.expect(address.U)
      dut.io.output.bits.instruction.expect(2.U)
      dut.io.output.bits.thread.expect(0.U)
    }
  }

  test("InstructionFetchStage miss tag mismatch") {
    simulate(new InstructionFetchStage()) { dut =>
      val address = 0x1000

      simulateCacheMiss(dut, address)
      simulateCacheFill(dut, address, Seq.tabulate(16)(i => i + 1000L))

      // This maps to the same cache line, but the tag is different, so it should be a miss.
      dut.io.fetchRequest.valid.poke(true.B)
      dut.io.fetchRequest.bits.pc.raw.poke((address + cfg.cacheLineSizeBytes * cfg.icacheLines).U)
      dut.io.fetchRequest.bits.thread.poke(0.U)
      dut.clock.step(2)
      dut.io.nearMiss.expect(false.B)
      dut.io.output.valid.expect(false.B)
      dut.io.fillRequest.valid.expect(true.B)
      dut.io.fillRequest.bits.address.raw.expect((address + cfg.cacheLineSizeBytes * cfg.icacheLines).U)
    }
  }
}

