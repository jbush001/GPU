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

  test("InstructionFetch basic operation") {
    simulate(new InstructionFetch()) { dut =>
      // Simulate a cache miss
      val address = 0x1000
      dut.io.fetchEnable.poke(true.B)
      dut.io.fetchPc.raw.poke(address.U)
      dut.io.fetchThread.poke(0.U)
      dut.clock.step(2)
      dut.io.fillRequest.valid.expect(true.B)
      dut.io.fillRequest.bits.address.raw.expect(address.U)
      dut.io.fetchEnable.poke(false.B)

      val sequence = Seq.tabulate(16)(i => i + 1000L)

      // Fill cache line
      for (i <- 0 until 8) {
        dut.io.updateCache.valid.poke(true.B)
        dut.io.updateCache.bits.address.raw.poke((address + (i * 8)).U)
        dut.io.updateCache.bits.data.poke((sequence(i * 2) + (sequence(i * 2 + 1) << 32)).U)
        dut.io.updateCache.bits.last.poke((i == 7).B)
        dut.clock.step()
      }

      dut.io.updateCache.valid.poke(false.B)
      dut.io.updateCache.bits.last.poke(false.B)
      dut.clock.step(2)

      // Cache hit
      val latency = 2
      for (cycle <- 0 until 16) {
        if (cycle < sequence.length) {
          dut.io.fetchEnable.poke(true.B)
          dut.io.fetchPc.raw.poke((address + (cycle * 4)).U)
          dut.io.fetchThread.poke(1.U)
        } else {
          dut.io.fetchEnable.poke(false.B)
        }

        if (cycle >= latency) {
          dut.io.outNearMiss.expect(false.B)
          dut.io.outValid.expect(true.B)
          dut.io.outPc.expect((address + ((cycle - latency) * 4)).U)
          dut.io.outThread.expect(1.U)

          // We're using a little endian convention, so this is the low 32 bits of the 64 bit bus value.
          dut.io.outInstruction.expect(sequence(cycle - latency).U(32.W))
        } else {
          dut.io.outValid.expect(false.B)
        }

        dut.clock.step()
      }
    }
  }

  test("InstructionFetch near miss") {
    simulate(new InstructionFetch()) { dut =>
      val address = 0x1000

      // Fill cache line
      for (i <- 0 until 8) {
        dut.io.updateCache.valid.poke(true.B)
        dut.io.updateCache.bits.address.raw.poke((address + (i * 8)).U)
        dut.io.updateCache.bits.data.poke(((i + 1) * 0x100000002L).U)
        dut.io.updateCache.bits.last.poke((i == 7).B)
        dut.clock.step()
      }

      // Read the same cache line.
      dut.io.fetchEnable.poke(true.B)
      dut.io.fetchPc.raw.poke((address + 4).U)
      dut.io.fetchThread.poke(0.U)
      dut.clock.step(2)
      dut.io.outNearMiss.expect(true.B)
      dut.io.outValid.expect(false.B)
    }
  }
}

