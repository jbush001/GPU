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

import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funsuite.AnyFunSuite
import chisel3._
import chisel3.util._
import gpu._


class ShaderBuilder {
  private val instructions = scala.collection.mutable.ArrayBuffer[Int]()
  private val labels = scala.collection.mutable.Map[String, Int]()
  private val fixups = scala.collection.mutable.ArrayBuffer[(String, Int)]()

  private def addInstruction(instruction: Int): Unit = {
    instructions += instruction
  }

  def rrrInst(opcode: OpCode.Type, rd: Int, rs1: Int, rs2: Int): this.type = {
    val instruction = opcode.litValue.toInt | (rd << 7) | (rs1 << 14) | (rs2 << 21)
    addInstruction(instruction)
    this
  }

  def rrInst(opcode: OpCode.Type, rd: Int, rs1: Int): this.type = {
    val instruction = opcode.litValue.toInt | (rd << 7) | (rs1 << 14)
    addInstruction(instruction)
    this
  }

  def bInst(opcode: OpCode.Type, rs1: Int, label: String): this.type = {
    fixups += ((label, instructions.length))
    val instruction = opcode.litValue.toInt | (rs1 << 14)
    addInstruction(instruction)
    this
  }

  def kInst(opcode: OpCode.Type, rd: Int, imm: Int): this.type = {
    val instruction = opcode.litValue.toInt | (rd << 7) | (imm << 16)
    addInstruction(instruction)
    this
  }

  def emitLabel(label: String): this.type = {
    if (labels.contains(label)) {
      throw new Exception(s"Label $label already defined")
    }
    labels(label) = instructions.length
    this
  }

  def finish(): Seq[Long] = {
    for ((label, index) <- fixups) {
      val targetIndex = labels.getOrElse(label, throw new Exception(s"Label $label not found"))
      val offset = targetIndex - (index + 1)
      val instruction = instructions(index)
      val newInstruction = instruction | (((offset & 0x7f) << 7)
                                          | ((offset >> 7) << 20))
      instructions(index) = newInstruction
    }

    // Need to pack these into 64 bit words
    for (i <- 0 until (instructions.length + 1) / 2) yield {
      val low = instructions(i * 2)
      val high = if (i * 2 + 1 < instructions.length) instructions(i * 2 + 1) else 0
      ((high.toLong << 32) | (low.toLong & 0xffffffffL))
    }
  }
}

class ShaderCoreTests extends AnyFunSuite with ChiselSim {
  implicit val cfg: GpuConfig = new GpuConfig

  class ShaderTestHarness(implicit cfg: GpuConfig) extends Module {
    val io = IO(new Bundle {
      val dap = new DirectAccessPort
      val startJob = Flipped(Decoupled(new Bundle {
        val startPc = UInt(cfg.busAddressBits.W)
        val params = new ShaderParams
      }))

      val outputResult = Valid(Vec(cfg.shaderVectorLanes, UInt(32.W)))
    })

    val arbiter = Module(new MemoryArbiter(1, 1))
    val memory = Module(new SimAxiMemory(1024))
    val core = Module(new ShaderCore)

    io.startJob <> core.io.startJob

    core.io.icacheReadPort <> arbiter.io.readPorts(0)
    arbiter.io.axiBus <> memory.io
    memory.dap <> io.dap
    io.outputResult <> core.io.outputResult

    arbiter.io.writePorts(0).burst.valid := false.B
    arbiter.io.writePorts(0).data.valid := false.B
    arbiter.io.writePorts(0).burst.bits.address := 0.U
    arbiter.io.writePorts(0).burst.bits.length := 0.U
    arbiter.io.writePorts(0).data.bits := 0.U
  }

  def runShaderTest(
    programBytes: Seq[Long],
    startAddr: Long,
    params: Seq[Seq[UInt]] = Seq.fill(2)(Seq.fill(cfg.shaderVectorLanes)(0.U))
  )(testBody: ShaderTestHarness => Unit)(implicit cfg: GpuConfig): Unit = {
    simulate(new ShaderTestHarness) { dut =>
      // Common Setup / Initialization
      SimMemAccess.write(dut.clock, dut.io.dap, startAddr, programBytes)

      dut.io.startJob.bits.startPc.poke(startAddr.U)
      for (param <- 0 until 2) {
        for (lane <- 0 until cfg.shaderVectorLanes) {
          dut.io.startJob.bits.params.params(param)(lane).poke(params(param)(lane))
        }
      }

      // Execute test-specific assertions or stimulus
      testBody(dut)
    }
  }

  test("ShaderCore execution") {
    val asm = new ShaderBuilder()
    asm
      .kInst(OpCode.LoadHi, 1, 0x1234)
      .kInst(OpCode.LoadLo, 1, 0x5678)
      .rrrInst(OpCode.And, 65, 111, 111) // v2 = lane ID
      .rrrInst(OpCode.Addi, 105, 1, 65) // output = r1 + v2
      .rrInst(OpCode.Halt, 0, 0)

    runShaderTest(asm.finish(), 0) { dut =>
      dut.io.startJob.valid.poke(true.B)
      dut.io.startJob.bits.startPc.poke(0.U)
      dut.clock.step(1)
      dut.io.startJob.valid.poke(false.B)

      var gotResult = false
      for (_ <- 0 until 50) {
        dut.clock.step(1)
        if (dut.io.outputResult.valid.peek().litToBoolean) {
          for (lane <- 0 until cfg.shaderVectorLanes) {
            dut.io.outputResult.bits(lane).expect((0x12345678 + lane).U)
            gotResult = true
          }
        }
      }

      assert(gotResult, "ShaderCore did not produce any output result")
    }
  }
}
