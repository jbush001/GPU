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

import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funsuite.AnyFunSuite
import chisel3._

object OpCode {
  val HALT = 0
  val AND = 1
  val OR = 2
  val XOR = 3
  val ADDI = 4
  val SUBI = 5
  val MULI = 6
  val MULIH = 7
  val LSL = 8
  val ASR = 9
  val LSR = 10
  val ADDF = 11
  val SUBF = 12
  val MULF = 13
  val RECIP = 14
  val FTOI = 15
  val ITOF = 16
  val SETGTF = 17
  val SETLTF = 18
  val SETGEI = 19
  val SETLTI = 20
  val SETGEU = 21
  val SETLTU = 22
  val SETEQ = 23
  val SETNE = 24
  val BNZ = 25
  val BZ = 26
  val J = 27
  val LOADLO = 28
  val LOADHI = 29
}

class ShaderBuilder {
  private val instructions = scala.collection.mutable.ArrayBuffer[Long]()
  private val labels = scala.collection.mutable.Map[String, Int]()
  private val fixups = scala.collection.mutable.ArrayBuffer[(String, Int)]()

  private def addInstruction(instruction: Long): Unit = {
    instructions += instruction
  }

  def rrrInst(opcode: Int, rd: Int, rs1: Int, rs2: Int): this.type = {
    val instruction = opcode | (rd << 7) | (rs1 << 14) | (rs2 << 21)
    addInstruction(instruction)
    this
  }

  def rrInst(opcode: Int, rd: Int, rs1: Int): this.type = {
    val instruction = opcode | (rd << 7) | (rs1 << 14)
    addInstruction(instruction)
    this
  }

  def bInst(opcode: Int, rs1: Int, label: String): this.type = {
    fixups += ((label, instructions.length))
    val instruction = opcode | (rs1 << 14)
    addInstruction(instruction)
    this
  }

  def kInst(opcode: Int, rd: Int, imm: Int): this.type = {
    val instruction = opcode | (rd << 7) | (imm << 16)
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
    instructions.toSeq
  }
}

class ShaderCoreTests extends AnyFunSuite with ChiselSim {
  implicit val cfg: GpuConfig = new GpuConfig

  class ShaderTestHarness(implicit cfg: GpuConfig) extends Module {
    val io = IO(new Bundle {
      val dap = new DirectAccessPort
    })

    val arbiter = Module(new MemoryArbiter(1, 1))
    val memory = Module(new SimAxiMemory(1024))
    val core = Module(new ShaderCore)

    core.io.icacheReadPort <> arbiter.io.readPorts(0)
    arbiter.io.axiBus <> memory.io
    memory.dap <> io.dap

    arbiter.io.writePorts(0).valid := false.B
    arbiter.io.writePorts(0).data.valid := false.B
    arbiter.io.writePorts(0).address := 0.U
    arbiter.io.writePorts(0).length := 0.U
    arbiter.io.writePorts(0).data.bits := 0.U
  }

  def runShaderTest(
    programBytes: Seq[Long],
    startAddr: Long = 0
  )(testBody: ShaderTestHarness => Unit)(implicit cfg: GpuConfig): Unit = {
    simulate(new ShaderTestHarness) { dut =>
      // Common Setup / Initialization
      SimMemAccess.write(dut.clock, dut.io.dap, startAddr, programBytes)

      // Execute test-specific assertions or stimulus
      testBody(dut)
    }
  }

  test("ShaderCore execution") {
    val asm = new ShaderBuilder()
    asm
      .kInst(OpCode.LOADLO, 1, 5) // r1 = 5
      .kInst(OpCode.LOADHI, 2, 10) // r2 = 10
      .rrrInst(OpCode.ADDI, 3, 1, 2) // r3 = r1 + r2
    // ...

    runShaderTest(asm.finish()) { dut =>
      dut.clock.step(1)
    }
  }
}
