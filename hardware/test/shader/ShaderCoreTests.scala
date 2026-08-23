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

  def rInst(opcode: OpCode.Type, rd: Int, rs1: Int, rs2: Int): this.type = {
    val instruction = opcode.litValue.toInt | (rd << 7) | (rs1 << 14) | (rs2 << 21)
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

  def move(rd: Int, rs: Int): this.type = {
    rInst(OpCode.Or, rd, rs, 53) // Const zero
    this
  }

  def halt(): this.type = {
    rInst(OpCode.Halt, 0, 0, 0)
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
      .move(65, 111) // v2 = lane ID
      .rInst(OpCode.Addi, 105, 1, 65) // output = r1 + v2
      .halt()

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

  // Stress test, tests multiple threads, branching, exec mask.
  test("ShaderCore gcd") {
    val DEBUG = false

    val program = new ShaderBuilder()
      .move(64, 96) // v0 = a
      .move(65, 97) // v1 = b
      .emitLabel("loop")
      .rInst(OpCode.Setne, 0, 64, 65)
      .bInst(OpCode.Bz, 0, "done")
      .rInst(OpCode.Setlti, 1, 64, 65)
      .rInst(OpCode.And, 32, 1, 0) // Exec mask
      .rInst(OpCode.Subi, 65, 65, 64)
      .rInst(OpCode.Xor, 1, 1, 55) // Invert exec mask ( xor 0xffffffff)
      .rInst(OpCode.And, 32, 1, 0) // Exec mask
      .rInst(OpCode.Subi, 64, 64, 65)
      .bInst(OpCode.Jump, 0, "loop")
      .emitLabel("done")
      .move(32, 55) // Restore exec mask
      .move(105, 64) // Store result in output
      .halt()
      .finish()
    val rng = new scala.util.Random(42)

    class Job {
      var active = false
      var a = Seq.fill(cfg.shaderVectorLanes)(0)
      var b = Seq.fill(cfg.shaderVectorLanes)(0)
      var expectedVector = Seq.fill(cfg.shaderVectorLanes)(0)
      var startCycle: Int = 0
    }

    val jobs = Seq.fill(cfg.shaderThreads)(new Job)

    def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

    def findFreeJob(): Option[Int] = {
      for (i <- 0 until cfg.shaderThreads) {
        if (!jobs(i).active) {
          return Some(i)
        }
      }

      None
    }

    val primes = Array(3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47)

    def initNewJob(index: Int): Job = {
      val job = jobs(index)

      // We're a bit clever here picking the numbers to avoid degenerate
      // cases.
      val base = Seq.fill(cfg.shaderVectorLanes)(rng.between(5, 30))
      job.a = base.map(x => x * primes(rng.nextInt(primes.length)))
      job.b = base.map(x => x * primes(rng.nextInt(primes.length)))
      job.active = true
      job.expectedVector = job.a.zip(job.b).map { case (x, y) => gcd(x, y) }
      job
    }

    def resultToVector(dut: ShaderTestHarness): Seq[Int] = {
      (0 until cfg.shaderVectorLanes).map { lane =>
        dut.io.outputResult.bits(lane).peek().litValue.toInt
      }
    }

    def findJobByResult(result: Seq[Int]): Int = {
      for (i <- 0 until cfg.shaderThreads) {
        if (jobs(i).active && jobs(i).expectedVector == result) {
          return i
        }
      }

      fail("No matching job found for result")
    }

    simulate(new ShaderTestHarness) { dut =>
      SimMemAccess.write(dut.clock, dut.io.dap, 0, program)
      dut.io.startJob.bits.startPc.poke(0.U)

      val maxJobs = 8 // Useful for debugging
      var activeJobs = 0
      val maxCycles = 40000
      val flushCycles = 2000
      for (cycle <- 0 until maxCycles) {
        if (dut.io.outputResult.valid.peek().litToBoolean) {
          val result = resultToVector(dut)
          val jobIndex = findJobByResult(result)
          if (DEBUG) {
            println(s"Job $jobIndex completed at cycle $cycle result = $result totalCycles ${cycle - jobs(jobIndex).startCycle}")
          }
          jobs(jobIndex).active = false
          activeJobs -= 1
        }

        // Start a new job. We stop creating new jobs near the end of the simulation to
        // allow all jobs to finish.
        if (dut.io.startJob.ready.peek().litToBoolean && cycle < (maxCycles - flushCycles) && activeJobs < maxJobs) {
          val jobIndex = findFreeJob()
          jobIndex match {
            case Some(index) =>
              val job = initNewJob(index)
              if (DEBUG) {
                println(s"Starting new job $index at cycle $cycle a=${job.a} b=${job.b} expected=${job.expectedVector}")
              }
              job.startCycle = cycle
              activeJobs += 1
              dut.io.startJob.valid.poke(true.B)
              for (lane <- 0 until cfg.shaderVectorLanes) {
                dut.io.startJob.bits.params.params(0)(lane).poke(job.a(lane))
                dut.io.startJob.bits.params.params(1)(lane).poke(job.b(lane))
              }
            case None => fail("DUT indicated ready, but all jobs are active")
          }
        } else {
          dut.io.startJob.valid.poke(false.B)
        }

        dut.clock.step(1)
      }

      for (index <- 0 until cfg.shaderThreads) {
        val job = jobs(index)
        if (job.active) {
          fail(s"Job $index did not complete, hung for ${maxCycles - job.startCycle} cycles a= ${job.a} b=${job.b} expected=${job.expectedVector}")
        }
      }
    }
  }
}
