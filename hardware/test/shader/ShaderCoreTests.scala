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

class ShaderCoreTests extends AnyFunSuite with ChiselSim {
  implicit val cfg: GpuConfig = new GpuConfig

  class ShaderTestHarness(implicit cfg: GpuConfig) extends Module {
    val io = IO(new Bundle {
      val dap = new DirectAccessPort
      val startJob = Flipped(Decoupled(new Bundle {
        val startPc = UInt(cfg.busAddressBits.W)
        val jobId = UInt(cfg.shaderJobIdBits.W)
      }))

      val jobFinished = Valid(UInt(cfg.shaderJobIdBits.W))

      val regRead = Valid(new Bundle {
        val jobId = UInt(cfg.shaderJobIdBits.W)
        val addr = UInt(3.W)
      })

      val regReadData = Flipped(Valid(Vec(cfg.shaderVectorLanes, UInt(32.W))))

      val ioWakeJob = Flipped(Valid(UInt(log2Up(cfg.shaderThreads).W)))

      val regWrite = Valid(new Bundle {
        val jobId = UInt(cfg.shaderJobIdBits.W)
        val addr = UInt(3.W)
        val data = Vec(cfg.shaderVectorLanes, UInt(32.W))
      })
    })

    val arbiter = Module(new MemoryArbiter(1, 1))
    val memory = Module(new SimAxiMemory(1024))
    val core = Module(new ShaderCore)

    io.startJob <> core.io.startJob

    core.io.icacheReadPort <> arbiter.io.readPorts(0)
    arbiter.io.axiBus <> memory.io
    memory.dap <> io.dap

    core.io.regRead <> io.regRead
    core.io.regReadData <> io.regReadData
    core.io.regWrite <> io.regWrite
    io.jobFinished <> core.io.jobFinished

    arbiter.io.writePorts(0).burst.valid := false.B
    arbiter.io.writePorts(0).data.valid := false.B
    arbiter.io.writePorts(0).burst.bits.address := 0.U
    arbiter.io.writePorts(0).burst.bits.length := 0.U
    arbiter.io.writePorts(0).data.bits := 0.U

    core.io.ioWakeJob <> io.ioWakeJob
  }

  def runShaderTest(
    programBytes: Seq[Long],
    startAddr: Long,
    jobId: UInt = 0.U
  )(testBody: ShaderTestHarness => Unit)(implicit cfg: GpuConfig): Unit = {
    simulate(new ShaderTestHarness) { dut =>
      // Common Setup / Initialization
      SimMemAccess.write(dut.clock, dut.io.dap, startAddr, programBytes)

      dut.io.startJob.bits.startPc.poke(startAddr.U)
      dut.io.startJob.bits.jobId.poke(jobId)

      // Execute test-specific assertions or stimulus
      testBody(dut)
    }
  }

  test("ShaderCore basic arithmetic") {
    val program = new ShaderAssembler()
      .kInst(OpCode.LoadHi, 1, 0x1234)
      .kInst(OpCode.LoadLo, 1, 0x5678)
      .move(65, 112) // v2 = lane ID
      .rInst(OpCode.Addi, 105, 1, 65) // output = r1 + v2
      .halt()

    runShaderTest(program.finish(), 0) { dut =>
      dut.io.startJob.valid.poke(true.B)
      dut.io.startJob.bits.startPc.poke(0.U)
      dut.clock.step(1)
      dut.io.startJob.valid.poke(false.B)

      var gotResult = false
      for (_ <- 0 until 50) {
        dut.clock.step(1)
        if (dut.io.regWrite.valid.peek().litToBoolean) {
          for (lane <- 0 until cfg.shaderVectorLanes) {
            dut.io.regWrite.bits.data(lane).expect((0x12345678 + lane).U)
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

    // Euclidean algorithm to compute GCD of two numbers. This has
    // nested conditionals, and a loop.
    val program = new ShaderAssembler()
      .move(64, 96) // v0 = a
      .move(65, 97) // v1 = b
      .emitLabel("loop")
      .rInst(OpCode.Setne, 0, 64, 65)   // if (a != b) {
      .bInst(OpCode.Bz, 0, "done")
      .rInst(OpCode.Setlti, 1, 64, 65)  //   if (a < b) {
      .rInst(OpCode.And, 32, 1, 0)      //     exec mask = exec mask & (a < b)
      .rInst(OpCode.Subi, 65, 65, 64)   //     b = b - a
      .rInst(OpCode.Xor, 1, 1, 55)      //   } else {
      .rInst(OpCode.And, 32, 1, 0)      //     exec mask = exec mask & (a >= b)
      .rInst(OpCode.Subi, 64, 64, 65)   //     a = a - b
      .bInst(OpCode.Jump, 0, "loop")    //   }
      .emitLabel("done")                // }
      .move(32, 55)                     // (Restore exec mask)
      .move(105, 64)                    // result = a
      .halt()
      .finish()

    val rng = new scala.util.Random(42)

    // We keep all threads active with jobs. When one completes, we start the
    // next one. The Job structure tracks the state of active threads.
    class Job {
      var active = false
      var gotResult = false
      var jobId: Int = 0
      var a = Seq.fill(cfg.shaderVectorLanes)(0)
      var b = Seq.fill(cfg.shaderVectorLanes)(0)
      var expectedVector = Seq.fill(cfg.shaderVectorLanes)(0)
      var startCycle: Int = 0
    }

    val jobs = Seq.fill(cfg.shaderThreads)(new Job)

    def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

    def findFreeJob(): Int = {
      for (i <- 0 until cfg.shaderThreads) {
        if (!jobs(i).active) {
          return i
        }
      }

      fail("No free job found")
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
      job.gotResult = false
      job.expectedVector = job.a.zip(job.b).map { case (x, y) => gcd(x, y) }
      job
    }

    def resultToVector(dut: ShaderTestHarness): Seq[Int] = {
      (0 until cfg.shaderVectorLanes).map { lane =>
        dut.io.regWrite.bits.data(lane).peek().litValue.toInt
      }
    }

    simulate(new ShaderTestHarness) { dut =>
      SimMemAccess.write(dut.clock, dut.io.dap, 0, program)
      dut.io.startJob.bits.startPc.poke(0.U)

      val maxJobs = 8 // Useful for debugging
      var activeJobs = 0
      val maxCycles = 40000
      val flushCycles = 2000
      var regReadResult = Seq.fill(cfg.shaderVectorLanes)(0)
      var hasRegReadResult = false
      for (cycle <- 0 until maxCycles) {
        dut.io.regReadData.valid.poke(hasRegReadResult.B)
        for (lane <- 0 until cfg.shaderVectorLanes) {
          dut.io.regReadData.bits(lane).poke(regReadResult(lane).U)
        }

        if (dut.io.regRead.valid.peek().litToBoolean) {
          val readJobId = dut.io.regRead.bits.jobId.peek().litValue.toInt
          val job = jobs(readJobId)
          val addr = dut.io.regRead.bits.addr.peek().litValue.toInt
          regReadResult = if (addr == 0) job.a else if (addr == 1) job.b else Seq.fill(cfg.shaderVectorLanes)(0)
          hasRegReadResult = true
        } else {
          hasRegReadResult = false
        }

        if (dut.io.regWrite.valid.peek().litToBoolean) {
          val result = resultToVector(dut)
          val jobIndex = dut.io.regWrite.bits.jobId.peek().litValue.toInt
          if (DEBUG) {
            println(s"Job $jobIndex completed at cycle $cycle result = $result totalCycles ${cycle - jobs(jobIndex).startCycle}")
          }

          assert(jobs(jobIndex).active, s"Job $jobIndex was not active when result was received")
          assert(result == jobs(jobIndex).expectedVector, s"Job $jobIndex failed: got $result, expected ${jobs(jobIndex).expectedVector}")
          assert(!jobs(jobIndex).gotResult, s"Job $jobIndex received multiple results")

          jobs(jobIndex).gotResult = true
        }

        if (dut.io.jobFinished.valid.peek().litToBoolean) {
          val jobIndex = dut.io.jobFinished.bits.peek().litValue.toInt
          assert(jobs(jobIndex).gotResult, s"Job $jobIndex finished without producing a result")
          jobs(jobIndex).active = false
          activeJobs -= 1
        }

        // Start a new job. We stop creating new jobs near the end of the simulation to
        // allow all jobs to finish.
        if (dut.io.startJob.ready.peek().litToBoolean && cycle < (maxCycles - flushCycles) && activeJobs < maxJobs) {
          val jobIndex = findFreeJob()
          val job = initNewJob(jobIndex)
          if (DEBUG) {
            println(s"Starting new job $jobIndex at cycle $cycle a=${job.a} b=${job.b} expected=${job.expectedVector}")
          }

          job.startCycle = cycle
          activeJobs += 1
          dut.io.startJob.valid.poke(true.B)
          dut.io.startJob.bits.jobId.poke(jobIndex.U)
        } else {
          dut.io.startJob.valid.poke(false.B)
        }

        dut.clock.step(1)
      }

      for (index <- 0 until cfg.shaderThreads) {
        val job = jobs(index)
        if (job.active) {
          fail(s"Job $index did not complete, hung for ${maxCycles - job.startCycle} cycles expected=${job.expectedVector}")
        }
      }
    }
  }

  // Simulate texture fetch
  test("ShaderCore register read wait") {
    val program = new ShaderAssembler()
      .kInst(OpCode.LoadHi, 1, 0x1234)
      .move(64, 99)
      .move(104, 64)
      .halt()

    runShaderTest(program.finish(), 0) { dut =>
      dut.io.startJob.valid.poke(true.B)
      dut.io.startJob.bits.startPc.poke(0.U)
      dut.clock.step(1)
      dut.io.startJob.valid.poke(false.B)

      var gotResult = false
      var wakeupDelay = 0
      var gotRead = false
      var threadWoken = false
      var readJobId = 0
      var regReadValid = false
      for (_ <- 0 until 60) {
        dut.io.regReadData.valid.poke(regReadValid.B)

        regReadValid = false
        if (dut.io.regRead.valid.peek().litToBoolean) {
          readJobId = dut.io.regRead.bits.jobId.peek().litValue.toInt
          if (gotRead) {
            assert(threadWoken, "Thread should have been woken before second read")
            // Second read, return value
            regReadValid = true
            for (lane <- 0 until cfg.shaderVectorLanes) {
              dut.io.regReadData.bits(lane).poke((0x12345678 + lane).U)
            }
          } else {
            // First read, block the thread until data is available
            gotRead = true
            wakeupDelay = 15
          }
        }

        dut.io.ioWakeJob.valid.poke(false.B)
        if (gotRead && !threadWoken) {
          wakeupDelay -= 1
          if (wakeupDelay == 0) {
            dut.io.ioWakeJob.valid.poke(true.B)
            dut.io.ioWakeJob.bits.poke(readJobId.U)
            threadWoken = true
          }
        }

        dut.clock.step(1)

        if (dut.io.regWrite.valid.peek().litToBoolean) {
          for (lane <- 0 until cfg.shaderVectorLanes) {
            dut.io.regWrite.bits.data(lane).expect((0x12345678 + lane).U)
            gotResult = true
          }
        }
      }

      assert(gotResult, "ShaderCore did not produce any output result")
    }
  }
}
