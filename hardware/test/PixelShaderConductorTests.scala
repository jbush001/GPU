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
import scala.util.Random

class FragmentShaderConductorTests extends AnyFunSuite with ChiselSim with TestHelpers {
  implicit val cfg: GpuConfig = GpuConfig()

  /** Simulate a source quad arriving at the pixel shader conductor from the
    * rasterizer/depth interpolator.
    */
  def loadSourceQuad(dut: FragmentShaderConductor, x: Int, y: Int, mask: Int,
    lambda: Seq[Seq[Float]], depths: Seq[Float],triangleId: Int = 0,
    lastQuad: Boolean = false, fence: Boolean = false, fragShaderAddr: Int = 0): Unit = {
    dut.io.sourceQuad.valid.poke(true)
    dut.io.sourceQuad.bits.location.poke(x, y)
    dut.io.sourceQuad.bits.mask.poke(mask)
    dut.io.sourceQuad.bits.triangleId.poke(triangleId)
    dut.io.sourceQuad.bits.fragShaderAddr.poke(fragShaderAddr)
    for (i <- lambda.indices) {
      for (j <- lambda(i).indices) {
        dut.io.sourceQuad.bits.lambda(i)(j).poke(lambda(i)(j))
      }
    }

    for (i <- depths.indices) {
      dut.io.sourceQuad.bits.depths(i).poke(depths(i))
    }

    dut.io.sourceQuad.bits.lastQuad.poke(lastQuad || fence)
    dut.io.sourceQuad.bits.fence.poke(fence)

    dut.clock.step()
    dut.io.sourceQuad.valid.poke(false)
  }

  /** Validate a finished quad output from the FragmentShaderConductor */
  def drainShadedQuad(dut: FragmentShaderConductor, expectedX: Int,
    expectedY: Int, expectedMask: Int, colors: Seq[Seq[Float]], depths: Seq[Float]): Unit = {
    dut.io.shadedQuad.bits.location.expect(expectedX, expectedY)
    dut.io.shadedQuad.bits.mask.expect(expectedMask)
    dut.io.shadedQuad.valid.expect(true)
    for (i <- colors.indices) {
      for (j <- colors(i).indices) {
        dut.io.shadedQuad.bits.colors(i)(j).expect(colors(i)(j))
      }
    }

    for (i <- depths.indices) {
      dut.io.shadedQuad.bits.depths(i).expect(depths(i))
    }

    dut.clock.step()
  }

  /** Simulate a shader core reading a register from the FragmentShaderConductor */
  def readRegister(dut: FragmentShaderConductor, jobId: Int, addr: Int): Seq[Int] = {
    dut.io.shaderRegRead.valid.poke(true)
    dut.io.shaderRegRead.bits.jobId.poke(jobId)
    dut.io.shaderRegRead.bits.addr.poke(addr)
    dut.clock.step()
    dut.io.shaderRegReadData.valid.expect(true)
    val result = Seq.tabulate(cfg.shaderVectorLanes)(i =>
      (dut.io.shaderRegReadData.bits(i).peek().litValue & 0xffffffff).toInt)
    dut.io.shaderRegRead.valid.poke(false)
    result
  }

  def readRegisterFloat(dut: FragmentShaderConductor, jobId: Int, addr: Int): Seq[Float] = {
    readRegister(dut, jobId, addr).map(java.lang.Float.intBitsToFloat)
  }

  /** Simulate a shader core writing a register to the FragmentShaderConductor */
  def pokeShaderReg(dut: FragmentShaderConductor, jobId: Int, addr: Int, data: Seq[Int]): Unit = {
    dut.io.shaderRegWrite.valid.poke(true)
    dut.io.shaderRegWrite.bits.jobId.poke(jobId)
    dut.io.shaderRegWrite.bits.addr.poke(addr)
    for (i <- data.indices) {
      dut.io.shaderRegWrite.bits.data(i).poke(data(i) & 0xffffffff)
    }
    dut.clock.step()
    dut.io.shaderRegWrite.valid.poke(false)
  }

  def pokeShaderRegFloat(dut: FragmentShaderConductor, jobId: Int, addr: Int, data: Seq[Float]): Unit = {
    pokeShaderReg(dut, jobId, addr, data.map(java.lang.Float.floatToIntBits))
  }

  /** Simulate the triangle setup module writing a varying coefficient to the FragmentShaderConductor */
  def writeVaryingCoeff(dut: FragmentShaderConductor, triangleId: Int, index: Int, data: Float): Unit = {
    dut.io.writeVaryingCoeff.valid.poke(true)
    dut.io.writeVaryingCoeff.bits.triangleId.poke(triangleId)
    dut.io.writeVaryingCoeff.bits.index.poke(index)
    dut.io.writeVaryingCoeff.bits.value.poke(data)
    dut.clock.step()
    dut.io.writeVaryingCoeff.valid.poke(false)
  }

  test("basic operation") {
    simulate(new FragmentShaderConductor) { dut =>
      // Fill
      val fragShaderAddr = 0x1234567
      val masks = Seq.tabulate(cfg.shaderVectorLanes / Consts.pixelsPerQuad)(i => 10 + i)
      for (i <- 0 until cfg.shaderVectorLanes / Consts.pixelsPerQuad) {
        dut.io.startJob.valid.expect(false)
        dut.io.sourceQuad.ready.expect(true)
        val base = i * Consts.pixelsPerQuad
        loadSourceQuad(dut, i + 3, i + 4, masks(i),
          Seq.tabulate(Consts.pixelsPerQuad)(j => Seq(5.0f + base + j, 10.0f + base + j)),
          Seq(1.0f, 2.0f, 3.0f, 4.0f),
          triangleId = 3,
          lastQuad = (i == (cfg.shaderVectorLanes / Consts.pixelsPerQuad - 1)),
          fragShaderAddr = fragShaderAddr)
      }

      // Process
      dut.io.startJob.valid.expect(true)
      dut.io.startJob.ready.poke(true)
      dut.io.sourceQuad.ready.expect(true)
      val jobId = dut.io.startJob.bits.jobId.peek().litValue.toInt
      dut.io.startJob.bits.startPc.expect(fragShaderAddr)
      dut.clock.step()
      dut.io.startJob.valid.expect(false)

      // Read/write registers
      assert(readRegisterFloat(dut, jobId, 0) ==
        Seq.tabulate(cfg.shaderVectorLanes)(i => 5.0f + i)) // lambda 0
      assert(readRegisterFloat(dut, jobId, 1) ==
        Seq.tabulate(cfg.shaderVectorLanes)(i => 10.0f + i)) // lambda 1

      pokeShaderRegFloat(dut, jobId, 0, Seq.tabulate(cfg.shaderVectorLanes)(i => 15.0f + i)) // red
      pokeShaderRegFloat(dut, jobId, 1, Seq.tabulate(cfg.shaderVectorLanes)(i => 20.0f + i)) // blue
      pokeShaderRegFloat(dut, jobId, 2, Seq.tabulate(cfg.shaderVectorLanes)(i => 30.0f + i)) // green
      pokeShaderRegFloat(dut, jobId, 3, Seq.tabulate(cfg.shaderVectorLanes)(i => 40.0f + i)) // alpha

      dut.io.jobFinished.valid.poke(true)
      dut.io.jobFinished.bits.poke(jobId)
      dut.clock.step()
      dut.io.jobFinished.valid.poke(false)

      // Drain
      for (i <- 0 until cfg.shaderVectorLanes / Consts.pixelsPerQuad) {
        dut.io.startJob.valid.expect(false)
        dut.io.sourceQuad.ready.expect(true)

        if (i == (cfg.shaderVectorLanes / Consts.pixelsPerQuad - 1)) {
          dut.io.triangleFinished.valid.expect(true)
          dut.io.triangleFinished.bits.expect(3)
        }

        drainShadedQuad(dut, i + 3, i + 4, masks(i), Seq.tabulate(Consts.pixelsPerQuad)(j =>
          Seq(i * 4 + j + 15.0f, i * 4 + j + 20.0f, i * 4 + j + 30.0f, i * 4 + j + 40.0f)),
          Seq(1.0f, 2.0f, 3.0f, 4.0f))
      }

      dut.io.shadedQuad.valid.expect(false)
    }
  }

  test("drain") {
    simulate(new FragmentShaderConductor) { dut =>
      dut.io.startJob.ready.poke(true)

      dut.io.startJob.valid.expect(false)

      // Set last triangle bit on an incomplete job.
      loadSourceQuad(dut, 3, 4, 15,
        Seq.tabulate(Consts.pixelsPerQuad)(i => Seq(1.0f + i * 2, 2.0f + i * 2)),
        Seq.fill(Consts.pixelsPerQuad)(0.0f),
        lastQuad = true, fence = true)
      dut.io.startJob.valid.expect(true)
      dut.io.startJob.ready.poke(true)
      val jobId = dut.io.startJob.bits.jobId.peek().litValue.toInt
      dut.clock.step()

      // Finish processing
      dut.io.jobFinished.valid.poke(true)
      dut.io.jobFinished.bits.poke(jobId)
      dut.clock.step()
      dut.io.jobFinished.valid.poke(false)

      // Valid pixel
      dut.io.shadedQuad.valid.expect(true)
      dut.io.shadedQuad.bits.location.expect(3, 4)
      dut.io.shadedQuad.bits.mask.expect(15)
      dut.clock.step()

      // Now it should short circuit and not emit the null quads.
      dut.io.shadedQuad.valid.expect(false)
    }
  }

  test("drains jobs in source order") {
    simulate(new FragmentShaderConductor) { dut =>
      dut.io.startJob.ready.poke(true)

      for (x <- 0 until cfg.shaderVectorLanes / Consts.pixelsPerQuad) {
        loadSourceQuad(dut, x, 0, 15,
          Seq.fill(Consts.pixelsPerQuad)(Seq(0, 0)),
          Seq.fill(Consts.pixelsPerQuad)(0.0f),
          triangleId = 2)
      }

      dut.io.startJob.valid.expect(true)
      val firstJobId = dut.io.startJob.bits.jobId.peek().litValue.toInt
      dut.clock.step()

      for (x <- 0 until cfg.shaderVectorLanes / Consts.pixelsPerQuad) {
        loadSourceQuad(dut, x + 10, 0, 15,
          Seq.fill(Consts.pixelsPerQuad)(Seq(0, 0)),
          Seq.fill(Consts.pixelsPerQuad)(0.0f),
          triangleId = 2,
          lastQuad = (x == (cfg.shaderVectorLanes / Consts.pixelsPerQuad - 1)))
      }

      dut.io.startJob.valid.expect(true)
      val secondJobId = dut.io.startJob.bits.jobId.peek().litValue.toInt
      dut.clock.step()

      // A later completed job cannot overtake the head of the FIFO.
      dut.io.jobFinished.valid.poke(true)
      dut.io.jobFinished.bits.poke(secondJobId)
      dut.clock.step()
      dut.io.jobFinished.valid.poke(false)
      dut.io.shadedQuad.valid.expect(false)

      dut.io.jobFinished.valid.poke(true)
      dut.io.jobFinished.bits.poke(firstJobId)
      dut.clock.step()
      dut.io.jobFinished.valid.poke(false)

      for (x <- 0 until cfg.shaderVectorLanes / Consts.pixelsPerQuad) {
        dut.io.triangleFinished.valid.expect(false)
        drainShadedQuad(dut, x, 0, 15,
          Seq.fill(Consts.pixelsPerQuad)(Seq.fill(Color.numChannels)(0)),
          Seq.fill(Consts.pixelsPerQuad)(0.0f))
      }

      for (x <- 0 until cfg.shaderVectorLanes / Consts.pixelsPerQuad) {
        if (x == (cfg.shaderVectorLanes / Consts.pixelsPerQuad - 1)) {
          dut.io.triangleFinished.valid.expect(true)
          dut.io.triangleFinished.bits.expect(2)
        } else {
          dut.io.triangleFinished.valid.expect(false)
        }

        drainShadedQuad(dut, x + 10, 0, 15,
          Seq.fill(Consts.pixelsPerQuad)(Seq.fill(Color.numChannels)(0)),
          Seq.fill(Consts.pixelsPerQuad)(0.0f))
      }
    }
  }

  // Checks that job states are handled properly.
  // TODO: need to perform register accesses to ensure writes
  // and reads are propagaged.
  test("stress") {
    simulate(new FragmentShaderConductor) { dut =>
      val rng = new Random(42)

      var tilex = 1
      var tiley = 1
      val WIDTH = 100

      class ActiveJob(val jobId: Int) {
        var cyclesLeft = rng.nextInt(20) + 10
      }

      val activeJobs = scala.collection.mutable.Set[ActiveJob]()
      val outstandingQuads = scala.collection.mutable.Set[(Int, Int)]()

      val maxCycles = 1000
      val finalizeCycles = 100
      var sentFence = false
      for (cycle <- 0 until maxCycles) {
        // Check on loading new rasterized quads
        if (cycle <= maxCycles - finalizeCycles &&
          dut.io.sourceQuad.ready.peek().litToBoolean) {
          tilex += 1
          if (tilex >= WIDTH) {
            tilex = 0
            tiley += 1
          }

          dut.io.sourceQuad.valid.poke(true)
          dut.io.sourceQuad.bits.location.poke(tilex, tiley)
          dut.io.sourceQuad.bits.mask.poke(15)
          if (cycle >= maxCycles - finalizeCycles && !sentFence) {
            dut.io.sourceQuad.bits.fence.poke(true)
            dut.io.sourceQuad.bits.lastQuad.poke(true)
            sentFence = true
          } else {
            dut.io.sourceQuad.bits.fence.poke(false)
            dut.io.sourceQuad.bits.lastQuad.poke(false)
          }

          outstandingQuads += ((tilex, tiley))
        } else {
          dut.io.sourceQuad.valid.poke(false)
        }

        for (job <- activeJobs) {
          if (job.cyclesLeft > 0) {
            job.cyclesLeft -= 1
          }
        }

        // Pick one completed job to finish
        val completedJobs = activeJobs.filter(_.cyclesLeft == 0)
        if (completedJobs.nonEmpty) {
          val jobToFinish = completedJobs.head
          activeJobs -= jobToFinish
          dut.io.jobFinished.valid.poke(true)
          dut.io.jobFinished.bits.poke(jobToFinish.jobId)
        } else {
          dut.io.jobFinished.valid.poke(false)
        }

        // Assert ready if there are fewer than 8 active jobs
        val ready = activeJobs.size < 8
        dut.io.startJob.ready.poke(ready)

        // Note we do this after checking to end jobs so we don't try to finish
        // jobs the same cycle they start.
        if (dut.io.startJob.valid.peek().litToBoolean && ready) {
          val jobId = dut.io.startJob.bits.jobId.peek().litValue.toInt
          activeJobs += new ActiveJob(jobId)
        }

        // Handle output pixels
        if (dut.io.shadedQuad.valid.peek().litToBoolean) {
          val locationX = dut.io.shadedQuad.bits.location.x.peek().litValue.toInt
          val locationY = dut.io.shadedQuad.bits.location.y.peek().litValue.toInt
          outstandingQuads -= ((locationX, locationY))
        }

        dut.clock.step()
      }

      assert(outstandingQuads.isEmpty, "There are still outstanding quads at the end of the simulation.")
      assert(activeJobs.isEmpty, "There are still active jobs at the end of the simulation.")
    }
  }

  test("texture fetch") {
    simulate(new FragmentShaderConductor) { dut =>
      // Start a job
      for (_ <- 0 until cfg.shaderVectorLanes / Consts.pixelsPerQuad) {
        dut.io.sourceQuad.ready.expect(true)
        loadSourceQuad(dut, 0, 0, 15,
            Seq.fill(Consts.pixelsPerQuad)(Seq(0, 0)),
            Seq.fill(Consts.pixelsPerQuad)(0.0f))
        dut.io.textureFetchRequest.valid.expect(false)
      }

      dut.io.startJob.valid.expect(true)
      dut.io.startJob.ready.poke(true)
      dut.io.sourceQuad.ready.expect(true)
      val jobId = dut.io.startJob.bits.jobId.peek().litValue.toInt
      dut.clock.step()

      dut.io.textureFetchRequest.valid.expect(false)

      // Initiate a texture fetch by writing coordinates
      pokeShaderRegFloat(dut, jobId, 4, Seq.tabulate(cfg.shaderVectorLanes)(i => 1.0f + i)) // s
      pokeShaderRegFloat(dut, jobId, 5, Seq.tabulate(cfg.shaderVectorLanes)(i => 2.0f + i)) // t
      dut.clock.step()

      // Ensure the texel requests are delivered to the interface
      var requestId = 0
      for (j <- 0 until 4) {
        dut.io.textureFetchRequest.ready.poke(true)
        dut.io.textureFetchRequest.valid.expect(true)
        if (j == 0) {
          requestId = dut.io.textureFetchRequest.bits.requestId.peek().litValue.toInt
        }

        for (i <- 0 until 4) {
          dut.io.textureFetchRequest.bits.coord(0)(i).expect(1.0f + (j * 4 + i))
          dut.io.textureFetchRequest.bits.coord(1)(i).expect(2.0f + (j * 4 + i))
        }

        dut.clock.step()
      }

      // Case 1: the texel is returned before we try to read it. Read does not block.
      dut.io.textureFetchResponse.valid.poke(true)
      for (j <- 0 until 4) {
        dut.io.textureFetchResponse.bits.requestId.poke(requestId + (j << 4))
        for (i <- 0 until 4) {
          for (colorChannel <- 0 until Color.numChannels) {
            dut.io.textureFetchResponse.bits.texels(colorChannel)(i).poke(300.0f + (j * 4 + i) + 10 * colorChannel)
          }
        }

        dut.clock.step()
      }

      dut.io.textureFetchResponse.valid.poke(false)

      // Read the registers, which will be available without blocking
      for (colorChannel <- 0 until Color.numChannels) {
        assert(readRegisterFloat(dut, jobId, 3 + colorChannel) == Seq.tabulate(cfg.shaderVectorLanes)(i => 300.0f + i + 10 * colorChannel))
      }

      // Case 2: the texel is read before the texture fetch response arrives. The
      // caller needs to block.
      pokeShaderRegFloat(dut, jobId, 4, Seq.tabulate(cfg.shaderVectorLanes)(i => 1.0f + i)) // s
      pokeShaderRegFloat(dut, jobId, 5, Seq.tabulate(cfg.shaderVectorLanes)(i => 2.0f + i)) // t
      dut.clock.step()

      // Initiate a register read
      dut.io.shaderRegRead.valid.poke(true)
      dut.io.shaderRegRead.bits.jobId.poke(jobId)
      dut.io.shaderRegRead.bits.addr.poke(3) // Read the first color channel
      dut.clock.step()
      dut.io.shaderRegRead.valid.poke(false)

      // At this point, the read should be blocked because the texture fetch response has not arrived yet.
      assert(!dut.io.shaderRegReadData.valid.peek().litToBoolean)

      dut.clock.step()

      // Now, provide the texture fetch response
      dut.io.textureFetchResponse.valid.poke(true)
      for (j <- 0 until 4) {
        dut.io.textureFetchResponse.bits.requestId.poke(requestId + (j << 4))
        for (i <- 0 until 4) {
          for (colorChannel <- 0 until Color.numChannels) {
            dut.io.textureFetchResponse.bits.texels(colorChannel)(i).poke(300.0f +(j * 4 + i) + 10 * colorChannel)
          }
        }

        if (j == 3) {
          // Ensure we get an ioWake
          dut.io.ioWakeJob.valid.expect(true)
          dut.io.ioWakeJob.bits.expect(requestId)
        }

        dut.clock.step()
      }

      dut.io.textureFetchResponse.valid.poke(false)

      dut.clock.step()
      dut.io.textureFetchResponse.valid.poke(false)

      // The read should now succeed
      assert(readRegisterFloat(dut, jobId, 3) == Seq.tabulate(cfg.shaderVectorLanes)(i => 300.0f + i))

      // Run a few more cycles to ensure we don't get any asserts
      for (_ <- 0 until 10) {
        dut.clock.step()
      }
    }
  }

  // Edge case: a read finishes the same cycle a read is attempted. Ensure
  // we send the result.
  test("texture fetch read bypass") {
    simulate(new FragmentShaderConductor) { dut =>
      // Start a job
      for (_ <- 0 until cfg.shaderVectorLanes / Consts.pixelsPerQuad) {
        dut.io.sourceQuad.ready.expect(true)
        loadSourceQuad(dut, 0, 0, 15,
            Seq.fill(Consts.pixelsPerQuad)(Seq(0, 0)),
            Seq.fill(Consts.pixelsPerQuad)(0.0f))
        dut.io.textureFetchRequest.valid.expect(false)
      }

      dut.io.startJob.valid.expect(true)
      dut.io.startJob.ready.poke(true)
      dut.io.sourceQuad.ready.expect(true)
      val jobId = dut.io.startJob.bits.jobId.peek().litValue.toInt
      dut.clock.step()

      // Initiate a texture fetch by writing coordinates
      pokeShaderRegFloat(dut, jobId, 4, Seq.tabulate(cfg.shaderVectorLanes)(i => 1.0f + i)) // s
      pokeShaderRegFloat(dut, jobId, 5, Seq.tabulate(cfg.shaderVectorLanes)(i => 2.0f + i )) // t
      dut.clock.step()

      dut.io.textureFetchRequest.ready.poke(true)
      dut.io.textureFetchRequest.valid.expect(true)
      val requestId = dut.io.textureFetchRequest.bits.requestId.peek().litValue.toInt
      dut.clock.step()

      dut.io.textureFetchResponse.valid.poke(true)
      for (j <- 0 until 4) {
        dut.io.textureFetchResponse.bits.requestId.poke(requestId + (j << 4))
        for (i <- 0 until 4) {
          for (colorChannel <- 0 until Color.numChannels) {
            dut.io.textureFetchResponse.bits.texels(colorChannel)(i).poke((300.0f + j * 4 + i) + 10 * colorChannel)
          }
        }

        // Don't run the last clock after the final response.
        if (j < 3) {
          dut.clock.step()
        }
      }

      // Initiate a read the same cycle the last response comes back
      dut.io.shaderRegRead.valid.poke(true)
      dut.io.shaderRegRead.bits.jobId.poke(jobId)
      dut.io.shaderRegRead.bits.addr.poke(3) // Read the first color channel
      dut.clock.step()

      // No wake, no sleep, just ensure pixels are valid
      dut.io.ioWakeJob.valid.expect(false)
      dut.io.shaderRegReadData.valid.expect(true)
      for (i <- 0 until cfg.shaderVectorLanes) {
        dut.io.shaderRegReadData.bits(i).expect(java.lang.Float.floatToRawIntBits(300.0f + i))
      }
    }
  }

  test("read varying") {
    simulate(new FragmentShaderConductor) { dut =>
      // Write varyings (triangles/varying coefficients)
      val coeffs = Seq(
        Seq(1.0f, 2.0f),
        Seq(3.0f, 4.0f),
      )

      for (triangle <- coeffs.indices) {
        for (index <- coeffs(triangle).indices) {
          writeVaryingCoeff(dut, triangle, index, coeffs(triangle)(index))
        }
      }

      // Start a few jobs
      for (triangleId <- 0 until 2) {
        dut.io.sourceQuad.ready.expect(true)
        for (_ <- 0 until cfg.shaderVectorLanes / Consts.pixelsPerQuad) {
          loadSourceQuad(dut, 0, 0, 15,
              Seq.fill(Consts.pixelsPerQuad)(Seq(0, 0)),
              Seq.fill(Consts.pixelsPerQuad)(0.0f),
              triangleId)
        }
      }

      dut.io.startJob.valid.expect(true)
      dut.io.startJob.ready.poke(true)
      dut.io.sourceQuad.ready.expect(true)
      val jobId1 = dut.io.startJob.bits.jobId.peek().litValue.toInt
      dut.clock.step()
      dut.io.sourceQuad.ready.expect(true)
      val jobId2 = dut.io.startJob.bits.jobId.peek().litValue.toInt
      dut.clock.step()
      dut.io.startJob.valid.expect(false)

      // Now read the varying registers
      val got1 = readRegister(dut, jobId1, 2)
      val expect1 = Seq.fill(cfg.shaderVectorLanes)(java.lang.Float.floatToRawIntBits(1.0f))
      assert(got1 == expect1)
      val got2 = readRegister(dut, jobId1, 2)
      val expect2 = Seq.fill(cfg.shaderVectorLanes)(java.lang.Float.floatToRawIntBits(2.0f))
      assert(got2 == expect2)

      val got3 = readRegister(dut, jobId2, 2)
      val expect3 = Seq.fill(cfg.shaderVectorLanes)(java.lang.Float.floatToRawIntBits(3.0f))
      assert(got3 == expect3)
      val got4 = readRegister(dut, jobId2, 2)
      val expect4 = Seq.fill(cfg.shaderVectorLanes)(java.lang.Float.floatToRawIntBits(4.0f))
      assert(got4 == expect4)
    }
  }
}
