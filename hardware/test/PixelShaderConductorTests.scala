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

class PixelShaderConductorTests extends AnyFunSuite with ChiselSim {
  implicit val cfg: GpuConfig = GpuConfig()

  def loadSourceQuad(dut: PixelShaderConductor, x: Int, y: Int, mask: Int,
    lambda: Seq[Seq[Int]], depths: Seq[Float],primitiveId: Int = 0): Unit = {
    dut.io.sourceQuad.valid.poke(true)
    dut.io.sourceQuad.bits.location.x.poke(x)
    dut.io.sourceQuad.bits.location.y.poke(y)
    dut.io.sourceQuad.bits.mask.poke(mask)
    dut.io.sourceQuad.bits.primitiveId.poke(primitiveId)
    for (i <- lambda.indices) {
      for (j <- lambda(i).indices) {
        dut.io.sourceQuad.bits.lambda(i)(j).raw.poke(lambda(i)(j))
      }
    }

    for (i <- depths.indices) {
      dut.io.sourceQuad.bits.depths(i).raw.poke(java.lang.Float.floatToRawIntBits(depths(i)))
    }

    dut.clock.step()
    dut.io.sourceQuad.valid.poke(false)
  }

  def drainShadedQuad(dut: PixelShaderConductor, expectedX: Int,
    expectedY: Int, expectedMask: Int, colors: Seq[Seq[Int]], depths: Seq[Float]): Unit = {
    dut.io.shadedQuad.bits.location.x.expect(expectedX)
    dut.io.shadedQuad.bits.location.y.expect(expectedY)
    dut.io.shadedQuad.bits.mask.expect(expectedMask)
    dut.io.shadedQuad.valid.expect(true)
    for (i <- colors.indices) {
      for (j <- colors(i).indices) {
        dut.io.shadedQuad.bits.colors(i)(j).raw.expect(colors(i)(j))
      }
    }

    for (i <- depths.indices) {
      dut.io.shadedQuad.bits.depths(i).raw.expect(java.lang.Float.floatToRawIntBits(depths(i)))
    }

    dut.clock.step()
  }

  def readRegister(dut: PixelShaderConductor, jobId: Int, addr: Int): Seq[Int] = {
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

  def pokeShaderReg(dut: PixelShaderConductor, jobId: Int, addr: Int, data: Seq[Int]): Unit = {
    dut.io.shaderRegWrite.valid.poke(true)
    dut.io.shaderRegWrite.bits.jobId.poke(jobId)
    dut.io.shaderRegWrite.bits.addr.poke(addr)
    for (i <- data.indices) {
      dut.io.shaderRegWrite.bits.data(i).poke(data(i) & 0xffffffff)
    }
    dut.clock.step()
    dut.io.shaderRegWrite.valid.poke(false)
  }

  def writeVaryingCoeff(dut: PixelShaderConductor, primitiveId: Int, index: Int, data: Float): Unit = {
    dut.io.writeVaryingCoeff.valid.poke(true)
    dut.io.writeVaryingCoeff.bits.primitiveId.poke(primitiveId)
    dut.io.writeVaryingCoeff.bits.index.poke(index)
    dut.io.writeVaryingCoeff.bits.value.raw.poke(java.lang.Float.floatToRawIntBits(data) & 0xffffffff)
    dut.clock.step()
    dut.io.writeVaryingCoeff.valid.poke(false)
  }

  test("PixelShaderConductor basic operation") {
    simulate(new PixelShaderConductor) { dut =>
      dut.io.idle.expect(true)

      // Fill
      val masks = Seq.tabulate(cfg.shaderVectorLanes / Consts.pixelsPerQuad)(i => 10 + i)
      for (i <- 0 until cfg.shaderVectorLanes / Consts.pixelsPerQuad) {
        dut.io.startJob.valid.expect(false)
        dut.io.sourceQuad.ready.expect(true)
        val base = i * Consts.pixelsPerQuad
        loadSourceQuad(dut, i + 3, i + 4, masks(i),
          Seq.tabulate(Consts.pixelsPerQuad)(j => Seq(1000 + base + j, 2000 + base + j)),
          Seq(1.0f, 2.0f, 3.0f, 4.0f))
        dut.io.idle.expect(false)
      }

      // Process
      dut.io.startJob.valid.expect(true)
      dut.io.startJob.ready.poke(true)
      dut.io.sourceQuad.ready.expect(true)
      val jobId = dut.io.startJob.bits.jobId.peek().litValue.toInt
      dut.clock.step()
      dut.io.startJob.valid.expect(false)
      dut.io.idle.expect(false)

      // Read/write registers
      assert(readRegister(dut, jobId, 0) ==
        Seq.tabulate(cfg.shaderVectorLanes)(i => 1000 + i)) // lambda 0
      assert(readRegister(dut, jobId, 1) ==
        Seq.tabulate(cfg.shaderVectorLanes)(i => 2000 + i)) // lambda 1

      pokeShaderReg(dut, jobId, 0, Seq.tabulate(cfg.shaderVectorLanes)(i => i + 100)) // red
      pokeShaderReg(dut, jobId, 1, Seq.tabulate(cfg.shaderVectorLanes)(i => i + 200)) // blue
      pokeShaderReg(dut, jobId, 2, Seq.tabulate(cfg.shaderVectorLanes)(i => i + 300)) // green
      pokeShaderReg(dut, jobId, 3, Seq.tabulate(cfg.shaderVectorLanes)(i => i + 400)) // alpha

      dut.io.jobFinished.valid.poke(true)
      dut.io.jobFinished.bits.poke(jobId)
      dut.clock.step()
      dut.io.jobFinished.valid.poke(false)

      // Drain
      for (i <- 0 until cfg.shaderVectorLanes / Consts.pixelsPerQuad) {
        dut.io.idle.expect(false)
        dut.io.startJob.valid.expect(false)
        dut.io.sourceQuad.ready.expect(true)
        drainShadedQuad(dut, i + 3, i + 4, masks(i), Seq.tabulate(Consts.pixelsPerQuad)(j =>
          Seq(i * 4 + j + 100, i * 4 + j + 200, i * 4 + j + 300, i * 4 + j + 400)),
          Seq(1.0f, 2.0f, 3.0f, 4.0f))
      }

      dut.io.idle.expect(true)
      dut.io.shadedQuad.valid.expect(false)
    }
  }

  test("PixelShaderConductor flush") {
    simulate(new PixelShaderConductor) { dut =>
      dut.io.startJob.ready.poke(true)

      // Load one valid quad
      loadSourceQuad(dut, 3, 4, 15,
        Seq.tabulate(Consts.pixelsPerQuad)(i => Seq(i * 2 + 1, i * 2 + 2)),
        Seq.fill(Consts.pixelsPerQuad)(0.0f))

      // Flush
      dut.io.flush.poke(true)
      for (_ <- 0 until (cfg.shaderVectorLanes / Consts.pixelsPerQuad) - 1) {
        dut.io.startJob.valid.expect(false)
        dut.clock.step()
      }

      dut.io.flush.poke(false)

      dut.io.startJob.ready.poke(true)
      dut.io.startJob.valid.expect(true)
      val jobId = dut.io.startJob.bits.jobId.peek().litValue.toInt
      dut.clock.step()

      // Finish processing
      dut.io.jobFinished.valid.poke(true)
      dut.io.jobFinished.bits.poke(jobId)
      dut.clock.step()
      dut.io.jobFinished.valid.poke(false)

      // Valid pixel
      dut.io.shadedQuad.valid.expect(true)
      dut.io.shadedQuad.bits.location.x.expect(3)
      dut.io.shadedQuad.bits.location.y.expect(4)
      dut.io.shadedQuad.bits.mask.expect(15)
      dut.io.idle.expect(false)
      dut.clock.step()

      // Null quad with zero mask
      for (_ <- 0 until (cfg.shaderVectorLanes / Consts.pixelsPerQuad) - 1) {
        dut.io.idle.expect(false)
        dut.io.shadedQuad.valid.expect(true)
        dut.io.shadedQuad.bits.mask.expect(0)
        dut.clock.step()
      }

      dut.io.shadedQuad.valid.expect(false)
      dut.io.idle.expect(true)
    }
  }

  // Checks that job states are handled properly.
  // TODO: need to perform register accesses to ensure writes
  // and reads are propagaged.
  test("PixelShaderConductor stress") {
    simulate(new PixelShaderConductor) { dut =>
      val rng = new scala.util.Random

      var tilex = 1
      var tiley = 1
      val WIDTH = 100

      class ActiveJob(val jobId: Int) {
        var cyclesLeft = rng.nextInt(20) + 10
      }

      val activeJobs = scala.collection.mutable.Set[ActiveJob]()
      val outstandingQuads = scala.collection.mutable.Set[(Int, Int)]()

      val maxCycles = 1000
      val flushCycles = 100
      for (cycle <- 0 until maxCycles) {
        val flush = cycle >= maxCycles - flushCycles
        if (flush) {
          dut.io.flush.poke(true)
        }

        // Check on loading new rasterized quads
        if (cycle < maxCycles - flushCycles &&
          dut.io.sourceQuad.ready.peek().litToBoolean) {
          tilex += 1
          if (tilex >= WIDTH) {
            tilex = 0
            tiley += 1
          }

          dut.io.sourceQuad.valid.poke(true)
          dut.io.sourceQuad.bits.location.x.poke(tilex)
          dut.io.sourceQuad.bits.location.y.poke(tiley)
          dut.io.sourceQuad.bits.mask.poke(15)
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

  test("PixelShaderConductor texture fetch") {
    simulate(new PixelShaderConductor) { dut =>
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
      pokeShaderReg(dut, jobId, 4, Seq.tabulate(cfg.shaderVectorLanes)(i => i + 100)) // s
      pokeShaderReg(dut, jobId, 5, Seq.tabulate(cfg.shaderVectorLanes)(i => i + 200)) // t
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
          dut.io.textureFetchRequest.bits.coord(0)(i).raw.expect((j * 4 + i) + 100)
          dut.io.textureFetchRequest.bits.coord(1)(i).raw.expect((j * 4 + i) + 200)
        }

        dut.clock.step()
      }

      // Case 1: the texel is returned before we try to read it. Read does not block.
      dut.io.textureFetchResponse.valid.poke(true)
      for (j <- 0 until 4) {
        dut.io.textureFetchResponse.bits.requestId.poke(requestId + (j << 4))
        for (i <- 0 until 4) {
          for (colorChannel <- 0 until Color.numChannels) {
            dut.io.textureFetchResponse.bits.texels(colorChannel)(i).raw.poke((j * 4 + i) + 300 + 10 * colorChannel)
          }
        }

        dut.clock.step()
      }

      dut.io.textureFetchResponse.valid.poke(false)

      // Read the registers, which will be available without blocking
      for (colorChannel <- 0 until Color.numChannels) {
        assert(readRegister(dut, jobId, 3 + colorChannel) == Seq.tabulate(cfg.shaderVectorLanes)(i => i + 300 + 10 * colorChannel))
      }

      // Case 2: the texel is read before the texture fetch response arrives. The
      // caller needs to block.
      pokeShaderReg(dut, jobId, 4, Seq.tabulate(cfg.shaderVectorLanes)(i => i + 100)) // s
      pokeShaderReg(dut, jobId, 5, Seq.tabulate(cfg.shaderVectorLanes)(i => i + 200)) // t
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
            dut.io.textureFetchResponse.bits.texels(colorChannel)(i).raw.poke((j * 4 + i) + 300 + 10 * colorChannel)
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
      assert(readRegister(dut, jobId, 3) == Seq.tabulate(cfg.shaderVectorLanes)(i => i + 300))

      // Run a few more cycles to ensure we don't get any asserts
      for (_ <- 0 until 10) {
        dut.clock.step()
      }
    }
  }

  // Edge case: a read finishes the same cycle a read is attempted. Ensure
  // we send the result.
  test("PixelShaderConductor texture fetch read bypass") {
    simulate(new PixelShaderConductor) { dut =>
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
      pokeShaderReg(dut, jobId, 4, Seq.tabulate(cfg.shaderVectorLanes)(i => i + 100)) // s
      pokeShaderReg(dut, jobId, 5, Seq.tabulate(cfg.shaderVectorLanes)(i => i + 200)) // t
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
            dut.io.textureFetchResponse.bits.texels(colorChannel)(i).raw.poke((j * 4 + i) + 300 + 10 * colorChannel)
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
        dut.io.shaderRegReadData.bits(i).expect(i + 300)
      }
    }
  }

  test("PixelShaderConductor read varying") {
    simulate(new PixelShaderConductor) { dut =>
      // Write varyings
      val coeffs = Seq(
        Seq(1.0f, 2.0f),
        Seq(3.0f, 4.0f),
        Seq(5.0f, 6.0f),
        Seq(7.0f, 8.0f)
      )

      for (primitive <- coeffs.indices) {
        for (index <- coeffs(primitive).indices) {
          writeVaryingCoeff(dut, primitive, index, coeffs(primitive)(index))
        }
      }

      // Start a job
      for (primitiveId <- 0 until cfg.shaderVectorLanes / Consts.pixelsPerQuad) {
        dut.io.sourceQuad.ready.expect(true)
        loadSourceQuad(dut, 0, 0, 15,
            Seq.fill(Consts.pixelsPerQuad)(Seq(0, 0)),
            Seq.fill(Consts.pixelsPerQuad)(0.0f),
            primitiveId)
        dut.io.textureFetchRequest.valid.expect(false)
      }

      dut.io.startJob.valid.expect(true)
      dut.io.startJob.ready.poke(true)
      dut.io.sourceQuad.ready.expect(true)
      val jobId = dut.io.startJob.bits.jobId.peek().litValue.toInt
      dut.clock.step()

      // Now read the varying registers
      val got1 = readRegister(dut, jobId, 2)
      val expect1 = Seq(
        1.0f, 1.0f, 1.0f, 1.0f, 3.0f, 3.0f, 3.0f, 3.0f, 5.0f, 5.0f, 5.0f, 5.0f, 7.0f, 7.0f, 7.0f, 7.0f
      )

      dut.clock.step(10)
      assert(got1 == expect1.map(x => java.lang.Float.floatToRawIntBits(x)))

      val got2 = readRegister(dut, jobId, 2)
      val expect2 = Seq(
        2.0f, 2.0f, 2.0f, 2.0f, 4.0f, 4.0f, 4.0f, 4.0f, 6.0f, 6.0f, 6.0f, 6.0f, 8.0f, 8.0f, 8.0f, 8.0f
      )

      assert(got2 == expect2.map(x => java.lang.Float.floatToRawIntBits(x)))
    }
  }
}
