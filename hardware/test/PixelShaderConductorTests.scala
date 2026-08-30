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

  def loadRasterizedQuad(dut: PixelShaderConductor, x: Int, y: Int, mask: Int,
    lambda: Seq[Seq[Int]]): Unit = {
    dut.io.rasterizedQuad.valid.poke(true)
    dut.io.rasterizedQuad.bits.location.x.poke(x)
    dut.io.rasterizedQuad.bits.location.y.poke(y)
    dut.io.rasterizedQuad.bits.mask.poke(mask)
    for (i <- lambda.indices) {
      for (j <- lambda(i).indices) {
        dut.io.rasterizedQuad.bits.lambda(i)(j).raw.poke(lambda(i)(j))
      }
    }
    dut.clock.step()
    dut.io.rasterizedQuad.valid.poke(false)
  }

  def drainShadedQuad(dut: PixelShaderConductor, expectedX: Int,
    expectedY: Int, expectedMask: Int, colors: Seq[Seq[Int]]): Unit = {
    dut.io.shadedQuad.bits.location.x.expect(expectedX)
    dut.io.shadedQuad.bits.location.y.expect(expectedY)
    dut.io.shadedQuad.bits.mask.expect(expectedMask)
    dut.io.shadedQuad.valid.expect(true)
    for (i <- colors.indices) {
      for (j <- colors(i).indices) {
        dut.io.shadedQuad.bits.colors(i)(j).raw.expect(colors(i)(j))
      }
    }
    dut.clock.step()
  }

  def readRegister(dut: PixelShaderConductor, tag: Int, addr: Int): Seq[Int] = {
    dut.io.shaderRegRead.valid.poke(true)
    dut.io.shaderRegRead.bits.tag.poke(tag)
    dut.io.shaderRegRead.bits.addr.poke(addr)
    dut.clock.step()
    val result = Seq.tabulate(cfg.shaderVectorLanes)(i =>
      (dut.io.shaderRegReadData(i).peek().litValue & 0xffffffff).toInt)
    dut.io.shaderRegRead.valid.poke(false)
    result
  }

  def writeRegister(dut: PixelShaderConductor, tag: Int, addr: Int, data: Seq[Int]): Unit = {
    dut.io.shaderRegWrite.valid.poke(true)
    dut.io.shaderRegWrite.bits.tag.poke(tag)
    dut.io.shaderRegWrite.bits.addr.poke(addr)
    for (i <- data.indices) {
      dut.io.shaderRegWrite.bits.data(i).poke(data(i) & 0xffffffff)
    }
    dut.clock.step()
    dut.io.shaderRegWrite.valid.poke(false)
  }

  test("PixelShaderConductor basic operation") {
    simulate(new PixelShaderConductor) { dut =>
      dut.io.idle.expect(true)

      // Fill
      val masks = Seq.tabulate(cfg.shaderVectorLanes / Consts.pixelsPerQuad)(i => 10 + i)
      for (i <- 0 until cfg.shaderVectorLanes / Consts.pixelsPerQuad) {
        dut.io.startJob.valid.expect(false)
        dut.io.rasterizedQuad.ready.expect(true)
        val base = i * Consts.pixelsPerQuad
        loadRasterizedQuad(dut, 3, 4, masks(i),
          Seq.tabulate(Consts.pixelsPerQuad)(j => Seq(1000 + base + j, 2000 + base + j)))
        dut.io.idle.expect(false)
      }

      // Process
      dut.io.startJob.valid.expect(true)
      dut.io.startJob.ready.poke(true)
      dut.io.rasterizedQuad.ready.expect(true)
      val tag = dut.io.startJob.bits.tag.peek().litValue.toInt
      dut.clock.step()
      dut.io.startJob.valid.expect(false)
      dut.io.idle.expect(false)

      // Read/write registers
      assert(readRegister(dut, tag, 0) ==
        Seq.tabulate(cfg.shaderVectorLanes)(i => 1000 + i)) // lambda 0
      assert(readRegister(dut, tag, 1) ==
        Seq.tabulate(cfg.shaderVectorLanes)(i => 2000 + i)) // lambda 1

      writeRegister(dut, tag, 0, Seq.tabulate(cfg.shaderVectorLanes)(i => i + 100)) // red
      writeRegister(dut, tag, 1, Seq.tabulate(cfg.shaderVectorLanes)(i => i + 200)) // blue
      writeRegister(dut, tag, 2, Seq.tabulate(cfg.shaderVectorLanes)(i => i + 300)) // green
      writeRegister(dut, tag, 3, Seq.tabulate(cfg.shaderVectorLanes)(i => i + 400)) // alpha

      dut.io.jobFinished.valid.poke(true)
      dut.io.jobFinished.bits.poke(tag)
      dut.clock.step()
      dut.io.jobFinished.valid.poke(false)

      // Drain
      for (i <- 0 until cfg.shaderVectorLanes / Consts.pixelsPerQuad) {
        dut.io.idle.expect(false)
        dut.io.startJob.valid.expect(false)
        dut.io.rasterizedQuad.ready.expect(true)
        drainShadedQuad(dut, 3, 4, masks(i), Seq.tabulate(Consts.pixelsPerQuad)(j =>
          Seq(i * 4 + j + 100, i * 4 + j + 200, i * 4 + j + 300, i * 4 + j + 400)))
      }

      dut.io.idle.expect(true)
      dut.io.shadedQuad.valid.expect(false)
    }
  }

  test("PixelShaderConductor flush") {
    simulate(new PixelShaderConductor) { dut =>
      dut.io.startJob.ready.poke(true)

      // Load one valid quad
      loadRasterizedQuad(dut, 3, 4, 15,
        Seq.tabulate(Consts.pixelsPerQuad)(i => Seq(i * 2 + 1, i * 2 + 2)))

      // Flush
      dut.io.flush.poke(true)
      for (_ <- 0 until (cfg.shaderVectorLanes / Consts.pixelsPerQuad) - 1) {
        dut.io.startJob.valid.expect(false)
        dut.clock.step()
      }

      dut.io.flush.poke(false)

      dut.io.startJob.ready.poke(true)
      dut.io.startJob.valid.expect(true)
      val tag = dut.io.startJob.bits.tag.peek().litValue.toInt
      dut.clock.step()

      // Finish processing
      dut.io.jobFinished.valid.poke(true)
      dut.io.jobFinished.bits.poke(tag)
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

      class ActiveJob(val tag: Int) {
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
          dut.io.rasterizedQuad.ready.peek().litToBoolean) {
          tilex += 1
          if (tilex >= WIDTH) {
            tilex = 0
            tiley += 1
          }

          dut.io.rasterizedQuad.valid.poke(true)
          dut.io.rasterizedQuad.bits.location.x.poke(tilex)
          dut.io.rasterizedQuad.bits.location.y.poke(tiley)
          dut.io.rasterizedQuad.bits.mask.poke(15)
          outstandingQuads += ((tilex, tiley))
        } else {
          dut.io.rasterizedQuad.valid.poke(false)
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
          dut.io.jobFinished.bits.poke(jobToFinish.tag)
        } else {
          dut.io.jobFinished.valid.poke(false)
        }

        // Assert ready if there are fewer than 8 active jobs
        val ready = activeJobs.size < 8
        dut.io.startJob.ready.poke(ready)

        // Note we do this after checking to end jobs so we don't try to finish
        // jobs the same cycle they start.
        if (dut.io.startJob.valid.peek().litToBoolean && ready) {
          val tag = dut.io.startJob.bits.tag.peek().litValue.toInt
          activeJobs += new ActiveJob(tag)
        }

        // Handle output pixels
        if (dut.io.shadedQuad.valid.peek().litToBoolean) {
          val locationX = dut.io.shadedQuad.bits.location.x.peek().litValue.toInt
          val locationY = dut.io.shadedQuad.bits.location.y.peek().litValue.toInt
          outstandingQuads -= ((locationX, locationY))
        }

        // We ONLY step the clock here. ONLY ONLY ONLY
        dut.clock.step()
      }

      assert(outstandingQuads.isEmpty, "There are still outstanding quads at the end of the simulation.")
      assert(activeJobs.isEmpty, "There are still active jobs at the end of the simulation.")
    }
  }
}

