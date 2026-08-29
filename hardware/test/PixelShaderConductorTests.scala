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
        dut.io.rasterizedQuad.bits.lambda(i)(j).poke(lambda(i)(j))
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
        dut.io.shadedQuad.bits.colors(i).channels(j).expect(colors(i)(j))
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

      // Fill
      dut.io.startJob.valid.expect(false)
      dut.io.rasterizedQuad.ready.expect(true)
      loadRasterizedQuad(dut, 3, 4, 15,
        Seq.tabulate(Consts.pixelsPerQuad)(i => Seq(i * 2 + 1, i * 2 + 2)))

      dut.io.startJob.valid.expect(false)
      dut.io.rasterizedQuad.ready.expect(true)
      loadRasterizedQuad(dut, 5, 6, 15,
        Seq.tabulate(Consts.pixelsPerQuad)(i => Seq(i * 2 +
        cfg.shaderVectorLanes + 1, i * 2 + cfg.shaderVectorLanes + 2)))

      // Process
      dut.io.startJob.valid.expect(true)
      dut.io.startJob.ready.poke(true)
      dut.io.rasterizedQuad.ready.expect(true)
      val tag = dut.io.startJob.bits.tag.peek().litValue.toInt
      dut.clock.step()
      dut.io.startJob.valid.expect(false)

      // Read/write registers
      assert(readRegister(dut, tag, 0) ==
        Seq.tabulate(cfg.shaderVectorLanes)(i => i * 2 + 1)) // lambda 0
      assert(readRegister(dut, tag, 1) ==
        Seq.tabulate(cfg.shaderVectorLanes)(i => i * 2 + 2)) // lambda 1

      writeRegister(dut, tag, 0, Seq.tabulate(cfg.shaderVectorLanes)(i => i + 100)) // red
      writeRegister(dut, tag, 1, Seq.tabulate(cfg.shaderVectorLanes)(i => i + 200)) // blue
      writeRegister(dut, tag, 2, Seq.tabulate(cfg.shaderVectorLanes)(i => i + 300)) // green
      writeRegister(dut, tag, 3, Seq.tabulate(cfg.shaderVectorLanes)(i => i + 400)) // alpha

      dut.io.jobFinished.valid.poke(true)
      dut.io.jobFinished.bits.poke(tag)
      dut.clock.step()
      dut.io.jobFinished.valid.poke(false)

      // Drain
      val expectColor1 = Seq.tabulate(Consts.pixelsPerQuad)(i =>
        Seq(i + 100, i + 200, i + 300, i + 400))
      val expectColor2 = Seq.tabulate(Consts.pixelsPerQuad)(i =>
        Seq(i + 104, i + 204, i + 304, i + 404))
      drainShadedQuad(dut, 3, 4, 15, expectColor1)
      drainShadedQuad(dut, 5, 6, 15, expectColor2)
      dut.io.shadedQuad.valid.expect(false)
    }
  }
}

