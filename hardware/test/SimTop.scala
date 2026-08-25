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

package simulate

import chisel3._
import chisel3.util._
import chisel3.simulator.EphemeralSimulator._
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import gpu._

// This is a bit of a hack, as the components won't connect exactly like this
// in a real configuration, but demonstrates things working end-to-end.
class SimTop(implicit val cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val setupParams = Flipped(Decoupled(new RasterizerSetupParams))
    val startFlush = Input(Bool())
    val flushData = Decoupled(Bits(32.W))
    val flushBufferSel = Input(RenderBufferId()) // depth or color buffer
  })

  val rasterizer = Module(new Rasterizer)
  val tileBuffer = Module(new TileBuffer)

  val fillColors = Wire(Vec(4, new Color))
  for (pixel <- 0 until 4) {
    val lambda0 = rasterizer.io.quad.bits.lambda(pixel)(0)
    fillColors(pixel).channels(0) := ((rasterizer.io.quad.bits.lambda(pixel)(0) >> 6)
      .asUInt(Color.channelBits - 1, 0))
    val lambda1 = rasterizer.io.quad.bits.lambda(pixel)(1)
    fillColors(pixel).channels(1) := ((lambda1 >> 6)
      .asUInt(Color.channelBits - 1, 0))
    val lambda2 = 0x10000.S - (lambda0 + lambda1)
    fillColors(pixel).channels(2) := ((lambda2 >> 6)
      .asUInt(Color.channelBits - 1, 0))

    fillColors(pixel).channels(3) := 0x3ff.U
  }

  val fillDepth = 0.U(cfg.depthBufferBits.W)

  rasterizer.io.quad.ready := true.B // No wait
  tileBuffer.io.quad.valid := rasterizer.io.quad.valid
  tileBuffer.io.quad.bits.location := rasterizer.io.quad.bits.location
  tileBuffer.io.quad.bits.mask := rasterizer.io.quad.bits.mask
  tileBuffer.io.quad.bits.colors := fillColors
  tileBuffer.io.quad.bits.depths := VecInit.fill(4)(fillDepth)
  tileBuffer.io.clearColor.channels(0) := 0.U
  tileBuffer.io.clearColor.channels(1) := 0.U
  tileBuffer.io.clearColor.channels(2) := 0.U
  tileBuffer.io.clearColor.channels(3) := 0.U
  tileBuffer.io.clearDepth := 0xffffff.U(cfg.depthBufferBits.W)
  tileBuffer.io.startFlush := io.startFlush
  tileBuffer.io.flushBufferSel := io.flushBufferSel
  tileBuffer.io.enableDepthCheck := false.B
  tileBuffer.io.enableDepthWrite := true.B
  tileBuffer.io.enableBlend := false.B
  tileBuffer.io.flushData <> io.flushData
  rasterizer.io.setupParams <> io.setupParams
}

object Simulation extends App {
  implicit val cfg: GpuConfig = GpuConfig()

  simulate(new SimTop()) { dut =>
    dut.reset.poke(true.B)
    dut.io.startFlush.poke(false)
    dut.io.setupParams.valid.poke(false)
    dut.clock.step(5)
    dut.reset.poke(false.B)
    dut.clock.step(1)

    // Run a flush to clear out the buffer initially
    flushBuffer(dut, None, 0, 0)

    val fbSize = 128
    val fbData = new Array[Int](fbSize * fbSize)

    val vertices = Array((5, 7), (23, 110), (118, 49))

    for (tile <- 0 until 4) {
      val tileRow = tile / 2
      val tileColumn = tile % 2

      // Set up a triangle
      val tileLeft = tileColumn * cfg.tileSizePixels
      val tileTop = tileRow * cfg.tileSizePixels
      dut.io.setupParams.valid.poke(true)
      dut.io.setupParams.bits.boundingBox.left.poke(tileLeft)
      dut.io.setupParams.bits.boundingBox.top.poke(tileTop)
      dut.io.setupParams.bits.boundingBox.right.poke(tileLeft + cfg.tileSizePixels - 2)
      dut.io.setupParams.bits.boundingBox.bottom.poke(tileTop + cfg.tileSizePixels - 2)
      val rawParams = (0 until 3).map { i =>
        val (startX, startY) = vertices(i)
        val (endX, endY) = vertices((i + 1) % 3)
        val xs = endY - startY
        val ys = endX - startX
        val iv = ((tileLeft - startX) * xs - (tileTop - startY) * ys)
        (xs, -ys, iv)
      }

      val det = math.abs(rawParams.map(_._3).sum)

      rawParams.zipWithIndex.foreach { case ((xs, ys, iv), i) =>
        val normXs = (xs * 0x10000L / det).toInt
        val normYs = (ys * 0x10000L / det).toInt
        val normIv = (iv * 0x10000L / det).toInt

        dut.io.setupParams.bits.xStep(i).poke(normXs.S)
        dut.io.setupParams.bits.yStep(i).poke(normYs.S)
        dut.io.setupParams.bits.initialValue(i).poke(normIv.S)
      }

      while (dut.io.setupParams.ready.peek().litValue.toLong == 0) {
        dut.clock.step()
      }

      dut.clock.step()
      dut.io.setupParams.valid.poke(false)

      // Render stuff. Note that we don't check for completion, just run for
      // enough cycles we know it should finish.
      for (_ <- 0 until 1500) {
        dut.clock.step()
      }

      // Read out the final data
      val offset = (fbSize * cfg.tileSizePixels * tileRow) +
        (cfg.tileSizePixels * tileColumn)
      flushBuffer(dut, Some(fbData), offset, fbSize)
    }

    // Write an image file
    val canvas = new BufferedImage(fbSize, fbSize, BufferedImage.TYPE_INT_ARGB)
    canvas.setRGB(0, 0, fbSize, fbSize, fbData.toArray, 0, fbSize)
    val outputFile = new File("output.png")
    ImageIO.write(canvas, "png", outputFile)
    println("wrote output file to output.png")
  }

  def flushBuffer(dut: SimTop, out: Option[Array[Int]], start: Int, stride: Int) = {
    dut.io.startFlush.poke(true)
    dut.io.flushBufferSel.poke(RenderBufferId.Color)
    dut.io.flushData.ready.poke(true)

    var fbIndex = start

    for (_ <- 0 until cfg.tileSizePixels) {
      for (_ <- 0 until cfg.tileSizePixels) {
        dut.clock.step()
        dut.io.startFlush.poke(false)
        while (dut.io.flushData.valid.peek().litValue.toLong == 0 ||
          dut.io.flushData.ready.peek().litValue.toLong == 0) {
          dut.clock.step()
        }

        out match {
          case Some(arr) => {
            // Set alpha channel
            arr(fbIndex) = (dut.io.flushData.bits.peek().litValue.toLong | 0xff000000L).toInt
          }
          case None => {}
        }

        fbIndex += 1
      }

      fbIndex += stride - cfg.tileSizePixels
    }

    // Need to read the depth buffer in order to clear it.
    dut.io.startFlush.poke(true)
    dut.io.flushBufferSel.poke(RenderBufferId.Depth)

    for (_ <- 0 until cfg.totalTilePixels) {
      dut.clock.step()
      dut.io.startFlush.poke(false)
      while (dut.io.flushData.valid.peek().litValue.toLong == 0 ||
        dut.io.flushData.ready.peek().litValue.toLong == 0) {
        dut.clock.step()
      }
    }
  }
}
