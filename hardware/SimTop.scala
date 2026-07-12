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

import chisel3._
import chisel3.util._
import chisel3.simulator.EphemeralSimulator._
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

// This is a bit of a hack, as the components won't connect exactly like this
// in a real configuration, but demonstrates things working end-to-end.
class SimTop extends Module {
  val io = IO(new Bundle {
    val inputTriangle = Flipped(Decoupled(new BoundingBox))
    val startFlush = Input(Bool())
    val flushData = Decoupled(Bits(32.W))
    val flushBufferSel = Input(RenderBufferId()) // depth or color buffer
    val vpi = new VertexParameterInterface()
  })

  val setup = Module(new TriangleSetup)
  val rasterizer = Module(new Rasterizer)
  val tileBuffer = Module(new TileBuffer)
  setup.io.output <> rasterizer.io.input
  io.vpi.readEn := setup.io.vpi.readEn
  io.vpi.readAddress := setup.io.vpi.readAddress
  setup.io.vpi.readData := io.vpi.readData

  val fillColors = Wire(Vec(4, new RgbaColor))
  val normFactor = rasterizer.io.output.bits.lambda(0)(0) + rasterizer.io.output.bits.lambda(0)(1) + rasterizer.io.output.bits.lambda(0)(2)
  for (pixel <- 0 until 4) {
    for (component <- 0 until 3) {
      fillColors(pixel).channels(component) := (rasterizer.io.output.bits.lambda(pixel)(component) * 255.S / normFactor).asUInt
    }

    fillColors(pixel).channels(3) := 0xff.U
  }

  val fillDepth = 0.U(GpuConfig.depthBits.W)

  rasterizer.io.output.ready := true.B // No wait
  tileBuffer.io.valid := rasterizer.io.output.valid
  tileBuffer.io.quadLoc := rasterizer.io.output.bits.location
  tileBuffer.io.mask := rasterizer.io.output.bits.mask
  tileBuffer.io.colors := fillColors
  tileBuffer.io.depths := VecInit.fill(4)(fillDepth)
  tileBuffer.io.clearColor.channels(0) := 0.U
  tileBuffer.io.clearColor.channels(1) := 0.U
  tileBuffer.io.clearColor.channels(2) := 0.U
  tileBuffer.io.clearColor.channels(3) := 0.U
  tileBuffer.io.clearDepth := 0xffffff.U(GpuConfig.depthBits.W)
  tileBuffer.io.startFlush := io.startFlush
  tileBuffer.io.flushBufferSel := io.flushBufferSel
  tileBuffer.io.enableDepthCheck := false.B
  tileBuffer.io.enableDepthWrite := true.B
  tileBuffer.io.enableBlend := false.B
  tileBuffer.io.flushData <> io.flushData
  setup.io.input <> io.inputTriangle
}

object Simulation {
  def run() = {
    simulate(new SimTop()) { dut =>
      dut.reset.poke(true.B)
      dut.io.startFlush.poke(false)
      dut.io.inputTriangle.valid.poke(false)
      dut.clock.step(5)
      dut.reset.poke(false.B)
      dut.clock.step(1)

      // Simulate vertex parameter memory
      val vpmData = Array(5, 7, 118, 49, 23, 110)

      // Run a flush to clear out the buffer initially
      flushBuffer(dut, None, 0, 0)

      val fbSize = 128
      val fbData = new Array[Int](fbSize * fbSize)
      for (tile <- 0 until 4) {
        val tileRow = tile / 2
        val tileColumn = tile % 2

        // Set up a triangle
        dut.io.inputTriangle.bits.left.poke(tileColumn * GpuConfig.tileSizePixels)
        dut.io.inputTriangle.bits.top.poke(tileRow * GpuConfig.tileSizePixels)
        dut.io.inputTriangle.bits.right.poke((tileColumn + 1) * GpuConfig.tileSizePixels - 2)
        dut.io.inputTriangle.bits.bottom.poke((tileRow + 1) * GpuConfig.tileSizePixels - 2)

        dut.io.inputTriangle.valid.poke(true)
        dut.clock.step()
        dut.io.inputTriangle.valid.poke(false)

        // Render stuff. Note that we don't check for completion, just run for
        // enough cycles we know it should finish.
        for (_ <- 0 until 1500) {
          if (dut.io.vpi.readEn.peek().litValue.toInt != 0) {
            val readData = vpmData(dut.io.vpi.readAddress.peek().litValue.toInt)
            dut.clock.step()
            dut.io.vpi.readData.poke(readData)
          } else {
            dut.clock.step()
          }
        }

        // Read out the final data
        val offset = (fbSize * GpuConfig.tileSizePixels * tileRow) +
          (GpuConfig.tileSizePixels * tileColumn)
        flushBuffer(dut, Some(fbData), offset, fbSize)
      }

      // Write an image file
      val canvas = new BufferedImage(fbSize, fbSize, BufferedImage.TYPE_INT_ARGB)
      canvas.setRGB(0, 0, fbSize, fbSize, fbData.toArray, 0, fbSize)
      val outputFile = new File("output.png")
      ImageIO.write(canvas, "png", outputFile)
    }
  }

  def flushBuffer(dut: SimTop, out: Option[Array[Int]], start: Int, stride: Int) = {
    dut.io.startFlush.poke(true)
    dut.io.flushBufferSel.poke(RenderBufferId.Color)
    dut.io.flushData.ready.poke(true)

    var fbIndex = start

    for (_ <- 0 until GpuConfig.tileSizePixels) {
      for (_ <- 0 until GpuConfig.tileSizePixels) {
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

      fbIndex += stride - GpuConfig.tileSizePixels
    }

    // Need to read the depth buffer in order to clear it.
    dut.io.startFlush.poke(true)
    dut.io.flushBufferSel.poke(RenderBufferId.Depth)

    for (_ <- 0 until GpuConfig.tileSizePixels * GpuConfig.tileSizePixels) {
      dut.clock.step()
      dut.io.startFlush.poke(false)
      while (dut.io.flushData.valid.peek().litValue.toLong == 0 ||
        dut.io.flushData.ready.peek().litValue.toLong == 0) {
        dut.clock.step()
      }
    }
  }
}
