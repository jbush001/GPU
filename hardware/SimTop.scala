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

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

// This is a bit of a hack, as the components won't connect exactly like this
// in a real configuration, but demonstrates things working end-to-end.
class SimTop extends Component {
  val io = new Bundle {
    val inputTriangle = slave(spinal.lib.Stream(new BoundingBox))
    val startFlush = in(Bool())
    val flushData = master(Stream(Bits(32 bits)))
    val flushBufferSel = in(RenderBufferId()) // depth or color buffer
    val vpi = master(new VertexParameterInterface())
  }

  val setup = new TriangleSetup
  val rasterizer = new Rasterizer
  val tileBuffer = new TileBuffer
  setup.io.output >> rasterizer.io.input
  io.vpi.readEn := setup.io.vpi.readEn
  io.vpi.readAddress := setup.io.vpi.readAddress
  setup.io.vpi.readData := io.vpi.readData


  val fillColors = Vec(RgbaColor(), 4)
  val normFactor = rasterizer.io.output.lambda(0)(0) + rasterizer.io.output.lambda(0)(1) + rasterizer.io.output.lambda(0)(2)
  for (pixel <- 0 until 4) {
    for (component <- 0 until 3) {
      fillColors(pixel).channels(component) := U(rasterizer.io.output.lambda(pixel)(component) * 255 / normFactor, 8 bits)
    }

    fillColors(pixel).channels(3) := 0xff
  }

  val fillDepth = U(0, GpuConfig.depthBits bits)

  rasterizer.io.output.ready := True // No wait
  tileBuffer.io.valid  := rasterizer.io.output.valid
  tileBuffer.io.quadLoc  := rasterizer.io.output.payload.location
  tileBuffer.io.mask   := rasterizer.io.output.payload.mask
  tileBuffer.io.colors := fillColors
  tileBuffer.io.depths := Vec.fill(4)(fillDepth)
  tileBuffer.io.clearColor := RgbaColor(0, 0, 0, 0)
  tileBuffer.io.clearDepth := U(0xffffff, GpuConfig.depthBits bits)
  tileBuffer.io.startFlush := io.startFlush
  tileBuffer.io.flushBufferSel := io.flushBufferSel
  tileBuffer.io.enableDepthCheck := True
  tileBuffer.io.enableDepthWrite := True
  tileBuffer.io.enableBlend := True
  io.flushData << tileBuffer.io.flushData
  setup.io.input << io.inputTriangle
}

object Simulation {
  def run() = {
    SimConfig.workspacePath("hardware/gen/simulation")
 //     .withFstWave
      .compile(new SimTop()).doSim { dut =>
      dut.io.startFlush #= false
      dut.io.inputTriangle.valid #= false
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.waitSampling(100)

      // Simulate vertex parameter memory
      val vpmData = Array(5, 7, 118, 49, 23, 110)

      fork {
        while (true) {
          dut.clockDomain.waitSampling()
          if (dut.io.vpi.readEn.toBoolean) {
            dut.io.vpi.readData #= vpmData(dut.io.vpi.readAddress.toInt)
          }
        }
      }

      // Run a flush to clear out the buffer initially
      flushBuffer(dut, None, 0, 0)

      val fbSize = 128
      val fbData = new Array[Int](fbSize * fbSize)
      for (tile <- 0 until 4) {
        val tileRow = tile / 2
        val tileColumn = tile % 2

        // Set up a triangle
        dut.io.inputTriangle.left #= tileColumn * GpuConfig.tileSizePixels
        dut.io.inputTriangle.top #= tileRow * GpuConfig.tileSizePixels
        dut.io.inputTriangle.right #= (tileColumn + 1) * GpuConfig.tileSizePixels - 2
        dut.io.inputTriangle.bottom #= (tileRow + 1) * GpuConfig.tileSizePixels - 2

        dut.io.inputTriangle.valid #= true
        dut.clockDomain.waitSampling()
        dut.io.inputTriangle.valid #= false

        // Render stuff. Note that we don't check for completion, just run for
        // enough cycles we know it should finish.
        for (_ <- 0 until 1500) {
          dut.clockDomain.waitSampling()
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
    dut.io.startFlush #= true
    dut.io.flushBufferSel #= RenderBufferId.Color
    dut.clockDomain.waitSampling()
    dut.io.startFlush #= false
    dut.io.flushData.ready #= true

    var fbIndex = start

    for (_ <- 0 until GpuConfig.tileSizePixels) {
      for (_ <- 0 until GpuConfig.tileSizePixels) {
        dut.clockDomain.waitSampling()
        while (!(dut.io.flushData.valid.toBoolean &&
          dut.io.flushData.ready.toBoolean)) {
          dut.clockDomain.waitSampling()
        }

        out match {
          case Some(arr) => {
            arr(fbIndex) = dut.io.flushData.payload.toInt | 0xff000000 // Set alpha channel
          }
          case None => {}
        }

        fbIndex += 1
      }

      fbIndex += stride - GpuConfig.tileSizePixels
    }

    // Need to read the depth buffer in order to clear it.
    dut.io.startFlush #= true
    dut.io.flushBufferSel #= RenderBufferId.Depth
    dut.clockDomain.waitSampling()
    dut.io.startFlush #= false

    for (_ <- 0 until GpuConfig.tileSizePixels * GpuConfig.tileSizePixels) {
        dut.clockDomain.waitSampling()
        while (!(dut.io.flushData.valid.toBoolean &&
          dut.io.flushData.ready.toBoolean)) {
          dut.clockDomain.waitSampling()
        }
    }
  }
}
