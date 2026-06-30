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
import scala.collection.mutable.ArrayBuffer

// This is a bit of a hack, as the components won't connect exactly like this
// in a real configuration, but demonstrates things working end-to-end.
class SimTop extends Component {
  val io = new Bundle {
    val inputTriangle = slave(spinal.lib.Stream(new TriangleSetupParams))
    val startFlush = in(Bool())
    val flushData = master(Stream(Bits(32 bits)))
  }

  val setup = new TriangleSetup
  val rasterizer = new Rasterizer
  val tileBuffer = new TileBuffer
  setup.io.output >> rasterizer.io.input

  val fillColor = RgbaColor(0xff, 0, 0, 0xff)
  val fillDepth = U(0, GpuConfig.depthBits bits)

  rasterizer.io.output.ready := True // No wait
  tileBuffer.io.valid  := rasterizer.io.output.valid
  tileBuffer.io.quadX  := rasterizer.io.output.payload.x
  tileBuffer.io.quadY  := rasterizer.io.output.payload.y
  tileBuffer.io.mask   := rasterizer.io.output.payload.mask
  tileBuffer.io.colors := Vec.fill(4)(fillColor)
  tileBuffer.io.depths := Vec.fill(4)(fillDepth)
  tileBuffer.io.clearColor := RgbaColor(0, 0, 0, 0)
  tileBuffer.io.clearDepth := U(0xffffff, GpuConfig.depthBits bits)
  tileBuffer.io.startFlush := io.startFlush
  tileBuffer.io.flushBufferSel := RenderBufferId.Color
  io.flushData << tileBuffer.io.flushData
  setup.io.input << io.inputTriangle
}

object Simulation {
  def run() = {
    val sim = SimConfig.workspacePath("hardware/gen/simulation")
    if (System.getProperty("trace", "false").toBoolean) {
      sim.withFstWave
    } else {
      sim
    }.compile(new SimTop())
      .doSim { dut =>
        dut.io.startFlush #= false
        dut.io.inputTriangle.valid #= false
        dut.clockDomain.forkStimulus(period = 10)
        dut.clockDomain.waitSampling(100)

        // Run a resolve to clear out the buffer initially
        resolve(dut)

        // Set up a triangle
        dut.io.inputTriangle.bbLeft #= 0
        dut.io.inputTriangle.bbTop #= 0
        dut.io.inputTriangle.bbRight #= GpuConfig.tileSizeQuads
        dut.io.inputTriangle.bbBottom #= GpuConfig.tileSizeQuads
        dut.io.inputTriangle.x0 #= 5
        dut.io.inputTriangle.y0 #= 7
        dut.io.inputTriangle.x1 #= 61
        dut.io.inputTriangle.y1 #= 49
        dut.io.inputTriangle.x2 #= 23
        dut.io.inputTriangle.y2 #= 60
        dut.io.inputTriangle.valid #= true
        dut.clockDomain.waitSampling()
        dut.io.inputTriangle.valid #= false

        // Render stuff. Note that we don't check for completion, just run for
        // enough cycles we know it should finish.
        for (_ <- 0 until 1500) {
          dut.clockDomain.waitSampling()
        }

        // Read out the final data
        val fbData = resolve(dut)

        // Write an image file
        val canvas = new BufferedImage(GpuConfig.tileSizePixels, GpuConfig.tileSizePixels,
          BufferedImage.TYPE_INT_ARGB)
        canvas.setRGB(0, 0, GpuConfig.tileSizePixels, GpuConfig.tileSizePixels, fbData.toArray, 0,
          GpuConfig.tileSizePixels)
        val outputFile = new File("output.png")
        ImageIO.write(canvas, "png", outputFile)
    }
  }

  def resolve(dut: SimTop): Array[Int] = {
    val fbData = ArrayBuffer[Int]()
    dut.io.startFlush #= true
    dut.clockDomain.waitSampling()
    dut.io.startFlush #= false
    dut.io.flushData.ready #= true

    val totalPixels = GpuConfig.tileSizePixels * GpuConfig.tileSizePixels
    while (fbData.length < totalPixels) {
      dut.clockDomain.waitSampling()
      if (dut.io.flushData.valid.toBoolean &&
        dut.io.flushData.ready.toBoolean) {
        fbData += dut.io.flushData.payload.toInt | 0xff000000 // Set alpha channel
      }
    }

    fbData.toArray
  }
}
