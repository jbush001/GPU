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
import gpu.shader._

// This is a bit of a hack, as the components won't connect exactly like this
// in a real configuration, but demonstrates things working end-to-end.
class SimTop(implicit val cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val dap = new DirectAccessPort
    val edgeCoeffs = Flipped(Decoupled(new EdgeCoeffs))
    val writeVaryingCoeff = Flipped(Valid(new Bundle {
      val index = UInt(5.W)
      val value = new Float32()
    }))
    val startFlush = Input(Bool())
    val flushData = Decoupled(Bits(32.W))
    val flushBufferSel = Input(RenderBufferId()) // depth or color buffer
    val complete = Output(Bool())
  })

  val rasterizer = Module(new Rasterizer)
  val tileBuffer = Module(new TileBuffer)
  val pixelShaderConductor = Module(new PixelShaderConductor)
  val shaderCore = Module(new ShaderCore)
  val memoryArbiter = Module(new MemoryArbiter(1, 1))
  val memory = Module(new SimAxiMemory(1024))
  val floatArrayToColor = Module(new FloatArrayToColor)

  io.complete := pixelShaderConductor.io.idle && rasterizer.io.complete

  rasterizer.io.quad <> pixelShaderConductor.io.rasterizedQuad
  pixelShaderConductor.io.flush := rasterizer.io.complete
  pixelShaderConductor.io.startJob <> shaderCore.io.startJob
  shaderCore.io.jobFinished <> pixelShaderConductor.io.jobFinished
  shaderCore.io.regRead <> pixelShaderConductor.io.shaderRegRead
  pixelShaderConductor.io.shaderRegReadData <> shaderCore.io.regReadData
  shaderCore.io.regWrite <> pixelShaderConductor.io.shaderRegWrite
  pixelShaderConductor.io.shadedQuad <> floatArrayToColor.io.floatQuad
  floatArrayToColor.io.shadedQuad <> tileBuffer.io.shadedQuad
  shaderCore.io.icacheReadPort <> memoryArbiter.io.readPorts(0)
  memoryArbiter.io.axiBus <> memory.io
  memory.dap <> io.dap
  memoryArbiter.io.writePorts(0).burst.valid := false.B
  memoryArbiter.io.writePorts(0).data.valid := false.B
  memoryArbiter.io.writePorts(0).burst.bits.address := 0.U
  memoryArbiter.io.writePorts(0).burst.bits.length := 0.U
  memoryArbiter.io.writePorts(0).data.bits := 0.U

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
  rasterizer.io.edgeCoeffs <> io.edgeCoeffs

  io.writeVaryingCoeff <> pixelShaderConductor.io.writeVaryingCoeff
}

object Simulation extends App {
  implicit val cfg: GpuConfig = GpuConfig()

  simulate(new SimTop()) { dut =>
    dut.reset.poke(true.B)
    dut.io.startFlush.poke(false)
    dut.io.edgeCoeffs.valid.poke(false)
    dut.clock.step(5)
    dut.reset.poke(false.B)
    dut.clock.step(1)

    val asm = new ShaderAssembler()
    asm
      .move(64, 96) // lambda0
      .rInst(OpCode.Mulf, 64, 98, 64) // dQ1 * lambda0
      .move(65, 97) // lambda1
      .rInst(OpCode.Mulf, 65, 98, 65) // dQ2 * lambda1
      .rInst(OpCode.Addf, 64, 64, 65) // (dQ1 * lambda0) + (dQ2 * lambda1)
      .rInst(OpCode.Addf, 64, 98, 64) // (dQ1 * lambda0) + (dQ2 * lambda1) + Q0

      .move(104, 64) // red = lambda0
      .move(105, 64) // green = lambda1
      .move(106, 64) // blue = lambda2
      .move(107, 60) // alpha = 1.0
      .halt()
    val programBytes = asm.finish()

    // Fill in memory
    SimMemAccess.write(dut.clock, dut.io.dap, 0, programBytes)

    // Run a flush to clear out the buffer initially
    flushBuffer(dut, None, 0, 0)

    val fbSize = 128
    val fbData = new Array[Int](fbSize * fbSize)

    val vertices = Array((5, 7), (23, 110), (118, 49))

    setUpVarying(dut, (0.3f, 0.411504425f, 1.0f))

    for (tile <- 0 until 4) {
      val tileRow = tile / 2
      val tileColumn = tile % 2

      // Set up a triangle
      val tileLeft = tileColumn * cfg.tileSizePixels
      val tileTop = tileRow * cfg.tileSizePixels
      setUpRasterizer(dut, vertices, tileLeft, tileTop)

      var totalCycles = 0
      while (!dut.io.complete.peek().litToBoolean) {
        dut.clock.step()
        totalCycles += 1
      }

      println(s"Completed tile in $totalCycles cycles")

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

  def floatToRawBits(fval: Float) = java.lang.Float.floatToIntBits(fval) & 0xffffffffL

  var nextVaryingCoeffWrite = 0

  def setUpVarying(dut: SimTop, values: (Float, Float, Float)): Unit = {
    dut.io.writeVaryingCoeff.valid.poke(true)
    dut.io.writeVaryingCoeff.bits.index.poke(nextVaryingCoeffWrite)
    nextVaryingCoeffWrite += 1
    dut.io.writeVaryingCoeff.bits.value.raw.poke(floatToRawBits(values._2 - values._1)) // dQ1
    dut.clock.step()
    dut.io.writeVaryingCoeff.bits.index.poke(nextVaryingCoeffWrite)
    nextVaryingCoeffWrite += 1
    dut.io.writeVaryingCoeff.bits.value.raw.poke(floatToRawBits(values._3 - values._1)) // dQ2
    dut.clock.step()
    dut.io.writeVaryingCoeff.bits.index.poke(nextVaryingCoeffWrite)
    nextVaryingCoeffWrite += 1
    dut.io.writeVaryingCoeff.bits.value.raw.poke(floatToRawBits(values._1))
    dut.clock.step()
  }

  def setUpRasterizer(dut: SimTop, vertices: Array[(Int, Int)], tileLeft: Int, tileTop: Int): Unit = {
    dut.io.edgeCoeffs.valid.poke(true)
    dut.io.edgeCoeffs.bits.offset.x.poke(tileLeft)
    dut.io.edgeCoeffs.bits.offset.y.poke(tileTop)

    // Compute minimal bounding box that contains the triangle (but is inside the tile)
    val bbLeft = math.max(vertices.map(_._1).min & ~1, tileLeft)
    val bbTop = math.max(vertices.map(_._2).min & ~1, tileTop)
    val bbRight = math.min((vertices.map(_._1).max + 1) & ~1, tileLeft + cfg.tileSizePixels - 2)
    val bbBottom = math.min((vertices.map(_._2).max + 1) & ~1, tileTop + cfg.tileSizePixels - 2)

    dut.io.edgeCoeffs.bits.boundingBox.left.poke(bbLeft)
    dut.io.edgeCoeffs.bits.boundingBox.top.poke(bbTop)
    dut.io.edgeCoeffs.bits.boundingBox.right.poke(bbRight)
    dut.io.edgeCoeffs.bits.boundingBox.bottom.poke(bbBottom)
    val rawCoeffs = (0 until 3).map { i =>
      val (startX, startY) = vertices(i)
      val (endX, endY) = vertices((i + 1) % 3)
      val xs = endY - startY
      val ys = endX - startX
      val iv = ((bbLeft - startX) * xs - (bbTop - startY) * ys)
      (xs, -ys, iv)
    }

    val det = math.abs(rawCoeffs.map(_._3).sum)

    rawCoeffs.zipWithIndex.foreach { case ((xs, ys, iv), i) =>
      val normXs = (xs * 0xffffL / det).toInt
      val normYs = (ys * 0xffffL / det).toInt
      val normIv = (iv * 0xffffL / det).toInt

      dut.io.edgeCoeffs.bits.xStep(i).poke(normXs.S)
      dut.io.edgeCoeffs.bits.yStep(i).poke(normYs.S)
      dut.io.edgeCoeffs.bits.initialValue(i).poke(normIv.S)
    }

    while (dut.io.edgeCoeffs.ready.peek().litValue.toLong == 0) {
      dut.clock.step()
    }

    dut.clock.step()
    dut.io.edgeCoeffs.valid.poke(false)

    dut.clock.step() // Wait for rasterizer to start to complete is false.
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
