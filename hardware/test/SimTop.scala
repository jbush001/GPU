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
    val setupParams = Flipped(Decoupled(new RasterizerSetupParams))
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

  io.complete := pixelShaderConductor.io.idle && rasterizer.io.complete

  rasterizer.io.quad <> pixelShaderConductor.io.rasterizedQuad
  pixelShaderConductor.io.flush := rasterizer.io.complete
  pixelShaderConductor.io.startJob <> shaderCore.io.startJob
  shaderCore.io.jobFinished <> pixelShaderConductor.io.jobFinished
  shaderCore.io.regRead <> pixelShaderConductor.io.shaderRegRead
  pixelShaderConductor.io.shaderRegReadData <> shaderCore.io.regReadData
  shaderCore.io.regWrite <> pixelShaderConductor.io.shaderRegWrite
  pixelShaderConductor.io.shadedQuad <> tileBuffer.io.shadedQuad

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

    val asm = new ShaderAssembler()
    asm
      .move(64, 96) // lambda0
      .move(65, 97) // lambda1
      .rInst(OpCode.Addi, 66, 64, 65) // a = lambda0 + lambda1
      .kInst(OpCode.LoadHi, 67, 0)
      .kInst(OpCode.LoadLo, 67, 0xffff)
      .rInst(OpCode.Subi, 66, 67, 66) // lambda2 = 0x10000 - (lambda0 + lambda1)
      .kInst(OpCode.LoadHi, 67, 0)
      .kInst(OpCode.LoadLo, 67, 6)
      .rInst(OpCode.Lsr, 104, 64, 67) // red = lambda0 >> 6
      .rInst(OpCode.Lsr, 105, 65, 67) // green = lambda1 >> 6
      .rInst(OpCode.Lsr, 106, 66, 67) // blue = lambda2 >> 6
      .move(107, 55) // alpha = 0xff
      .halt()
    val programBytes = asm.finish()

    // Fill in memory
    SimMemAccess.write(dut.clock, dut.io.dap, 0, programBytes)

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
        val normXs = (xs * 0xffffL / det).toInt
        val normYs = (ys * 0xffffL / det).toInt
        val normIv = (iv * 0xffffL / det).toInt

        dut.io.setupParams.bits.xStep(i).poke(normXs.S)
        dut.io.setupParams.bits.yStep(i).poke(normYs.S)
        dut.io.setupParams.bits.initialValue(i).poke(normIv.S)
      }

      while (dut.io.setupParams.ready.peek().litValue.toLong == 0) {
        dut.clock.step()
      }

      dut.clock.step()
      dut.io.setupParams.valid.poke(false)

      dut.clock.step() // Wait for rasterizer to start to complete is false.

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
