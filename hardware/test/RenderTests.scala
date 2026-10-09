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
import chisel3.simulator.scalatest.ChiselSim
import java.awt.image.BufferedImage
import java.io.FileInputStream
import javax.imageio.ImageIO
import org.scalatest.funsuite.AnyFunSuite
import shader._

class SimTop(implicit val cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val dap = new DirectAccessPort
    val coeffs = Flipped(Decoupled(new RasterizerCoeffs))
    val writeVaryingCoeff = Flipped(Valid(new Bundle {
      val triangleId = UInt(cfg.triangleIdBits.W)
      val index = UInt(5.W)
      val value = Float32()
    }))

    val writeDepthCoeffs = Flipped(Valid(new Bundle {
      val triangleId = UInt(cfg.triangleIdBits.W)
      val coeffs = new DepthInterpolatorCoeffs()
    }))

    val startFlush = Input(Bool())
    val flushColor = Decoupled(Bits(32.W))
    val flushBufferSel = Input(RenderBufferId()) // depth or color buffer

    val complete = Output(Bool())
    val triangleFinished = Valid(UInt(cfg.triangleIdBits.W))
    val lastTriangle = Output(Bool())
  })

  val gpu = Module(new Gpu)
  val memory = Module(new SimAxiMemory(1024))

  io.lastTriangle := gpu.io.lastTriangle
  io.triangleFinished <> gpu.io.triangleFinished

  val complete = RegInit(false.B)
  io.complete := complete
  when (gpu.io.lastTriangle) {
    complete := true.B
  }.elsewhen (io.startFlush) {
    complete := false.B // Hack
  }

  gpu.io.coeffs <> io.coeffs
  io.writeVaryingCoeff <> gpu.io.writeVaryingCoeff
  gpu.io.writeDepthCoeffs <> io.writeDepthCoeffs
  gpu.io.startFlush := io.startFlush
  gpu.io.flushData.ready := io.flushColor.ready
  io.flushColor.valid := gpu.io.flushData.valid
  when (io.flushBufferSel === RenderBufferId.Color) {
    io.flushColor.bits := gpu.io.flushData.bits.color.toArgb32
  }.otherwise {
    io.flushColor.bits := Fill(4, gpu.io.flushData.bits.depth.toUnorm(8))
  }

  gpu.io.flushBufferSel := io.flushBufferSel
  gpu.io.axiBus <> memory.io
  memory.dap <> io.dap
}

class RenderTests extends AnyFunSuite with ChiselSim {
  implicit val cfg: GpuConfig = GpuConfig()

  test("render texture") {
    simulate(new SimTop()) { dut =>
      val asm = new ShaderAssembler()
      asm
        // Compute s
        .move(64, SpecialReg.Lambda0)
        .rInst(OpCode.Mulf, 64, SpecialReg.Varying, 64) // dQ1 * lambda0
        .move(65, SpecialReg.Lambda1)
        .rInst(OpCode.Mulf, 65, SpecialReg.Varying, 65) // dQ2 * lambda1
        .rInst(OpCode.Addf, 64, 64, 65) // (dQ1 * lambda0) + (dQ2 * lambda1)
        .rInst(OpCode.Addf, SpecialReg.TexelS, SpecialReg.Varying, 64) // (dQ1 * lambda0) + (dQ2 * lambda1) + Q0

        // Compute t
        // The last instruction will kick off the texture fetch
        .move(64, SpecialReg.Lambda0)
        .rInst(OpCode.Mulf, 64, SpecialReg.Varying, 64) // dQ1 * lambda0
        .move(65, SpecialReg.Lambda1)
        .rInst(OpCode.Mulf, 65, SpecialReg.Varying, 65) // dQ2 * lambda1
        .rInst(OpCode.Addf, 64, 64, 65) // (dQ1 * lambda0) + (dQ2 * lambda1)
        .rInst(OpCode.Addf, SpecialReg.TexelT, SpecialReg.Varying, 64) // (dQ1 * lambda0) + (dQ2 * lambda1) + Q0

        // Read back texture data, store in output registers
        .move(SpecialReg.OutputR, SpecialReg.TexelR) // red
        .move(SpecialReg.OutputG, SpecialReg.TexelG) // green
        .move(SpecialReg.OutputB, SpecialReg.TexelB) // blue
        .move(SpecialReg.OutputA, SpecialReg.Const1_0f) // alpha = 1.0
        .halt()
      val programBytes = asm.finish()

      val vertices = Seq(
        (-0.375f, 0.921875f, 0.6f),
        (-0.921875f, -0.953125f, 0.2f),
        (0.875f, -0.953125f, 0.2f),
        (0.375f, 0.921875f, 0.6f)
      )
      val varyings: Seq[Seq[Float]] = Seq(
        Seq(0.0f, 0.0f),
        Seq(0.0f, 1.0f),
        Seq(1.0f, 1.0f),
        Seq(1.0f, 0.0f)
      )

      runRenderTest(dut, programBytes, vertices, varyings, Seq(0, 1, 2, 0, 2, 3))
    }
  }

  test("render intersecting triangles") {
    simulate(new SimTop()) { dut =>
      val asm = new ShaderAssembler()
      asm
        .move(SpecialReg.Const1_0f, SpecialReg.Varying)
        .move(SpecialReg.Const1_0f, SpecialReg.Varying)
        .move(SpecialReg.OutputR, SpecialReg.Varying)
        .move(SpecialReg.Const1_0f, SpecialReg.Varying)
        .move(SpecialReg.Const1_0f, SpecialReg.Varying)
        .move(SpecialReg.OutputG, SpecialReg.Varying)
        .move(SpecialReg.OutputA, SpecialReg.Const1_0f)
        .halt()
      val programBytes = asm.finish()
      val vertices = Seq(
        (-0.9375f, 0.625f, 0.1f),
        (0.0f, -0.9375f, 0.9f),
        (0.9375f, 0.9375f, 0.1f),

        (0.0f, 0.9375f, 0.9f),
        (-0.9375f, -0.625f, 0.3f),
        (0.9375f, -0.9375f, 0.1f),
      )

      val varyings: Seq[Seq[Float]] = Seq(
        Seq(0.0f, 0.9f),
        Seq(0.0f, 0.9f),
        Seq(0.0f, 0.9f),
        Seq(0.9f, 0.0f),
        Seq(0.9f, 0.0f),
        Seq(0.9f, 0.0f),
      )

      runRenderTest(dut, programBytes, vertices, varyings, Seq(0, 1, 2, 3, 4, 5))
    }
  }

  def runRenderTest(dut: SimTop, programBytes: Seq[Long], vertices: Seq[(Float, Float, Float)],
                    varyings: Seq[Seq[Float]], indices: Seq[Int]): Unit = {
    val imageData = renderBuffer(dut, programBytes, vertices, varyings, indices)
    val reference = loadReferenceImage(getReferenceImageName())
    reference match {
      case Some(ref) =>
        // Compare the rendered image with the reference image
        if (!imageData.sameElements(ref)) {
          writeOutputImage("output.png", 128, imageData)
          fail("Rendered image does not match reference image")
        }
      case None =>
        println(s"No reference image available ${getReferenceImageName()}, writing output image.")
        writeOutputImage("output.png", 128, imageData)
    }
  }

  def renderBuffer(dut: SimTop, programBytes: Seq[Long], vertices: Seq[(Float, Float, Float)],
    varyings: Seq[Seq[Float]], indices: Seq[Int]): Array[Int] = {
    // Copy shader into memory
    SimMemAccess.write(dut.clock, dut.io.dap, 0, programBytes)

    // Run a flush to clear out the buffer initially
    flushBuffer(dut, None, 0, 0)

    val fbSize = 128
    val fbData = new Array[Int](fbSize * fbSize)

    for (tileRow <- 0 until fbSize / cfg.tileSizePixels) {
      for (tileColumn <- 0 until fbSize / cfg.tileSizePixels) {
        val tileLeft = tileColumn * cfg.tileSizePixels
        val tileTop = tileRow * cfg.tileSizePixels

        // XXX for now does not check for deallocation of triangle IDs
        var triangleId = 0
        while (!dut.io.complete.peek().litToBoolean || triangleId * 3 < indices.length) {
          // Submit new triangles. This is a stand-in for the unimplemented setup unit.
          if (dut.io.coeffs.ready.peek().litToBoolean && triangleId * 3 < indices.length) {
            val triangleIndices = (0 until 3).map(i => indices(triangleId * 3 + i))
            val triangleVerts = triangleIndices.map(i => vertices(i))

            setUpTriangle(dut, triangleId, triangleVerts, tileLeft, tileTop, fbSize, fbSize,
              triangleIndices.map(i => varyings(i)),
              triangleId >= (indices.length / 3) - 1)
            triangleId += 1
          }

          dut.clock.step()
        }

        // Flush the tile buffer pipeline
        dut.clock.step(5)

        // Read out the final data
        val offset = (fbSize * cfg.tileSizePixels * tileRow) +
          (cfg.tileSizePixels * tileColumn)
        flushBuffer(dut, Some(fbData), offset, fbSize)
      }
    }

    fbData
  }

  def floatToRawBits(fval: Float) = java.lang.Float.floatToIntBits(fval) & 0xffffffffL

  def setUpTriangle(dut: SimTop, triangleId: Int, vertices: Seq[(Float, Float, Float)],
    tileLeft: Int, tileTop: Int, fbWidth: Int, fbHeight: Int, varyings: Seq[Seq[Float]],
    lastTriangle: Boolean): Unit = {

    // Tile constants
    val xPixelStep = 2.0f / fbWidth.toFloat
    val yPixelStep = 2.0f / fbHeight.toFloat
    val bbLeft = ((tileLeft.toFloat / fbWidth.toFloat) - 0.5f) * 2.0f
    val bbTop = (0.5f - (tileTop.toFloat / fbHeight.toFloat)) * 2.0f

    // Set up depth interpolation coefficients
    val invW0 = 1.0f / vertices(0)._3
    val invW1 = 1.0f / vertices(1)._3
    val invW2 = 1.0f / vertices(2)._3
    dut.io.writeDepthCoeffs.bits.triangleId.poke(triangleId)
    dut.io.writeDepthCoeffs.bits.coeffs.invW0.raw.poke(floatToRawBits(invW0))
    dut.io.writeDepthCoeffs.bits.coeffs.invW1.raw.poke(floatToRawBits(invW1))
    dut.io.writeDepthCoeffs.bits.coeffs.invdW1.raw.poke(floatToRawBits(invW1 - invW0))
    dut.io.writeDepthCoeffs.bits.coeffs.invW2.raw.poke(floatToRawBits(invW2))
    dut.io.writeDepthCoeffs.bits.coeffs.invdW2.raw.poke(floatToRawBits(invW2 - invW0))
    dut.io.writeDepthCoeffs.valid.poke(true)
    dut.clock.step()
    dut.io.writeDepthCoeffs.valid.poke(false)

    // Set up varying interpolation coefficients
    dut.io.writeVaryingCoeff.valid.poke(true)
    dut.io.writeVaryingCoeff.bits.triangleId.poke(triangleId)
    for (i <- varyings(0).indices) {
      dut.io.writeVaryingCoeff.bits.index.poke(i * 3)
      dut.io.writeVaryingCoeff.bits.value.raw.poke(floatToRawBits(varyings(1)(i) - varyings(0)(i))) // dQ1
      dut.clock.step()
      dut.io.writeVaryingCoeff.bits.index.poke(i * 3 + 1)
      dut.io.writeVaryingCoeff.bits.value.raw.poke(floatToRawBits(varyings(2)(i) - varyings(0)(i))) // dQ2
      dut.clock.step()
      dut.io.writeVaryingCoeff.bits.index.poke(i * 3 + 2)
      dut.io.writeVaryingCoeff.bits.value.raw.poke(floatToRawBits(varyings(0)(i))) // Q0
      dut.clock.step()
    }

    dut.io.writeVaryingCoeff.valid.poke(false)

    // Set up rasterizer coefficients
    dut.io.coeffs.valid.poke(true)
    dut.io.coeffs.bits.offset.x.poke(tileLeft)
    dut.io.coeffs.bits.offset.y.poke(tileTop)
    dut.io.coeffs.bits.triangleId.poke(triangleId)
    dut.io.coeffs.bits.lastTriangle.poke(lastTriangle)

    dut.io.coeffs.bits.boundingBox.left.poke(tileLeft)
    dut.io.coeffs.bits.boundingBox.top.poke(tileTop)
    dut.io.coeffs.bits.boundingBox.right.poke(tileLeft + cfg.tileSizePixels - 2)
    dut.io.coeffs.bits.boundingBox.bottom.poke(tileTop + cfg.tileSizePixels - 2)

    val rawCoeffs = new Array[(Float, Float, Float)](3)
    var area2 = 0.0f
    for (edge <- 0 until 3) {
      val (startX, startY, _) = vertices(edge)
      val (endX, endY, _) = vertices((edge + 1) % 3)
      val dx = startY - endY
      val dy = endX - startX
      val iv = ((bbLeft - startX) * dx + (bbTop - startY) * dy)
      area2 += iv
      rawCoeffs(edge) = (dx * xPixelStep, -dy * yPixelStep, iv)
    }

    val normFactor = 1.0f / area2

    for (edge <- rawCoeffs.indices) {
      val (xs, ys, iv) = rawCoeffs(edge)
      val normXs = (xs * normFactor * 0xffffL).toInt
      val normYs = (ys * normFactor * 0xffffL).toInt
      val isTopLeft = (normXs > 0) || (normXs == 0 && normYs > 0)
      val normIv = (iv * normFactor * 0xffffL).toInt - (if (isTopLeft) 0 else 1)

      dut.io.coeffs.bits.edges(edge).xStep.poke(normXs.S)
      dut.io.coeffs.bits.edges(edge).yStep.poke(normYs.S)
      dut.io.coeffs.bits.edges(edge).initialValue.poke(normIv.S)
    }

    while (dut.io.coeffs.ready.peek().litValue.toLong == 0) {
      dut.clock.step()
    }

    dut.clock.step()
    dut.io.coeffs.valid.poke(false)

    dut.clock.step() // Wait for rasterizer to start to complete is false.
  }

  // XXX placeholder until we implement proper memory interface for TileBuffer
  // flushes.
  def flushBuffer(dut: SimTop, out: Option[Array[Int]], start: Int, stride: Int) = {
    dut.io.startFlush.poke(true)
    dut.io.flushBufferSel.poke(RenderBufferId.Color)
    dut.io.flushColor.ready.poke(true)

    var fbIndex = start

    for (_ <- 0 until cfg.tileSizePixels) {
      for (_ <- 0 until cfg.tileSizePixels) {
        dut.clock.step()
        dut.io.startFlush.poke(false)
        while (dut.io.flushColor.valid.peek().litValue.toLong == 0 ||
          dut.io.flushColor.ready.peek().litValue.toLong == 0) {
          dut.clock.step()
        }

        out match {
          case Some(arr) => {
            arr(fbIndex) = (dut.io.flushColor.bits.peek().litValue.toLong | 0xff000000L).toInt
          }
          case None => {}
        }

        fbIndex += 1
      }

      fbIndex += stride - cfg.tileSizePixels
    }

    dut.clock.step() // Clear last pixel

    // Need to read the depth buffer in order to clear it.
    dut.io.startFlush.poke(true)
    dut.io.flushBufferSel.poke(RenderBufferId.Depth)

    for (_ <- 0 until cfg.totalTilePixels) {
      dut.clock.step()
      dut.io.startFlush.poke(false)

      // Bit of a hack here, the flush color handshaking signals
      // are valid for depth info, even though we're not flushing that.
      while (dut.io.flushColor.valid.peek().litValue.toLong == 0 ||
        dut.io.flushColor.ready.peek().litValue.toLong == 0) {
        dut.clock.step()
      }
    }

    dut.clock.step() // Clear last pixel
  }

  def getReferenceImageName(): String = {
    "hardware/test/resources/" + implementation.getDirectory.getFileName.toString + ".png"
  }

  def loadReferenceImage(name: String): Option[Array[Int]] = {
    if (!new java.io.File(name).exists()) {
      return None
    }

    val inputStream = new FileInputStream(name)
    val bufferedImage: BufferedImage = ImageIO.read(inputStream)
    val width = bufferedImage.getWidth
    val height = bufferedImage.getHeight
    val pixels = new Array[Int](width * height)
    bufferedImage.getRGB(0, 0, width, height, pixels, 0, width)
    Some(pixels)
  }

  def writeOutputImage(fileName: String, fbSize: Int, fbData: Array[Int]): Unit = {
    // Write an image file
    val canvas = new BufferedImage(fbSize, fbSize, BufferedImage.TYPE_INT_ARGB)
    canvas.setRGB(0, 0, fbSize, fbSize, fbData.toArray, 0, fbSize)
    val outputFile = implementation.getDirectory.resolve(fileName).toFile
    ImageIO.write(canvas, "png", outputFile)
    println(s"wrote output file to ${outputFile.getAbsolutePath}")
  }
}
