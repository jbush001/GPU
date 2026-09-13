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
    val edgeCoeffs = Flipped(Decoupled(new RasterizerCoeffs))
    val writeVaryingCoeff = Flipped(Valid(new Bundle {
      val index = UInt(5.W)
      val value = Float32()
    }))
    val startFlush = Input(Bool())
    val flushData = Decoupled(Bits(32.W))
    val flushBufferSel = Input(RenderBufferId()) // depth or color buffer
    val complete = Output(Bool())
  })

  val gpu = Module(new Gpu)
  val memory = Module(new SimAxiMemory(1024))

  io.complete := gpu.io.complete

  gpu.io.edgeCoeffs <> io.edgeCoeffs
  io.writeVaryingCoeff <> gpu.io.writeVaryingCoeff
  gpu.io.startFlush := io.startFlush
  gpu.io.flushData <> io.flushData
  gpu.io.flushBufferSel := io.flushBufferSel
  gpu.io.axiBus <> memory.io
  memory.dap <> io.dap
}

class RenderTests extends AnyFunSuite with ChiselSim {
  implicit val cfg: GpuConfig = GpuConfig()

  test("triangle1") {
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

      val vertices = Array((5, 7), (23, 110), (118, 49))
      val varying1 = (vertices(0)._1.toFloat / 127.0f,
        vertices(1)._1.toFloat / 127.0f, vertices(2)._1.toFloat / 127.0f)
      val varying2 = (vertices(0)._2.toFloat / 127.0f,
        vertices(1)._2.toFloat / 127.0f, vertices(2)._2.toFloat / 127.0f)
      val varyings = Seq(varying1, varying2)

      val imageData = renderBuffer(dut, programBytes, vertices, varyings)
      val reference = loadReferenceImage(getReferenceImageName())
      reference match {
        case Some(ref) =>
          // Compare the rendered image with the reference image
          assert(imageData.sameElements(ref), "Rendered image does not match reference image")
        case None =>
          println(s"No reference image available ${getReferenceImageName()}, writing output image.")
          writeOutputImage("output.png", 128, imageData)
      }
    }
  }

  def renderBuffer(dut: SimTop, programBytes: Seq[Long], vertices: Array[(Int, Int)],
    varyings: Seq[(Float, Float, Float)]): Array[Int] = {
    // Copy shader into memory
    SimMemAccess.write(dut.clock, dut.io.dap, 0, programBytes)

    // Run a flush to clear out the buffer initially
    flushBuffer(dut, None, 0, 0)

    val fbSize = 128
    val fbData = new Array[Int](fbSize * fbSize)

    // Set up attributes
    for (i <- varyings.indices) {
      setUpVarying(dut, i * 3, varyings(i))
    }

    for (tile <- 0 until 4) {
      val tileRow = tile / 2
      val tileColumn = tile % 2

      // Set up a triangle
      val tileLeft = tileColumn * cfg.tileSizePixels
      val tileTop = tileRow * cfg.tileSizePixels
      setUpRasterizer(dut, vertices, tileLeft, tileTop)

      while (!dut.io.complete.peek().litToBoolean) {
        dut.clock.step()
      }

      // Read out the final data
      val offset = (fbSize * cfg.tileSizePixels * tileRow) +
        (cfg.tileSizePixels * tileColumn)
      flushBuffer(dut, Some(fbData), offset, fbSize)
    }

    fbData
  }

  def floatToRawBits(fval: Float) = java.lang.Float.floatToIntBits(fval) & 0xffffffffL

  var nextVaryingCoeffWrite = 0

  def setUpVarying(dut: SimTop, index: Int, values: (Float, Float, Float)): Unit = {
    dut.io.writeVaryingCoeff.valid.poke(true)
    dut.io.writeVaryingCoeff.bits.index.poke(index)
    dut.io.writeVaryingCoeff.bits.value.raw.poke(floatToRawBits(values._2 - values._1)) // dQ1
    dut.clock.step()
    dut.io.writeVaryingCoeff.bits.index.poke(index + 1)
    dut.io.writeVaryingCoeff.bits.value.raw.poke(floatToRawBits(values._3 - values._1)) // dQ2
    dut.clock.step()
    dut.io.writeVaryingCoeff.bits.index.poke(index + 2)
    dut.io.writeVaryingCoeff.bits.value.raw.poke(floatToRawBits(values._1)) // Q0
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
      val dx = endY - startY
      val dy = endX - startX

      // Implement top-left fill convention.
      val isTopLeft = (dy > 0) || (dy == 0 && dx < 0)
      val rawIv = ((bbLeft - startX) * dx - (bbTop - startY) * dy)
      val biasedIv = rawIv + (if (isTopLeft) 0 else -1)
      (dx, -dy, rawIv, biasedIv)
    }

    val det = math.abs(rawCoeffs.map(_._3).sum)

    rawCoeffs.zipWithIndex.foreach { case ((xs, ys, _, biasedIv), i) =>
      val normXs = (xs * 0xffffL / det).toInt
      val normYs = (ys * 0xffffL / det).toInt
      val normIv = (biasedIv * 0xffffL / det).toInt

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

    dut.clock.step() // Clear last pixel

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

    dut.clock.step() // Clear last pixel
  }

  def getReferenceImageName(): String = {
    "hardware/test/resources/" + implementation.getDirectory.getFileName.toString + ".png"
  }

  def loadReferenceImage(name: String): Option[Array[Int]] = {
    val inputStream = new FileInputStream(name)
    if (inputStream == null) {
      return None
    }

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
