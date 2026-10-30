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
    val startTriangle = Flipped(Decoupled(new TriangleSetupParams))
    val writeVertexParam = Flipped(Valid(new Bundle {
      val addr = UInt(cfg.vertexParamIndexBits.W)
      val value = UInt(32.W)
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

  gpu.io.startTriangle <> io.startTriangle
  gpu.io.writeVertexParam <> io.writeVertexParam
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

class RenderTests extends AnyFunSuite with ChiselSim with TestHelpers {
  implicit val cfg: GpuConfig = GpuConfig()

  test("render texture") {
    simulate(new SimTop()) { dut =>
      val asm = new ShaderAssembler()
      asm
        // Compute s
        .move(128, SpecialReg.Lambda0)
        .rInst(OpCode.Mulf, 128, SpecialReg.Varying, 128) // dQ1 * lambda0
        .move(129, SpecialReg.Lambda1)
        .rInst(OpCode.Mulf, 129, SpecialReg.Varying, 129) // dQ2 * lambda1
        .rInst(OpCode.Addf, 128, 128, 129) // (dQ1 * lambda0) + (dQ2 * lambda1)
        .rInst(OpCode.Addf, SpecialReg.TexelS, SpecialReg.Varying, 128) // (dQ1 * lambda0) + (dQ2 * lambda1) + Q0

        // Compute t
        // The last instruction will kick off the texture fetch
        .move(128, SpecialReg.Lambda0)
        .rInst(OpCode.Mulf, 128, SpecialReg.Varying, 128) // dQ1 * lambda0
        .move(129, SpecialReg.Lambda1)
        .rInst(OpCode.Mulf, 129, SpecialReg.Varying, 129) // dQ2 * lambda1
        .rInst(OpCode.Addf, 128, 128, 129) // (dQ1 * lambda0) + (dQ2 * lambda1)
        .rInst(OpCode.Addf, SpecialReg.TexelT, SpecialReg.Varying, 128) // (dQ1 * lambda0) + (dQ2 * lambda1) + Q0

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

        var triangleIndex = 0
        while (!dut.io.complete.peek().litToBoolean || triangleIndex * 3 < indices.length) {
          if (triangleIndex * 3 < indices.length) {
            val triangleIndices = (0 until 3).map(i => indices(triangleIndex * 3 + i))
            val triangleVerts = triangleIndices.map(i => vertices(i))

            setUpTriangle(dut, triangleVerts, tileLeft, tileTop, fbSize, fbSize,
              triangleIndices.map(i => varyings(i)),
              triangleIndex >= (indices.length / 3) - 1)
            triangleIndex += 1
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

  var nextVertexBaseAddress = 0

  def setUpTriangle(dut: SimTop, vertices: Seq[(Float, Float, Float)],
    tileLeft: Int, tileTop: Int, fbWidth: Int, fbHeight: Int, varyings: Seq[Seq[Float]],
    lastTriangle: Boolean): Unit = {
    val vertexBaseAddresses = Seq(nextVertexBaseAddress, nextVertexBaseAddress + 15, nextVertexBaseAddress + 30)
    nextVertexBaseAddress += 45

    def writeParameter(addr: Int, value: Long): Unit = {
      dut.io.writeVertexParam.valid.poke(true.B)
      dut.io.writeVertexParam.bits.addr.poke(addr)
      dut.io.writeVertexParam.bits.value.poke(value)
      dut.clock.step()
    }

    for (((x, y, w), vertex) <- vertices.zipWithIndex) {
      val screenX = (((x + 1.0f) * fbWidth * 0.5f).toInt) & 0xffff
      val screenY = (((1.0f - y) * fbHeight * 0.5f).toInt) & 0xffff
      val baseAddr = vertexBaseAddresses(vertex)
      writeParameter(baseAddr, (screenX << 16) | screenY)
      writeParameter(baseAddr + 1, java.lang.Float.floatToRawIntBits(1.0f / w) & 0xffffffffL)
      for ((value, index) <- varyings(vertex).zipWithIndex) {
        writeParameter(baseAddr + index + 2,
          java.lang.Float.floatToRawIntBits(value) & 0xffffffffL)
      }
    }
    dut.io.writeVertexParam.valid.poke(false.B)

    dut.io.startTriangle.bits.lastTriangle.poke(lastTriangle.B)
    dut.io.startTriangle.bits.numVaryings.poke(varyings.head.length)
    dut.io.startTriangle.bits.tileOffset.x.poke(tileLeft)
    dut.io.startTriangle.bits.tileOffset.y.poke(tileTop)
    for (vertex <- 0 until 3) {
      dut.io.startTriangle.bits.vertexBaseAddr(vertex).poke(vertexBaseAddresses(vertex))
    }
    dut.io.startTriangle.valid.poke(true.B)
    while (!dut.io.startTriangle.ready.peek().litToBoolean) {
      dut.clock.step()
    }
    dut.clock.step()
    dut.io.startTriangle.valid.poke(false.B)
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
