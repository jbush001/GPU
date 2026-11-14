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
    val writeVertexParam = Flipped(Valid(new Bundle{
      val column = UInt(cfg.vpmColumnIndexBits.W)
      val value = Vec(cfg.shaderVectorLanes, UInt(32.W))
    }))

    val controlBus = Flipped(new ControlBus)

    val complete = Output(Bool())
    val tileWriterDone = Output(Bool())
    val triangleFinished = Valid(UInt(cfg.triangleIdBits.W))
    val lastTriangle = Output(Bool())
  })

  val gpu = Module(new Gpu)
  val memory = Module(new SimAxiMemory(16384))

  io.lastTriangle := gpu.io.lastTriangle
  io.tileWriterDone := gpu.io.tileWriterDone
  io.triangleFinished <> gpu.io.triangleFinished

  val completeReg = RegInit(false.B)
  io.complete := completeReg
  when (gpu.io.lastTriangle) {
    completeReg := true.B
  }.elsewhen (io.controlBus.regWrite.valid
    && (io.controlBus.regWrite.bits.address === ControlRegisterIndex.TileWriteStart
      || io.controlBus.regWrite.bits.address === ControlRegisterIndex.TileClearStart)) {
    // This is a hack: because the control processor isn't fully wired in,
    // we need a way to check when the render is finished. The complete
    // flag is cleared as a side effect when a new tile is started.
    completeReg := false.B
  }

  gpu.io.startTriangle <> io.startTriangle
  gpu.io.writeVertexParam <> io.writeVertexParam
  gpu.io.controlBus <> io.controlBus

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

  def writeControlReg(dut: SimTop, address: UInt, data: Int): Unit = {
    dut.io.controlBus.regWrite.valid.poke(true.B)
    dut.io.controlBus.regWrite.bits.address.poke(address)
    dut.io.controlBus.regWrite.bits.data.poke(data)
    dut.clock.step()
    dut.io.controlBus.regWrite.valid.poke(false.B)
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

  var nextVertexRow = 0

  def renderBuffer(dut: SimTop, programBytes: Seq[Long], vertices: Seq[(Float, Float, Float)],
    varyings: Seq[Seq[Float]], indices: Seq[Int]): Array[Int] = {
    // Copy shader into memory
    SimMemAccess.write(dut.clock, dut.io.dap, 0, programBytes)

    writeControlReg(dut, ControlRegisterIndex.NumVaryings, varyings.head.length)
    writeControlReg(dut, ControlRegisterIndex.ShaderAddressL, 0) // XXX hardcoded for this test.
    writeControlReg(dut, ControlRegisterIndex.ShaderAddressH, 0) // XXX hardcoded for this test.

    val fbSize = 128
    val framebufferAddress = 0x1000
    clearTileBuffer(dut)

    for (tileRow <- 0 until fbSize / cfg.tileSizePixels) {
      for (tileColumn <- 0 until fbSize / cfg.tileSizePixels) {
        nextVertexRow = 0

        writeControlReg(dut, ControlRegisterIndex.TileOffsetX,
          tileColumn * cfg.tileSizePixels)
        writeControlReg(dut, ControlRegisterIndex.TileOffsetY,
          tileRow * cfg.tileSizePixels)

        var triangleIndex = 0
        while (!dut.io.complete.peek().litToBoolean || triangleIndex * 3 < indices.length) {
          if (triangleIndex * 3 < indices.length) {
            val triangleIndices = (0 until 3).map(i => indices(triangleIndex * 3 + i))
            val triangleVerts = triangleIndices.map(i => vertices(i))

            setUpTriangle(dut, triangleVerts, fbSize, fbSize,
              triangleIndices.map(i => varyings(i)),
              triangleIndex >= (indices.length / 3) - 1)
            triangleIndex += 1
          }

          dut.clock.step()
        }

        // Flush the tile buffer pipeline
        dut.clock.step(5)

        val tileAddress = framebufferAddress +
          ((fbSize * cfg.tileSizePixels * tileRow) +
            (cfg.tileSizePixels * tileColumn)) * 4
        writeControlReg(dut, ControlRegisterIndex.TileWriteAddressL, tileAddress)
        writeControlReg(dut, ControlRegisterIndex.TileWriteAddressH, 0)
        writeControlReg(dut, ControlRegisterIndex.TileWriteStride, fbSize * 4)
        writeControlReg(dut, ControlRegisterIndex.TileWriteStart,
          RenderBufferId.Color.litValue.toInt)
        waitForTileWriter(dut)
        clearTileBuffer(dut)
      }
    }

    SimMemAccess.read(dut.clock, dut.io.dap, framebufferAddress, fbSize * fbSize / 2)
      .flatMap(word => Seq(word.toInt | 0xff000000, (word >>> 32).toInt | 0xff000000))
      .toArray
  }


  def setUpTriangle(dut: SimTop, vertices: Seq[(Float, Float, Float)],
    fbWidth: Int, fbHeight: Int, varyings: Seq[Seq[Float]],
    lastTriangle: Boolean): Unit = {

    val paramValues = Array.fill(cfg.shaderVectorLanes, cfg.vpmColumns)(0)

    def writeParameter(row: Int, col: Int, value: Long): Unit = {
      paramValues(row)(col) = value.toInt
      dut.clock.step()
    }

    for (((x, y, w), vertex) <- vertices.zipWithIndex) {
      val screenX = (((x + 1.0f) * fbWidth * 0.5f).toInt) & 0xffff
      val screenY = (((1.0f - y) * fbHeight * 0.5f).toInt) & 0xffff
      val baseRow = nextVertexRow + vertex
      writeParameter(baseRow, 0, (screenX << 16) | screenY)
      writeParameter(baseRow, 1, java.lang.Float.floatToRawIntBits(1.0f / w) & 0xffffffffL)
      for ((value, index) <- varyings(vertex).zipWithIndex) {
        writeParameter(baseRow, index + 2,
          java.lang.Float.floatToRawIntBits(value) & 0xffffffffL)
      }
    }

    // This basically transposes the values and writes as columns in vertex
    // parameter memory.
    dut.io.writeVertexParam.valid.poke(true.B)
    for (col <- 0 until 2 + varyings.head.length) {
      dut.io.writeVertexParam.bits.column.poke(col)
      for (row <- 0 until cfg.shaderVectorLanes) {
        dut.io.writeVertexParam.bits.value(row).poke(paramValues(row)(col))
      }

      dut.clock.step()
    }

    dut.io.writeVertexParam.valid.poke(false.B)

    dut.io.startTriangle.bits.lastTriangle.poke(lastTriangle.B)
    for (vertex <- 0 until 3) {
      dut.io.startTriangle.bits.vertexBaseAddr(vertex).row.poke(nextVertexRow + vertex)
      dut.io.startTriangle.bits.vertexBaseAddr(vertex).column.poke(0)
    }

    nextVertexRow += 3

    dut.io.startTriangle.valid.poke(true.B)
    while (!dut.io.startTriangle.ready.peek().litToBoolean) {
      dut.clock.step()
    }
    dut.clock.step()
    dut.io.startTriangle.valid.poke(false.B)
  }

  def waitForTileWriter(dut: SimTop): Unit = {
    while (!dut.io.tileWriterDone.peek().litToBoolean) {
      dut.clock.step()
    }
  }

  def clearTileBuffer(dut: SimTop): Unit = {
    for (buffer <- Seq(RenderBufferId.Color, RenderBufferId.Depth)) {
      writeControlReg(dut, ControlRegisterIndex.TileClearStart, buffer.litValue.toInt)
      waitForTileWriter(dut)
    }
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
