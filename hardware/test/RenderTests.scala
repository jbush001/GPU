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
import java.nio.file.{Files, Paths}
import scala.sys.process.{Process, ProcessLogger}
import shader._

class SimTop(implicit val cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val dap = new DirectAccessPort
    val triangleSetupReady = Output(Bool())
    val writeVertexParam = Flipped(Valid(new Bundle{
      val column = UInt(cfg.vpmColumnIndexBits.W)
      val value = Vec(cfg.shaderVectorLanes, UInt(32.W))
    }))

    val axiSubordinate = Flipped(new Axi4LiteIO)

    val controlBus = Flipped(new ControlBus)

    val complete = Output(Bool())
    val tileWriterDone = Output(Bool())
    val triangleFinished = Valid(UInt(cfg.triangleIdBits.W))
    val fence = Output(Bool())
  })

  val gpu = Module(new Gpu)
  val memory = Module(new SimAxiMemory(16384))

  io.axiSubordinate <> gpu.io.axiSubordinate
  io.fence := gpu.io.fence
  io.tileWriterDone := gpu.io.tileWriterDone
  io.triangleFinished <> gpu.io.triangleFinished
  io.triangleSetupReady := gpu.io.triangleSetupReady

  val completeReg = RegInit(false.B)
  io.complete := completeReg
  when (gpu.io.fence) {
    completeReg := true.B
  }.elsewhen (io.controlBus.regWrite.valid
    && io.controlBus.regWrite.bits.address === ControlRegisterIndex.Fence
    && io.controlBus.regWrite.bits.data(0)) {
    // Latch completion for the newly requested fence.
    completeReg := false.B
  }

  gpu.io.writeVertexParam <> io.writeVertexParam
  gpu.io.controlBus <> io.controlBus

  gpu.io.axiManager <> memory.io
  memory.dap <> io.dap
}

class RenderTests extends AnyFunSuite with ChiselSim with TestHelpers {
  implicit val cfg: GpuConfig = GpuConfig()

  val commandProgram = assembleCommandProgram()

  object CommandId {
    val Flush = 1
    val LoadShaderState = 2
  }

  test("texture") {
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
      val fragmentShaderBin = asm.finish()

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

      runRenderTest(dut, fragmentShaderBin, vertices, varyings, Seq(0, 1, 2, 0, 2, 3))
    }
  }

  test("intersecting triangles") {
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
      val fragmentShaderBin = asm.finish()
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

      runRenderTest(dut, fragmentShaderBin, vertices, varyings, Seq(0, 1, 2, 3, 4, 5))
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
        writeOutputImage("output.png", 128, imageData)
        fail(s"No reference image available ${getReferenceImageName()}, writing output image.")
    }
  }

  var nextVertexRow = 0

  def renderBuffer(dut: SimTop, programBytes: Seq[Long], vertices: Seq[(Float, Float, Float)],
    varyings: Seq[Seq[Float]], indices: Seq[Int]): Array[Int] = {
    // Copy shader into memory
    SimMemAccess.write(dut.clock, dut.io.dap, 0, programBytes)

    loadCommandProgram(dut, commandProgram)

    sendCommand(dut, CommandId.LoadShaderState, varyings.head.length, 0) // Shader address is 0 for this test.
    waitForCommandsComplete(dut)

    val fbSize = 128
    val framebufferAddress = 0x1000
    clearTileBuffer(dut) // Initially clear both buffers

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
        sendCommand(dut, CommandId.Flush, fbSize * 4, tileAddress)
        waitForCommandsComplete(dut)
      }
    }

    SimMemAccess.read(dut.clock, dut.io.dap, framebufferAddress, fbSize * fbSize / 2)
      .flatMap(word => Seq(word.toInt | 0xff000000, (word >>> 32).toInt | 0xff000000))
      .toArray
  }

  def setUpTriangle(dut: SimTop, vertices: Seq[(Float, Float, Float)],
    fbWidth: Int, fbHeight: Int, varyings: Seq[Seq[Float]],
    fence: Boolean): Unit = {

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

    while (!dut.io.triangleSetupReady.peek().litToBoolean) {
      dut.clock.step()
    }

    if (fence) {
      writeControlReg(dut, ControlRegisterIndex.Fence, 1)
    }
    for (vertex <- 0 until 3) {
      writeControlReg(dut, ControlRegisterIndex.VertexVpmAddr, nextVertexRow + vertex)
    }

    nextVertexRow += 3

  }

  def waitForTileWriter(dut: SimTop): Unit = {
    while (!dut.io.tileWriterDone.peek().litToBoolean) {
      dut.clock.step()
    }
  }

  def clearTileBuffer(dut: SimTop): Unit = {
    writeControlReg(dut, ControlRegisterIndex.TileClearStart, 0)
    waitForTileWriter(dut)
  }

  def assembleCommandProgram(): Seq[BigInt] = {
    val resourceDir = "hardware/test/resources"
    val sourcePath = Paths.get(resourceDir, "render_command_program.s")
    val hexPath = Paths.get(resourceDir, "render_command_program.hex")
    val output = new StringBuilder
    val logger = ProcessLogger(
      line => output.append(line).append('\n'),
      line => output.append(line).append('\n')
    )

    val exitCode = Process(Seq("python3", "tools/assemble-command.py", sourcePath.toString)).!(logger)
    if (exitCode != 0) {
        fail(s"Assembler failed with exit code $exitCode:\n$output")
    }

    Files.readAllLines(hexPath).toArray.toSeq.map(_.toString).map(BigInt(_, 16))
  }

  var commandBufferBase = 0x12000L
  var commandBufferEnd = commandBufferBase

  def loadCommandProgram(dut: SimTop, program: Seq[BigInt]): Unit = {
    for (word <- program) {
      writeHostRegister(dut, GpuHostRegisters.LoadProgram, word.toLong)
    }

    // Start with an empty command buffer: begin (Scratchpad0) == end (Scratchpad1)
    commandBufferEnd = commandBufferBase
    writeHostRegister(dut, GpuHostRegisters.Scratchpad0, commandBufferBase)
    writeHostRegister(dut, GpuHostRegisters.Scratchpad1, commandBufferEnd)

    writeHostRegister(dut, GpuHostRegisters.HaltCommandProcessor, 0L)
  }

  /** Append a packet to the command buffer and advance the end pointer so the
    * command processor will consume it. 'payload' is the type-specific field
    * in the upper 24 bits of the first word, 'argument' is the second word.
    */
  def sendCommand(dut: SimTop, commandType: Int, payload: Int, argument: Long): Unit = {
    val word0 = (commandType & 0xff).toLong | ((payload.toLong & 0xffffff) << 8)
    val word1 = argument & 0xffffffffL
    SimMemAccess.write(dut.clock, dut.io.dap, commandBufferEnd, Seq(word0 | (word1 << 32)))
    commandBufferEnd += 8
    writeHostRegister(dut, GpuHostRegisters.Scratchpad1, commandBufferEnd)
  }

  /** The command processor advances the begin pointer (Scratchpad0) after it
    * finishes each packet, so the buffer is drained when it reaches the end.
    */
  def waitForCommandsComplete(dut: SimTop): Unit = {
    var cycles = 0
    while (readHostRegister(dut, GpuHostRegisters.Scratchpad0) != commandBufferEnd) {
      dut.clock.step()
      cycles += 1
    }
  }

  def readHostRegister(dut: SimTop, address: Long): Long = {
    dut.io.axiSubordinate.readData.ready.poke(true)
    dut.io.axiSubordinate.readRequest.valid.poke(true)
    dut.io.axiSubordinate.readRequest.bits.address.poke(address)
    dut.clock.step()
    dut.io.axiSubordinate.readRequest.valid.poke(false)
    val value = dut.io.axiSubordinate.readData.bits.data.peek().litValue.toLong
    dut.clock.step()
    value
  }

  def writeHostRegister(dut: SimTop, address: Long, value: Long): Unit = {
    dut.io.axiSubordinate.writeResponse.ready.poke(true)
    dut.io.axiSubordinate.writeRequest.valid.poke(true)
    dut.io.axiSubordinate.writeRequest.bits.address.poke(address)
    dut.io.axiSubordinate.writeData.valid.poke(true)
    dut.io.axiSubordinate.writeData.bits.data.poke(value)
    dut.clock.step()
    dut.io.axiSubordinate.writeRequest.valid.poke(false)
    dut.io.axiSubordinate.writeData.valid.poke(false)
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
