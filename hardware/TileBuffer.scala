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

//
// The TileBuffer stores rendered depth, and color information for a
// small square portion of the framebuffer (a tile).  It performs alpha
// blending, depth and other checks checks. It has a three stage
// read/modify/write pipeline and can accept one 2x2 quad per cycle. When
// rendering is finished for the tile, this can be commanded to go into a
// flush phase to copy its contents to memory.
//
// Constraints:
// - The same quad location cannot be written twice within 2 cycles. I'm assuming
//   this won't be a problem in practice.
// - There must be 3 cycles after the last write before a flush to flush
//   pipeline
// - It always clears the framebuffer during a flush (which assumes the client
//   is clearing a the beginning of a frame). In order to retain previously
//   rendered data in the next frame, it would need another port to read back from
//   memory.
//
// Open Questions/To do:
// - Currently hard coded 32 bpp output, ABGR format. Make this configurable.
// - Stencil buffers
// - How does early-Z connect with this module?
// - Implement configurable blend modes and depth checks
// - On flush, support different formats, e.g. abgr, bgra, rgba
//

package gpu

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.util.Random

object RenderBufferId extends SpinalEnum {
  val Color, Depth = newElement()
}

class TileBuffer extends Module {
  val pixelsPerQuad = 4

  val io = new Bundle {
    val valid = in(Bool())
    val quadX = in(ScreenCoord())
    val quadY = in(ScreenCoord())
    val mask = in(Bits(pixelsPerQuad bits))
    val colors = in(Vec(RgbaColor(), pixelsPerQuad))
    val depths = in(Vec(UInt(GpuConfig.depthBits bits), pixelsPerQuad))

    val startFlush = in(Bool())
    val flushBufferSel = in(RenderBufferId()) // depth or color buffer
    val flushData = master(Stream(Bits(32 bits)))
    val clearColor = in(RgbaColor())
    val clearDepth = in(UInt(GpuConfig.depthBits bits))
  }

  val memorySize = GpuConfig.tileSizeQuads * GpuConfig.tileSizeQuads
  val memoryAddrBits = log2Up(memorySize)
  val flushActive = Reg(Bool()) init(False)

  // The memory address references quads, but we need pixels.
  // During a flush after cycle 0, there is an invariant that flushCounter
  // always corresponds to the data on the read port of the SRAMs.
  val flushCounter = Reg(UInt((memoryAddrBits + 2) bits))
  val flushCounterNext = Mux(io.flushData.valid && io.flushData.ready,
    flushCounter + 1, flushCounter)

  // Memory is divided into four banks, one per pixel in the quad
  val colorMemory = Seq.fill(pixelsPerQuad)(Mem(RgbaColor(), wordCount = memorySize))
  val depthMemory = Seq.fill(pixelsPerQuad)(Mem(UInt(GpuConfig.depthBits bits), wordCount = memorySize))

  // Each quad stores its pixels across four banks, but during a flush, we
  // need to send them to memory in linear raster order. These do the bit
  // twiddling to flatten them.
  val coordBits = log2Up(GpuConfig.tileSizePixels)
  val flushX = flushCounterNext(coordBits - 1 downto 0)
  val flushY = flushCounterNext(coordBits * 2 - 1 downto coordBits)
  val flushAddress = Cat(flushY >> 1, flushX >> 1).asUInt

  // Note: we delay flush bank a cycle so it arrives with the data from memory
  val flushBank = RegNext(Cat(flushY(0), flushX(0)).asUInt)

  // The memory read ports are shared between flush and pixel operations
  // (which are never happening at the same time)
  val inputQuadAddress = Cat(io.quadY(GpuConfig.tileCoordBits - 1 downto 0),
    io.quadX(GpuConfig.tileCoordBits - 1 downto 0)).asUInt
  val readAddress = Mux(flushActive, flushAddress, inputQuadAddress)
  val colorReadVal = colorMemory.map(_.readSync(readAddress, io.valid
    || flushActive))
  val depthReadVal = depthMemory.map(_.readSync(readAddress, io.valid
    || flushActive))

  // Pipelined quad address registers
  val quadAddressStage1 = RegNext(readAddress)
  val quadAddressStage2 = RegNext(quadAddressStage1)

  // Same for write ports
  val quadWriteLanes = UInt(pixelsPerQuad bits) // Set by pixel processing pipelines
  val writeAddress = UInt(memoryAddrBits bits)
  val colorWriteVal = Vec(RgbaColor(), pixelsPerQuad)
  val depthWriteVal = Vec(UInt(GpuConfig.depthBits bits), pixelsPerQuad)

  // Clear writes are delayed one cycle after reads.
  val clearAddress = RegNext(flushAddress)
  writeAddress := Mux(flushActive, clearAddress, quadAddressStage2)

  // Compute write
  val clearMask = U(1) << flushBank
  val flushAdvance = io.flushData.valid && io.flushData.ready
  val colorClearMask = Mux(io.flushBufferSel === RenderBufferId.Color && flushAdvance, clearMask, U(0))
  val depthClearMask = Mux(io.flushBufferSel === RenderBufferId.Depth && flushAdvance, clearMask, U(0))
  val colorWriteMask = Mux(flushActive, colorClearMask, quadWriteLanes)
  val depthWriteMask = Mux(flushActive, depthClearMask, quadWriteLanes)

  for (pixel <- 0 until pixelsPerQuad) {
    val colorData = Mux(flushActive, io.clearColor, colorWriteVal(pixel))
    val depthData = Mux(flushActive, io.clearDepth, depthWriteVal(pixel))
    colorMemory(pixel).write(writeAddress, colorData, colorWriteMask(pixel))
    depthMemory(pixel).write(writeAddress, depthData, depthWriteMask(pixel))
  }

  io.flushData.payload := io.flushBufferSel.mux(
    RenderBufferId.Color -> colorReadVal(flushBank).toPackedBits,
    RenderBufferId.Depth -> (B"8'x00" ## depthReadVal(flushBank)),
  )
  val flushDataValid = Reg(Bool()).init(false)
  io.flushData.valid := flushDataValid

  // Flush state handling.
  when(io.startFlush) {
    flushActive := True
    flushCounter := 0
  } elsewhen(flushActive && flushCounter.andR && io.flushData.ready) {
    // Last cycle of flush transfer
    flushActive := False
    flushDataValid := False
  } elsewhen(flushActive) {
    flushCounter := flushCounterNext
    flushDataValid := True
  }

  // Pixel processing pipeline
  for (pixel <- 0 until pixelsPerQuad) {
    // Stage 1: This waits for the read of the old color and depth values above,
    // and passes through the other vlaues.
    val newColorStage1 = RegNext(io.colors(pixel))
    val newDepthStage1 = RegNext(io.depths(pixel))
    val maskStage1 = RegNext(io.mask(pixel) && io.valid).init(false)

    // Stage 2: visibility checks, destination blending
    val oneMinusAlpha = 0xff - newColorStage1.alpha
    val oldWeightedColorStage2 = RegNext(colorReadVal(pixel).scale(oneMinusAlpha))
    val newColorStage2 = RegNext(newColorStage1)
    val newDepthStage2 = RegNext(newDepthStage1)
    val maskStage2 = RegNext(maskStage1 && (newDepthStage1 < depthReadVal(pixel))).init(false)

    // Stage 3: Blend, flush.
    val blended = oldWeightedColorStage2 +| newColorStage2
    quadWriteLanes(pixel) := maskStage2 && !flushActive
    colorWriteVal(pixel) := blended
    depthWriteVal(pixel) := newDepthStage2
  }
}

// For testing, this tracks ground truth for what should be in the buffer.
class TileBufferReference {
  val colors = Array.fill(GpuConfig.tileSizePixels * GpuConfig.tileSizePixels)(0x00000000)
  val depths = Array.fill(GpuConfig.tileSizePixels * GpuConfig.tileSizePixels)(0xffffff)

  private def rasterIndex(quadX: Int, quadY: Int, pixel: Int): Int = {
    val offset = quadY * 2 * GpuConfig.tileSizePixels + quadX * 2
    pixel match {
      case 0 => offset
      case 1 => offset + 1
      case 2 => offset + GpuConfig.tileSizePixels
      case 3 => offset + 1 + GpuConfig.tileSizePixels
    }
  }

  def clamp(x: Int): Int = {
    x.min(255)
  }

  private def unpackChannel(value: Int, channel: Int): Int = {
    (value >> (channel * 8)) & 0xff
  }

  private def packColor(r: Int, g: Int, b: Int, a: Int): Int = {
    ((a & 0xff) << 24) | ((b & 0xff) << 16) | ((g & 0xff) << 8) | (r & 0xff)
  }

  def blend(newValue: Int, prevValue: Int): Int = {
    val oldR = unpackChannel(prevValue, 0)
    val oldG = unpackChannel(prevValue, 1)
    val oldB = unpackChannel(prevValue, 2)
    val oldA = unpackChannel(prevValue, 3)

    val incomingA = unpackChannel(newValue, 3)
    val oneMinusAlpha = 0xff - incomingA
    val newR = clamp(unpackChannel(newValue, 0) + (oldR * oneMinusAlpha / 0xff))
    val newG = clamp(unpackChannel(newValue, 1) + (oldG * oneMinusAlpha / 0xff))
    val newB = clamp(unpackChannel(newValue, 2) + (oldB * oneMinusAlpha / 0xff))
    val newA = clamp(unpackChannel(newValue, 3) + (oldA * oneMinusAlpha / 0xff))

    packColor(newR, newG, newB, newA)
  }

  def writeQuad(quadX: Int, quadY: Int, mask: Int,
                newColors: Seq[Int], newDepths: Seq[Int]) = {
    for (pixel <- 0 until 4) {
      if ((mask & (1 << pixel)) != 0) {
        val idx = rasterIndex(quadX, quadY, pixel)
        if (newDepths(pixel) < depths(idx)) {
          colors(idx) = blend(newColors(pixel), colors(idx))
          depths(idx) = newDepths(pixel)
        }
      }
    }
  }

  def checkBuffer(select: Int, results: Seq[Int]) = {
    for (y <- 0 until GpuConfig.tileSizePixels) {
      for (x <- 0 until GpuConfig.tileSizePixels) {
        val idx = y * GpuConfig.tileSizePixels + x
        val expected = if (select == 0) colors(idx) else depths(idx)
        assert(results(idx) == expected,
          s"Mismatch at ($x, $y): got ${Integer.toHexString(results(idx))}, expected ${Integer.toHexString(expected)}")
      }
    }
  }
}

// Test suite
class TileBufferSpec extends AnyFunSuite {
  val compiledModel = TestConfig.testSim.compile({
    new TileBuffer {
      // This is requred for getBufferContent
      for (pixel <- 0 until 4) {
        colorMemory(pixel).setName(s"colorMemory_$pixel").simPublic()
        depthMemory(pixel).setName(s"depthMemory_$pixel").simPublic()
      }
    }
  })

  // Push a quad through the DUT pipeline.
  def writeQuad(dut: TileBuffer, x: Int, y: Int, mask: Int, colors: Seq[Int],
    depths: Seq[Int]) = {

    dut.io.quadX #= x
    dut.io.quadY #= y
    for (pixel <- 0 until 4) {
      for (ch <- 0 until 4) {
        dut.io.colors(pixel).channels(ch) #= (colors(pixel) >> (ch * 8) & 0xff)
      }

      dut.io.depths(pixel) #= depths(pixel).toLong
    }

    dut.io.mask #= mask
    dut.io.valid #= true
    dut.clockDomain.waitSampling()
    dut.io.valid #= false
  }

  def flush(dut: TileBuffer, select: RenderBufferId.E): Seq[Int] = {
    val results = ArrayBuffer[Int]()
    dut.io.startFlush #= true
    dut.io.flushBufferSel #= select
    dut.clockDomain.waitSampling()
    dut.io.startFlush #= false
    dut.io.flushData.ready #= false
    assert(!dut.io.flushData.valid.toBoolean)

    val rng = new Random(42)
    val totalPixels = GpuConfig.tileSizePixels * GpuConfig.tileSizePixels
    while (results.length < totalPixels) {
      // Add random delays
      dut.io.flushData.ready #= rng.nextBoolean()
      dut.clockDomain.waitSampling()
      if (dut.io.flushData.valid.toBoolean &&
        dut.io.flushData.ready.toBoolean) {
        results += dut.io.flushData.payload.toInt
      }
    }

    for (_ <- 0 until 5) {
      dut.clockDomain.waitSampling()
      assert(!dut.io.flushData.valid.toBoolean)
    }

    results.toSeq
  }

  // For test setup, manually force framebuffer to a known state
  def clearBuffers(dut: TileBuffer) = {
    for (i <- 0 until (GpuConfig.tileSizeQuads * GpuConfig.tileSizeQuads)) {
      for (pixel <- 0 until 4) {
        dut.colorMemory(pixel).setBigInt(i, 0)
        dut.depthMemory(pixel).setBigInt(i, 0xffffff)
      }
    }
  }

  // Read directly from the buffer memory to check it, bypassing the flush
  // mechanism.
  def getBufferContent(dut: TileBuffer, bufferSelect: RenderBufferId.E): Seq[Int] = {
    val results = ArrayBuffer[Int]()
    for (y <- 0 until GpuConfig.tileSizePixels) {
      for (x <- 0 until GpuConfig.tileSizePixels) {
        val quadX = x / 2
        val quadY = y / 2
        val pixel = (y % 2) * 2 + (x % 2)
        val idx = quadY * (GpuConfig.tileSizePixels / 2) + quadX
        bufferSelect match {
          case RenderBufferId.Color => results += dut.colorMemory(pixel).getBigInt(idx).toInt
          case RenderBufferId.Depth => results += dut.depthMemory(pixel).getBigInt(idx).toInt
          case _ => throw new IllegalArgumentException("Unknown buffer type passed to getBuffeContent")
        }
      }
    }

    results.toSeq
  }

  test("alpha blend") {
    compiledModel.doSim { dut =>
      SimTimeout(1000000)
      this.clearBuffers(dut)

      dut.io.valid #= false
      dut.io.startFlush #= false
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.waitSampling() // Ensure we are out of reset.

      writeQuad(dut, 0, 0, 0xf,
        Seq(0xff0080ff, 0xff0080ff, 0xff0080ff, 0xff0080ff),
        Seq(2, 2, 2, 2))

      // Flush pipeline
      dut.clockDomain.waitSampling(4)

      // Blend
      writeQuad(dut, 0, 0, 0xf,
        Seq(0x00000000, 0x80ffffff, 0xffabcde7, 0x80808080),
        Seq(1, 1, 1, 1))

      // Flush pipeline
      dut.clockDomain.waitSampling(4)

      val color = this.getBufferContent(dut, RenderBufferId.Color)
      assert(color(0) == 0xff0080ff) // Alpha was zero, no change
      assert(color(1) == 0xffffffff) // Midway
      assert(color(GpuConfig.tileSizePixels) == 0xffabcde7) // Alpha is 255, take new value
      assert(color(GpuConfig.tileSizePixels + 1) == 0xff80bfff) // Blend
    }
  }

  test("random tile write") {
    compiledModel.doSim { dut =>
      SimTimeout(1000000)
      val reference = new TileBufferReference

      this.clearBuffers(dut)

      dut.io.valid #= false
      dut.io.startFlush #= false
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.waitSampling() // Ensure we are out of reset.

      // Perform some random writes (the sequence is fixed because we use a known seed)
      val rng = new Random(42)
      val coordHistory = Queue[(Int, Int)]()
      for (_ <- 0 until 2000) {
        var quadX = 0
        var quadY = 0

        // We can't generate the same quad within 3 cycles, so ensure that doesn't happen.
        var regenerateCoord = true
        while (regenerateCoord) {
          quadX = rng.nextInt(GpuConfig.tileSizeQuads)
          quadY = rng.nextInt(GpuConfig.tileSizeQuads)
          regenerateCoord = coordHistory.contains((quadX, quadY))
        }

        coordHistory.enqueue((quadX, quadY))
        if (coordHistory.size > 2) {
          coordHistory.dequeue()
        }

        val mask = rng.nextInt(0xf)
        val colors = Seq.fill(4) { rng.nextInt() }
        val depths = Seq.fill(4) { rng.nextInt(0xffffff) }

        writeQuad(dut, quadX, quadY, mask, colors, depths)
        reference.writeQuad(quadX, quadY, mask, colors, depths)
      }

      // Flush pipeline
      dut.clockDomain.waitSampling(4)

      val color = this.getBufferContent(dut, RenderBufferId.Color)
      reference.checkBuffer(0, color)
      val depth = this.getBufferContent(dut, RenderBufferId.Depth)
      reference.checkBuffer(1, depth)
    }
  }

  test("flush") {
    compiledModel.doSim { dut =>
      SimTimeout(1000000)
      this.clearBuffers(dut)

      dut.io.valid #= false
      dut.io.startFlush #= false
      dut.io.clearColor.channels(0) #= 0x12
      dut.io.clearColor.channels(1) #= 0xef
      dut.io.clearColor.channels(2) #= 0xcd
      dut.io.clearColor.channels(3) #= 0xab
      dut.io.clearDepth #= 0x654321
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.waitSampling() // Ensure we are out of reset.

      // Fill the framebuffer with random data
      val rng = new Random(42)
      for (y <- 0 until GpuConfig.tileSizeQuads) {
        for (x <- 0 until GpuConfig.tileSizeQuads) {
          val colors = Seq.fill(4) { rng.nextInt() }
          val depths = Seq.fill(4) { rng.nextInt(0xffffff) }
          writeQuad(dut, x, y, 0xf, colors, depths)
        }
      }

      // Flush pipeline
      dut.clockDomain.waitSampling(4)

      val colorActual = this.getBufferContent(dut, RenderBufferId.Color)
      val colorFlush = flush(dut, RenderBufferId.Color)
      assert(colorActual == colorFlush)

      // Check that the buffer is clear now
      assert(this.getBufferContent(dut, RenderBufferId.Color).forall(_ == 0xabcdef12))

      val depthActual = this.getBufferContent(dut, RenderBufferId.Depth)
      val depthFlush = flush(dut, RenderBufferId.Depth)
      assert(depthActual == depthFlush)

      // Check that the buffer is clear now
      assert(this.getBufferContent(dut, RenderBufferId.Depth).forall(_ == 0x654321))
    }
  }
}
