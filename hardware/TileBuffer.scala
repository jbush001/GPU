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
// blending, depth and other checks. It has a three stage read/modify/write
// pipeline and can accept one 2x2 quad per cycle. When rendering is finished
// for the tile, this can be commanded to go into a flush phase to copy its
// contents to memory.
//
// Constraints:
// - The same quad location cannot be written twice within 2 cycles.
// - There must be 3 cycles after the last write before a flush to flush
//   pipeline
// - It always clears the framebuffer during a flush (which assumes the client
//   is clearing a the beginning of a frame). In order to retain previously
//   rendered data in the next frame, it would need another port to read back from
//   memory.
//
// Open Questions/To do:
// - Stencil buffers
// - How does early-Z connect with this module?
// - Implement configurable blend modes and depth check modes
// - On flush, support different formats, e.g. abgr, bgra, rgba
//

package gpu

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.util.Random

object RenderBufferId extends ChiselEnum {
  val Color, Depth = Value
}

class TileBuffer extends Module {
  val pixelsPerQuad = 4

  val io = IO(new Bundle {
    val valid = Input(Bool())
    val quadLoc = Input(Point2D())
    val mask = Input(Bits(pixelsPerQuad.W))
    val colors = Input(Vec(pixelsPerQuad, RgbaColor()))
    val depths = Input(Vec(pixelsPerQuad, UInt(GpuConfig.depthBits.W)))

    val startFlush = Input(Bool())
    val flushBufferSel = Input(RenderBufferId()) // depth or color buffer
    val flushData = Decoupled(Bits(32.W))
    val clearColor = Input(RgbaColor())
    val clearDepth = Input(UInt(GpuConfig.depthBits.W))

    // Configuration
    val enableDepthWrite = Input(Bool())
    val enableDepthCheck = Input(Bool())
    val enableBlend = Input(Bool())
  })

  val memorySize = (GpuConfig.tileSizePixels * GpuConfig.tileSizePixels) / pixelsPerQuad
  val memoryAddrBits = log2Up(memorySize)
  val flushActive = RegInit(false.B)

  // The memory address references quads, but we need pixels.
  // During a flush after cycle 0, there is an invariant that flushCounter
  // always corresponds to the data on the read port of the SRAMs.
  val flushCounter = Reg(UInt((memoryAddrBits + 2).W))
  val flushCounterNext = Mux(io.flushData.valid && io.flushData.ready,
    flushCounter + 1.U, flushCounter)

  // Memory is divided into four banks, one per pixel in the quad
  val colorMemory = Seq.fill(pixelsPerQuad)(SyncReadMem(memorySize, RgbaColor()))
  val depthMemory = Seq.fill(pixelsPerQuad)(SyncReadMem(memorySize, UInt(GpuConfig.depthBits.W)))

  // Each quad stores its pixels across four banks, but during a flush, we
  // need to send them to memory in linear raster order. These do the shuffling
  // to flatten them.
  val coordBits = log2Up(GpuConfig.tileSizePixels)
  val flushX = flushCounterNext(coordBits - 1, 0)
  val flushY = flushCounterNext(coordBits * 2 - 1, coordBits)
  val flushAddress = Cat(flushY >> 1, flushX >> 1).asUInt

  // Note: we delay flush bank a cycle so it arrives with the data from memory
  val flushBank = RegNext(Cat(flushY(0), flushX(0)).asUInt)

  // The memory read ports are shared between flush and pixel operations
  // (which are never happening at the same time). Note we divide each
  // coordinate by two here to get the quad address from the pixel address.
  val inputQuadAddress = Cat(io.quadLoc.y(GpuConfig.tileCoordBits - 1, 1),
    io.quadLoc.x(GpuConfig.tileCoordBits - 1, 1)).asUInt
  val readAddress = Mux(flushActive, flushAddress, inputQuadAddress)
  val colorReadVal = VecInit(colorMemory.map(_.read(readAddress, io.valid
    || flushActive)))
  val depthReadVal = VecInit(depthMemory.map(_.read(readAddress, io.valid
    || flushActive)))

  // Pipelined quad address registers
  val quadAddressStage1 = RegNext(readAddress)
  val quadAddressStage2 = RegNext(quadAddressStage1)

  // Same for write ports
  val quadWriteLanes = Wire(Vec(pixelsPerQuad, Bool())) // Set by pixel processing pipelines
  val writeAddress = Wire(UInt(memoryAddrBits.W))
  val colorWriteVal = Wire(Vec(pixelsPerQuad, new RgbaColor))
  val depthWriteVal = Wire(Vec(pixelsPerQuad, UInt(GpuConfig.depthBits.W)))

  // Clear writes are delayed one cycle after reads.
  val clearAddress = RegNext(flushAddress)
  writeAddress := Mux(flushActive, clearAddress, quadAddressStage2)

  // Compute write
  for (pixel <- 0 until pixelsPerQuad) {
    val isClearLane = (flushBank === pixel.U)

    val colorClearEnable = isClearLane && (io.flushBufferSel === RenderBufferId.Color) && io.flushData.fire
    val colorWriteEnable = Mux(flushActive, colorClearEnable, quadWriteLanes(pixel))
    val colorData = Mux(flushActive, io.clearColor, colorWriteVal(pixel))
    when (colorWriteEnable) {
      colorMemory(pixel).write(writeAddress, colorData)
    }

    val depthClearEnable = isClearLane && (io.flushBufferSel === RenderBufferId.Depth) && io.flushData.fire
    val depthWriteEnable = Mux(flushActive, depthClearEnable, quadWriteLanes(pixel) && io.enableDepthWrite)
    val depthData = Mux(flushActive, io.clearDepth, depthWriteVal(pixel))
    when (depthWriteEnable) {
      depthMemory(pixel).write(writeAddress, depthData)
    }
  }

  io.flushData.bits := Mux(io.flushBufferSel === RenderBufferId.Depth,
    depthReadVal(flushBank).pad(32),
    colorReadVal(flushBank).toPackedBits
  )
  val flushDataValid = RegInit(false.B)
  io.flushData.valid := flushDataValid

  // Flush state handling.
  when(io.startFlush) {
    flushActive := true.B
    flushCounter := 0.U
  }.elsewhen(flushActive && flushCounter.andR && io.flushData.ready) {
    // Last cycle of flush transfer
    flushActive := false.B
    flushDataValid := false.B
  }.elsewhen(flushActive) {
    flushCounter := flushCounterNext
    flushDataValid := true.B
  }

  // Pixel processing pipeline
  for (pixel <- 0 until pixelsPerQuad) {
    // Stage 1: This waits for the read of the old color and depth values above,
    // and passes through the other vlaues.
    object stage1 {
      val newColor = RegNext(io.colors(pixel))
      val newDepth = RegNext(io.depths(pixel))
      val mask = RegNext(io.mask(pixel) && io.valid, false.B)
    }

    // Stage 2: visibility checks, destination blending
    object stage2 {
      val oneMinusAlpha = 0xff.U - stage1.newColor.alpha
      val oldWeightedColor = RegNext(colorReadVal(pixel).scale(oneMinusAlpha))
      val newColor = RegNext(stage1.newColor)
      val newDepth = RegNext(stage1.newDepth)
      val mask = RegNext(stage1.mask &&
        (!io.enableDepthCheck || stage1.newDepth < depthReadVal(pixel)), false.B)
    }

    // Stage 3: Blend, flush.
    {
      val blended = Mux(io.enableBlend,
        stage2.oldWeightedColor +| stage2.newColor,
        stage2.newColor)
      quadWriteLanes(pixel) := stage2.mask && !flushActive
      colorWriteVal(pixel) := blended
      depthWriteVal(pixel) := stage2.newDepth
    }
  }
}

// For testing, this tracks ground truth for what should be in the buffer.
class TileBufferReference {
  val colors = Array.fill(GpuConfig.tileSizePixels * GpuConfig.tileSizePixels)(0x00000000)
  val depths = Array.fill(GpuConfig.tileSizePixels * GpuConfig.tileSizePixels)(0xffffff)

  private def rasterIndex(quadX: Int, quadY: Int, pixel: Int): Int = {
    val offset = quadY * GpuConfig.tileSizePixels + quadX
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

class TileBufferTests extends AnyFunSuite with ChiselSim {
  // Push a quad through the DUT pipeline.
  def writeQuad(dut: TileBuffer, x: Int, y: Int, mask: Int, colors: Seq[Int],
    depths: Seq[Int]) = {

    dut.io.quadLoc.x.poke(x)
    dut.io.quadLoc.y.poke(y)
    for (pixel <- 0 until 4) {
      for (ch <- 0 until 4) {
        dut.io.colors(pixel).channels(ch).poke(colors(pixel) >> (ch * 8) & 0xff)
      }

      dut.io.depths(pixel).poke(depths(pixel))
    }

    dut.io.mask.poke(mask)
    dut.io.valid.poke(true)
    dut.clock.step()
    dut.io.valid.poke(false)
    dut.io.mask.poke(0)
  }

  def flush(dut: TileBuffer, select: RenderBufferId.Type): Seq[Int] = {
    val results = ArrayBuffer[Int]()
    dut.io.startFlush.poke(true)
    dut.io.flushBufferSel.poke(select)
    dut.clock.step()
    dut.io.startFlush.poke(false)
    dut.io.flushData.ready.poke(false)
    dut.io.flushData.valid.expect(0)

    val rng = new Random(42)
    val totalPixels = GpuConfig.tileSizePixels * GpuConfig.tileSizePixels
    while (results.length < totalPixels) {
      // Add random delays
      dut.io.flushData.ready.poke(rng.nextBoolean())
      dut.clock.step()
      if (dut.io.flushData.valid.peek().litValue.toLong != 0 &&
        dut.io.flushData.ready.peek().litValue.toLong != 0) {
        results += dut.io.flushData.bits.peek().litValue.toInt
      }
    }

    for (_ <- 0 until 5) {
      dut.clock.step()
      dut.io.flushData.valid.expect(0)
    }

    results.toSeq
  }

  // For test setup, manually force framebuffer to a known state
  def clearBuffers(dut: TileBuffer) = {
    dut.io.clearColor.channels(0).poke(0)
    dut.io.clearColor.channels(1).poke(0)
    dut.io.clearColor.channels(2).poke(0)
    dut.io.clearColor.channels(3).poke(0)
    this.flush(dut, RenderBufferId.Color)

    dut.io.clearDepth.poke(0xffffff)
    this.flush(dut, RenderBufferId.Depth)
  }

  test("alpha blend") {
   simulate(new TileBuffer()) { dut =>
      dut.io.valid.poke(false)
      dut.io.startFlush.poke(false)
      dut.io.enableBlend.poke(true)
      dut.io.enableDepthCheck.poke(true)
      dut.io.enableDepthWrite.poke(true)
      dut.io.mask.poke(0)

      this.clearBuffers(dut)

      writeQuad(dut, 0, 0, 0xf,
        Seq(0xff0080ff, 0xff0080ff, 0xff0080ff, 0xff0080ff),
        Seq(2, 2, 2, 2))

      // Flush pipeline
      dut.clock.step(4)

      // Blend
      writeQuad(dut, 0, 0, 0xf,
        Seq(0x00000000, 0x80ffffff, 0xffabcde7, 0x80808080),
        Seq(1, 1, 1, 1))

      // Flush pipeline
      dut.clock.step(4)

      val color = this.flush(dut, RenderBufferId.Color)
      assert(color(0) == 0xff0080ff) // Alpha was zero, no change
      assert(color(1) == 0xffffffff) // Midway
      assert(color(GpuConfig.tileSizePixels) == 0xffabcde7) // Alpha is 255, take new value
      assert(color(GpuConfig.tileSizePixels + 1) == 0xff80bfff) // Blend
    }
  }

  test("depth check disable") {
    simulate(new TileBuffer()) { dut =>
      this.clearBuffers(dut)
      dut.io.valid.poke(false)
      dut.io.startFlush.poke(false)
      dut.io.enableBlend.poke(true)
      dut.io.enableDepthCheck.poke(false)
      dut.io.enableDepthWrite.poke(true)
      dut.clock.step() // Ensure we are out of reset.

      writeQuad(dut, 0, 0, 0xf,
        Seq(0xff0080ff, 0xff0080ff, 0xff0080ff, 0xff0080ff),
        Seq(2, 2, 2, 2))

      // Flush pixels
      dut.clock.step(4)

      // Depth is greater. When checking is enabled, this iwl not write
      writeQuad(dut, 0, 0, 0xf,
        Seq(0x00000000, 0x80ffffff, 0xffabcde7, 0x80808080),
        Seq(3, 3, 3, 3))

      // Flush pipeline
      dut.clock.step(4)

      val depth = this.flush(dut, RenderBufferId.Depth)
      assert(depth(0) == 3)
      assert(depth(1) == 3)
      assert(depth(GpuConfig.tileSizePixels) == 3)
      assert(depth(GpuConfig.tileSizePixels + 1) == 3)

      // The last pixel written should have won.
      val color = this.flush(dut, RenderBufferId.Color)
      assert(color(0) == 0xff0080ff)
      assert(color(1) == 0xffffffff)
      assert(color(GpuConfig.tileSizePixels) == 0xffabcde7)
      assert(color(GpuConfig.tileSizePixels + 1) == 0xff80bfff)
    }
  }

  test("depth write disable") {
    simulate(new TileBuffer()) { dut =>
      this.clearBuffers(dut)

      dut.io.valid.poke(false)
      dut.io.startFlush.poke(false)
      dut.io.enableBlend.poke(true)
      dut.io.enableDepthCheck.poke(true)
      dut.io.enableDepthWrite.poke(false)
      dut.clock.step() // Ensure we are out of reset.

      writeQuad(dut, 0, 0, 0xf,
        Seq(0xff00aaff, 0xff00bbff, 0xff00ccff, 0xff00ddff),
        Seq(2, 2, 2, 2))

      // Flush pipeline
      dut.clock.step(4)

      // Depth is not written
      val depth = this.flush(dut, RenderBufferId.Depth)
      assert(depth(0) == 0xffffff)
      assert(depth(1) == 0xffffff)
      assert(depth(GpuConfig.tileSizePixels) == 0xffffff)
      assert(depth(GpuConfig.tileSizePixels + 1) == 0xffffff)

      // Color is written
      val color = this.flush(dut, RenderBufferId.Color)
      assert(color(0) == 0xff00aaff)
      assert(color(1) == 0xff00bbff)
      assert(color(GpuConfig.tileSizePixels) == 0xff00ccff)
      assert(color(GpuConfig.tileSizePixels + 1) == 0xff00ddff)
    }
  }

  test("alpha blend disable") {
    simulate(new TileBuffer()) { dut =>
      this.clearBuffers(dut)
      dut.io.valid.poke(false)
      dut.io.startFlush.poke(false)
      dut.io.enableBlend.poke(false)
      dut.io.enableDepthCheck.poke(true)
      dut.io.enableDepthWrite.poke(true)
      dut.clock.step() // Ensure we are out of reset.

      writeQuad(dut, 0, 0, 0xf,
        Seq(0x80ffff80, 0x80ffff80, 0x80ffff80, 0x80ffff80),
        Seq(2, 2, 2, 2))

      // Flush pipeline
      dut.clock.step(4)

      // All pixels shoudl be written full intensity
      val color = this.flush(dut, RenderBufferId.Color)
      assert(color(0) == 0x80ffff80)
      assert(color(1) == 0x80ffff80)
      assert(color(GpuConfig.tileSizePixels) == 0x80ffff80)
      assert(color(GpuConfig.tileSizePixels + 1) == 0x80ffff80)
    }
  }

  test("random tile write") {
    simulate(new TileBuffer()) { dut =>
      val reference = new TileBufferReference

      this.clearBuffers(dut)

      dut.io.valid.poke(false)
      dut.io.enableBlend.poke(true)
      dut.io.enableDepthCheck.poke(true)
      dut.io.enableDepthWrite.poke(true)
      dut.io.startFlush.poke(false)
      dut.clock.step() // Ensure we are out of reset.

      // Perform some random writes (the sequence is fixed because we use a known seed)
      val rng = new Random(42)
      val coordHistory = Queue[(Int, Int)]()
      for (_ <- 0 until 2000) {
        var quadX = 0
        var quadY = 0

        // We can't generate the same quad within 3 cycles, so ensure that doesn't happen.
        var regenerateCoord = true
        while (regenerateCoord) {
          quadX = rng.nextInt(GpuConfig.tileSizePixels) & ~1
          quadY = rng.nextInt(GpuConfig.tileSizePixels) & ~1
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
      dut.clock.step(4)

      val color = this.flush(dut, RenderBufferId.Color)
      reference.checkBuffer(0, color)
      val depth = this.flush(dut, RenderBufferId.Depth)
      reference.checkBuffer(1, depth)
    }
  }
}
