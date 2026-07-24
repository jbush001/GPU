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

object RenderBufferId extends ChiselEnum {
  val Color, Depth = Value
}

/** Stores rendered depth, and color information for square subset of the
  * framebuffer.
  *
  * This module performs alpha blending, depth tests, and other pixel-level
  * checks. It has a three stage read/modify/write pipeline that accepts one
  * 2x2 quad per cycle. It stores all information in on-chip SRAM. When
  * rendering completes for a tile, a flush copies the buffer contents to
  * external memory.
  *
  * Constraints:
  *
  *   - The same quad location cannot be written within 2 consecutive cycles.
  *   - A minimum of 3 cycles must pass after the last write before initiating
  *     a flush sequence to allow it to move through the pipeline.
  *   - This module automatically clears the framebuffer data during a flush
  *     operation. Retaining data across frames would require an additional
  *     read-back operation.
  *
  * @todo Stencil buffers
  * @todo How does early-Z connect with this module?
  * @todo Implement configurable blend modes and depth check modes
  * @todo On flush, support different formats, e.g. abgr, bgra, rgba
  */
class TileBuffer extends Module {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val quadLoc = Input(Point2D())
    val mask = Input(Bits(Consts.pixelsPerQuad.W))
    val colors = Input(Vec(Consts.pixelsPerQuad, Color()))
    val depths = Input(Vec(Consts.pixelsPerQuad, UInt(GpuConfig.depthBits.W)))

    val startFlush = Input(Bool())
    val flushBufferSel = Input(RenderBufferId()) // depth or color buffer
    val flushData = Decoupled(Bits(32.W))
    val clearColor = Input(Color())
    val clearDepth = Input(UInt(GpuConfig.depthBits.W))

    // Configuration
    val enableDepthWrite = Input(Bool())
    val enableDepthCheck = Input(Bool())
    val enableBlend = Input(Bool())
  })

  val memorySize = (GpuConfig.tileSizePixels * GpuConfig.tileSizePixels) / Consts.pixelsPerQuad
  val memoryAddrBits = log2Up(memorySize)
  val flushActive = RegInit(false.B)

  // The memory address references quads, but we need pixels.
  // During a flush after cycle 0, there is an invariant that flushCounter
  // always corresponds to the data on the read port of the SRAMs.
  val flushCounter = Reg(UInt((memoryAddrBits + 2).W))
  val flushCounterNext = Mux(io.flushData.valid && io.flushData.ready,
    flushCounter + 1.U, flushCounter)

  // Memory is divided into four banks, one per pixel in the quad
  val colorMemory = Seq.fill(Consts.pixelsPerQuad)(SyncReadMem(memorySize, Color()))
  val depthMemory = Seq.fill(Consts.pixelsPerQuad)(SyncReadMem(memorySize, UInt(GpuConfig.depthBits.W)))

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
  val quadWriteLanes = Wire(Vec(Consts.pixelsPerQuad, Bool())) // Set by pixel processing pipelines
  val writeAddress = Wire(UInt(memoryAddrBits.W))
  val colorWriteVal = Wire(Vec(Consts.pixelsPerQuad, new Color))
  val depthWriteVal = Wire(Vec(Consts.pixelsPerQuad, UInt(GpuConfig.depthBits.W)))

  // Clear writes are delayed one cycle after reads.
  val clearAddress = RegNext(flushAddress)
  writeAddress := Mux(flushActive, clearAddress, quadAddressStage2)

  // Compute write
  for (pixel <- 0 until Consts.pixelsPerQuad) {
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

  // @todo configure output conversion here.
  io.flushData.bits := Mux(io.flushBufferSel === RenderBufferId.Depth,
    depthReadVal(flushBank).pad(32),
    colorReadVal(flushBank).toPackedArgb32
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
  for (pixel <- 0 until Consts.pixelsPerQuad) {
    // Stage 1: This waits for the read of the old color and depth values above,
    // and passes through the other values.
    object stage1 {
      val newColor = RegNext(io.colors(pixel))
      val newDepth = RegNext(io.depths(pixel))
      val mask = RegNext(io.mask(pixel) && io.valid, false.B)
    }

    // Stage 2: visibility checks, destination blending
    object stage2 {
      val oneMinusAlpha = Color.maxChannelValue.U - stage1.newColor.alpha
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

