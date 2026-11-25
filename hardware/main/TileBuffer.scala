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

class ColorQuad(implicit val cfg: GpuConfig) extends Quad {
  val colors = Vec(Consts.pixelsPerQuad, Color())
  val depths = Vec(Consts.pixelsPerQuad, Float32())
}

/** Working storage for color and depth information for the framebuffer tile
  * currently being rendered.
  *
  * Shaded 2x2 pixel quads arrive from [[FragmentShaderConductor]]. Each is
  * depth-tested and alpha-blended with the stored values, and the surviving
  * color and depth are written back. This data is stored entirely in on-chip
  * SRAM. When the tile is complete, a flush operation streams its contents
  * to be written back to external memory and clears the buffer for the next
  * tile.
  *
  * Values are stored in a native formats ([[Color]] and [[Float32]]).
  * Conversion to the external-memory format happens downstream, during the
  * flush. The depth and color buffers are cleared as a side effect of flushing
  * the color buffer.
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
  * @todo Implement configurable blend modes and depth check modes
  * @todo Multisample anti-aliasing support
  */
class TileBuffer(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val colorQuad = Flipped(Valid(new ColorQuad))
    val controlBus = Flipped(new ControlBus)
    val flushData = Decoupled(new Bundle {
      val depth = Float32()
      val color = Color()
    })
  })

  // Configuration registers
  val startFlush = WireInit(false.B)
  val flushBufferSelReg = RegInit(RenderBufferId.Color) // depth or color buffer
  val clearColorReg = RegInit(0.U.asTypeOf(new Color))
  val clearDepthReg = RegInit(Float32.Zero)
  val enableDepthWriteReg = RegInit(true.B)
  val enableDepthCheckReg = RegInit(true.B)
  val enableBlendReg = RegInit(false.B)

  // Handle configuration register writes from [[CommandProcessor]]
  when (io.controlBus.regWrite.valid) {
    switch (io.controlBus.regWrite.bits.address) {
      is (ControlRegisterIndex.ClearColor) {
        clearColorReg := Color.fromArgb32(io.controlBus.regWrite.bits.data)
      }
      is (ControlRegisterIndex.ClearDepth) {
        clearDepthReg := Float32(io.controlBus.regWrite.bits.data)
      }
      is (ControlRegisterIndex.TileWriteFlags) {
        enableDepthWriteReg := io.controlBus.regWrite.bits.data(0)
        enableDepthCheckReg := io.controlBus.regWrite.bits.data(1)
        enableBlendReg := io.controlBus.regWrite.bits.data(2)
      }
      is (ControlRegisterIndex.TileWriteStart) {
        startFlush := true.B
        flushBufferSelReg := io.controlBus.regWrite.bits.data(0).asTypeOf(RenderBufferId())
      }
      is (ControlRegisterIndex.TileClearStart) {
        startFlush := true.B
        flushBufferSelReg := RenderBufferId.Color
      }
    }
  }

  val memorySize = (cfg.totalTilePixels) / Consts.pixelsPerQuad
  val memoryAddrBits = log2Up(memorySize)
  val flushActiveReg = RegInit(false.B)

  // Index of the pixel currently being flushed. The SRAMs are addressed by
  // quad, but we need to flush pixels in raster order, so the upper
  // memoryAddrBits bits are the quad address and the low 2 bits select a
  // pixel within the quad.
  //
  // Invariant: after the second cycle of a flush, flushCounterReg is the
  // index of the pixel whose data is on the SRAM read port. The read address
  // is driven from flushCounterNext to keep this true across the synchronous
  // read latency, and the counter only advances when flushData fires to
  // maintain this even when there is backpressure.
  val flushCounterReg = Reg(UInt((memoryAddrBits + 2).W))
  val flushCounterNext = Mux(io.flushData.fire,
    flushCounterReg + 1.U, flushCounterReg)

  // Each memory entry stores a complete quad.
  val colorMemory = SyncReadMem(memorySize, Vec(Consts.pixelsPerQuad, Color()))
  val depthMemory = SyncReadMem(memorySize, Vec(Consts.pixelsPerQuad, Float32()))

  // This does the shuffling to convert from quads to raster order. We scan
  // each row of quads twice, once for the upper row of pixels, and again for
  // the lower.
  val coordBits = log2Up(cfg.tileSizePixels)
  val flushX = flushCounterNext(coordBits - 1, 0)
  val flushY = flushCounterNext(coordBits * 2 - 1, coordBits)
  val flushAddress = (((flushY >> 1) ## (flushX >> 1)).asUInt)

  // Note: we delay flush bank a cycle so it arrives with the data from memory
  val flushBankReg = RegNext(flushY(0) ## flushX(0))

  // The memory read ports are shared between flush and pixel operations
  // (which are never happening at the same time). Note we divide each
  // coordinate by two here to get the quad address from the pixel address.
  val inputQuadAddress = (io.colorQuad.bits.location.y(cfg.tileCoordBits - 1, 1) ##
    io.colorQuad.bits.location.x(cfg.tileCoordBits - 1, 1)).asUInt
  val readAddress = Mux(flushActiveReg, flushAddress, inputQuadAddress)
  val colorReadVal = colorMemory.read(readAddress, io.colorQuad.valid || flushActiveReg)
  val depthReadVal = depthMemory.read(readAddress, io.colorQuad.valid || flushActiveReg)

  // Pipelined quad address registers
  val quadAddressStage1Reg = RegNext(readAddress)
  val quadAddressStage2Reg = RegNext(quadAddressStage1Reg)

  // Clear writes are delayed one cycle after reads, as memory is modeled
  // as write first.
  val clearAddressReg = RegNext(flushAddress)
  val writeAddress = Mux(flushActiveReg, clearAddressReg, quadAddressStage2Reg)

  // Only one of these will be valid depending on flushBufferSel.
  io.flushData.bits.depth := depthReadVal(flushBankReg)
  io.flushData.bits.color := colorReadVal(flushBankReg)

  val flushDataValidReg = RegInit(false.B)
  io.flushData.valid := flushDataValidReg

  // Flush state handling.
  when(startFlush) {
    assert(!flushActiveReg,
      "TileBuffer flush started while another flush is active")
    flushActiveReg := true.B
    flushCounterReg := 0.U
  }.elsewhen(flushActiveReg && flushCounterReg.andR && io.flushData.ready) {
    // Last cycle of flush transfer
    flushActiveReg := false.B
    flushDataValidReg := false.B
  }.elsewhen(flushActiveReg) {
    flushCounterReg := flushCounterNext
    flushDataValidReg := true.B
  }

  val colorWriteVal = Wire(Vec(Consts.pixelsPerQuad, new Color))
  val depthWriteVal = Wire(Vec(Consts.pixelsPerQuad, Float32()))
  val colorWriteMask = Wire(Vec(Consts.pixelsPerQuad, Bool()))
  val depthWriteMask = Wire(Vec(Consts.pixelsPerQuad, Bool()))

  // Pixel processing pipeline
  for (pixel <- 0 until Consts.pixelsPerQuad) {
    // Stage 1: This waits for the read of the old color and depth values above,
    // and passes through the other values.
    class Stage1 {
      val newColorReg = RegNext(io.colorQuad.bits.colors(pixel))
      val newDepthReg = RegNext(io.colorQuad.bits.depths(pixel))
      val maskReg = RegNext(io.colorQuad.bits.mask(pixel) && io.colorQuad.valid, false.B)
    }
    val stage1 = new Stage1()

    // Stage 2: visibility checks, destination blending
    class Stage2 {
      val oneMinusAlpha = Color.maxChannelValue.U - stage1.newColorReg.alpha
      val oldWeightedColorReg = RegNext(colorReadVal(pixel).scale(oneMinusAlpha))
      val newColorReg = RegNext(stage1.newColorReg)
      val newDepthReg = RegNext(stage1.newDepthReg)
      val maskReg = RegNext(stage1.maskReg &&
        (!enableDepthCheckReg || stage1.newDepthReg > depthReadVal(pixel)), false.B)
    }
    val stage2 = new Stage2()

    // Stage 3: Blend, writeback
    val blended = Mux(enableBlendReg,
      stage2.oldWeightedColorReg +| stage2.newColorReg,
      stage2.newColorReg)

    val isClearLane = (flushBankReg === pixel.U)

    val colorClearEnable = isClearLane &&
      (flushBufferSelReg === RenderBufferId.Color) &&
      io.flushData.fire
    colorWriteMask(pixel) := Mux(flushActiveReg, colorClearEnable, stage2.maskReg)
    colorWriteVal(pixel) := Mux(flushActiveReg, clearColorReg, blended)

    // Note: all buffers are cleared when we flush the color buffer. We don't commonly
    // flush the depth buffer, so this is an optimization. If there is a request to
    // explicitly flush the depth buffer, that should be flushed first.
    val depthClearEnable = isClearLane &&
      (flushBufferSelReg === RenderBufferId.Color) &&
      io.flushData.fire
    depthWriteMask(pixel) := Mux(flushActiveReg,
      depthClearEnable,
      stage2.maskReg && enableDepthWriteReg)
    depthWriteVal(pixel) := Mux(flushActiveReg, clearDepthReg, stage2.newDepthReg)
  }

  colorMemory.write(writeAddress, colorWriteVal, colorWriteMask)
  depthMemory.write(writeAddress, depthWriteVal, depthWriteMask)
}
