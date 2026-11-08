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

class ShadedQuad(implicit cfg: GpuConfig) extends Bundle {
  val location = Point2D() // In pixels
  val mask = Bits(Consts.pixelsPerQuad.W)
  val colors = Vec(Consts.pixelsPerQuad, Color())
  val depths = Vec(Consts.pixelsPerQuad, Float32())
}

/** Working storage for color and depth information for the framebuffer tile
  * currently being rendered.
  *
  * Shaded 2x2 pixel quads arrive from [[PixelShaderConductor]]. Each is
  * depth-tested and alpha-blended with the stored values, and the surviving
  * color and depth are written back. This data is stored entirely in on-chip
  * SRAM. When the tile is complete, a flush operation streams its contents
  * to be written back to external memory and clears the buffer for the next
  * tile.
  *
  * Values are stored in a native formats ([[Color]] and [[Float32]]).
  * Conversion to the external-memory format happens downstream, during the
  * flush.
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
    val shadedQuad = Flipped(Valid(new ShadedQuad))
    val controlBus = Flipped(new ControlBus)
    val flushData = Decoupled(new Bundle {
      val depth = Float32()
      val color = Color()
    })
  })

  // Configuration registers
  val startFlush = WireInit(false.B)
  val flushBufferSel = RegInit(RenderBufferId.Color) // depth or color buffer
  val clearColor = RegInit(Color(Seq.fill(Color.numChannels)(0.U(Color.channelBits.W))))
  val clearDepth = RegInit(Float32(1.0f))
  val enableDepthWrite = RegInit(true.B)
  val enableDepthCheck = RegInit(true.B)
  val enableBlend = RegInit(false.B)

  // Handle configuration register writes from [[CommandProcessor]]
  when (io.controlBus.regWrite.valid) {
    switch (io.controlBus.regWrite.bits.address) {
      is (ControlRegisterIndex.ClearColor) {
        clearColor := Color.fromArgb32(io.controlBus.regWrite.bits.data)
      }
      is (ControlRegisterIndex.ClearDepth) {
        clearDepth := Float32(io.controlBus.regWrite.bits.data)
      }
      is (ControlRegisterIndex.TileWriteFlags) {
        enableDepthWrite := io.controlBus.regWrite.bits.data(0)
        enableDepthCheck := io.controlBus.regWrite.bits.data(1)
        enableBlend := io.controlBus.regWrite.bits.data(2)
      }
      is (ControlRegisterIndex.TileWriteStart, ControlRegisterIndex.TileClearStart) {
        startFlush := true.B
        flushBufferSel := io.controlBus.regWrite.bits.data(0).asTypeOf(RenderBufferId())
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
  val inputQuadAddress = (io.shadedQuad.bits.location.y(cfg.tileCoordBits - 1, 1) ##
    io.shadedQuad.bits.location.x(cfg.tileCoordBits - 1, 1)).asUInt
  val readAddress = Mux(flushActiveReg, flushAddress, inputQuadAddress)
  val colorReadVal = colorMemory.read(readAddress, io.shadedQuad.valid || flushActiveReg)
  val depthReadVal = depthMemory.read(readAddress, io.shadedQuad.valid || flushActiveReg)

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
    val stage1 = new {
      val newColorReg = RegNext(io.shadedQuad.bits.colors(pixel))
      val newDepthReg = RegNext(io.shadedQuad.bits.depths(pixel))
      val maskReg = RegNext(io.shadedQuad.bits.mask(pixel) && io.shadedQuad.valid, false.B)
    }

    // Stage 2: visibility checks, destination blending
    val stage2 = new {
      val oneMinusAlpha = Color.maxChannelValue.U - stage1.newColorReg.alpha
      val oldWeightedColorReg = RegNext(colorReadVal(pixel).scale(oneMinusAlpha))
      val newColorReg = RegNext(stage1.newColorReg)
      val newDepthReg = RegNext(stage1.newDepthReg)
      val maskReg = RegNext(stage1.maskReg &&
        (!enableDepthCheck || depthReadVal(pixel) > stage1.newDepthReg), false.B)
    }

    // Stage 3: Blend, writeback
    val blended = Mux(enableBlend,
      stage2.oldWeightedColorReg +| stage2.newColorReg,
      stage2.newColorReg)

    val isClearLane = (flushBankReg === pixel.U)

    val colorClearEnable = isClearLane &&
      (flushBufferSel === RenderBufferId.Color) &&
      io.flushData.fire
    colorWriteMask(pixel) := Mux(flushActiveReg, colorClearEnable, stage2.maskReg)
    colorWriteVal(pixel) := Mux(flushActiveReg, clearColor, blended)

    val depthClearEnable = isClearLane &&
      (flushBufferSel === RenderBufferId.Depth) &&
      io.flushData.fire
    depthWriteMask(pixel) := Mux(flushActiveReg,
      depthClearEnable,
      stage2.maskReg && enableDepthWrite)
    depthWriteVal(pixel) := Mux(flushActiveReg, clearDepth, stage2.newDepthReg)
  }

  colorMemory.write(writeAddress, colorWriteVal, colorWriteMask)
  depthMemory.write(writeAddress, depthWriteVal, depthWriteMask)
}
