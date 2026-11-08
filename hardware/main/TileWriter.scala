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

/** Copies data from the [[TileBuffer]] on-chip SRAM to external memory via
  * the memory interface.
  */
class TileWriter(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val writePort = new MemWritePort
    val flushData = Flipped(Decoupled(new Bundle {
      val depth = Float32()
      val color = Color()
    }))
    val controlBus = Flipped(new ControlBus)
    val done = Output(Bool())
  })

  val pixelsPerWord = cfg.busDataBits / 32
  val wordsPerRow = cfg.tileSizePixels / pixelsPerWord

  val outputAddressReg = RegInit(0.U(cfg.busAddressBits.W))
  val outputStrideReg = RegInit(0.U(32.W))
  val writeStart = WireInit(false.B)
  val clearStart = WireInit(false.B)
  val startBufferSel = WireInit(RenderBufferId.Color)

  when (io.controlBus.writeValue.valid) {
    switch(io.controlBus.writeValue.bits.address) {
      is (ControlRegisterIndex.TileWriteAddressL) {
        outputAddressReg := outputAddressReg(cfg.busAddressBits - 1, 32) ## io.controlBus.writeValue.bits.data
      }
      is (ControlRegisterIndex.TileWriteAddressH) {
        outputAddressReg := io.controlBus.writeValue.bits.data ## outputAddressReg(31, 0)
      }
      is (ControlRegisterIndex.TileWriteStride) {
        outputStrideReg := io.controlBus.writeValue.bits.data
      }
      is (ControlRegisterIndex.TileWriteStart) {
        writeStart := true.B
        startBufferSel := io.controlBus.writeValue.bits.data(0).asTypeOf(RenderBufferId())
      }
      is (ControlRegisterIndex.TileClearStart) {
        clearStart := true.B
        startBufferSel := io.controlBus.writeValue.bits.data(0).asTypeOf(RenderBufferId())
      }
    }
  }

  object State extends ChiselEnum {
    val Idle, IssueBurst, WriteData, Clear = Value
  }

  val stateReg = RegInit(State.Idle)
  val rowReg = RegInit(0.U(log2Ceil(cfg.tileSizePixels).W))
  val wordReg = RegInit(0.U(log2Ceil(wordsPerRow).W)) // count of words written in current row.
  val burstAddressReg = Reg(UInt(cfg.busAddressBits.W))
  val activeStrideReg = Reg(UInt(32.W))
  val lowPixelReg = Reg(UInt(32.W))
  val lowPixelValidReg = RegInit(false.B)
  val highPixelReg = Reg(UInt(cfg.busDataBits.W))
  val highPixelValidReg = RegInit(false.B)
  val clearCountReg = RegInit(0.U(log2Ceil(cfg.totalTilePixels).W))

  io.writePort.burst.valid := stateReg === State.IssueBurst
  io.writePort.burst.bits.address := burstAddressReg
  io.writePort.burst.bits.length := (wordsPerRow - 1).U
  io.writePort.data.valid := stateReg === State.WriteData && highPixelValidReg
  io.writePort.data.bits := highPixelReg ## lowPixelReg
  io.flushData.ready := (stateReg === State.Clear) ||
    (stateReg === State.WriteData && !highPixelValidReg)
  io.done := stateReg === State.Idle

  switch (stateReg) {
    is (State.Idle) {
      when (writeStart) {
        assert(outputAddressReg(2, 0) === 0.U,
          "TileWriter output address must be 8-byte aligned")
        assert(outputStrideReg(2, 0) === 0.U,
          "TileWriter output stride must be 8-byte aligned")
        assert(outputStrideReg >= (cfg.tileSizePixels * 4).U,
          "TileWriter output stride must fit a complete tile row")
        assert(startBufferSel === RenderBufferId.Color,
          "TileWriter currently supports only color buffer writes")
        assert(!highPixelValidReg && !lowPixelValidReg,
          "TileWriter started while previous data is still valid")
        rowReg := 0.U
        burstAddressReg := outputAddressReg
        activeStrideReg := outputStrideReg
        stateReg := State.IssueBurst
      }.elsewhen (clearStart) {
        clearCountReg := 0.U
        stateReg := State.Clear
      }
    }
    is (State.IssueBurst) {
      wordReg := 0.U
      stateReg := State.WriteData
    }
    is (State.WriteData) {
      when (io.flushData.fire) {
        // The data bus width holds two pixels, so we need to wait for two to
        // arrive from the flushData interface before we can issue a write
        // cycle.
        when (lowPixelValidReg) {
          highPixelReg := io.flushData.bits.color.toArgb32
          highPixelValidReg := true.B
        }.otherwise {
          lowPixelReg := io.flushData.bits.color.toArgb32
          lowPixelValidReg := true.B
        }
      }

      when (io.writePort.data.fire) {
        assert(highPixelValidReg && lowPixelValidReg,
          "TileWriter attempted to write data without two valid pixels")
        highPixelValidReg := false.B
        lowPixelValidReg := false.B
        when (wordReg === (wordsPerRow - 1).U) {
          when (rowReg === (cfg.tileSizePixels - 1).U) {
            stateReg := State.Idle
          }.otherwise {
            rowReg := rowReg + 1.U
            burstAddressReg := burstAddressReg + activeStrideReg
            stateReg := State.IssueBurst
          }
        }.otherwise {
          wordReg := wordReg + 1.U
        }
      }
    }
    is (State.Clear) {
      when (io.flushData.fire) {
        when (clearCountReg === (cfg.totalTilePixels - 1).U) {
          stateReg := State.Idle
        }.otherwise {
          clearCountReg := clearCountReg + 1.U
        }
      }
    }
  }

  when (stateReg =/= State.Idle) {
    assert(!writeStart && !clearStart, "TileWriter started while another operation is active")
  }
}
