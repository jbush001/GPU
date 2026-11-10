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

  val outputAddressReg = RegInit(VecInit(Seq.fill(cfg.busAddressBits / 32)(0.U(32.W))))
  val outputStrideReg = RegInit(0.U(32.W))
  val writeStart = WireInit(false.B)
  val clearStart = WireInit(false.B)
  val startBufferSel = WireInit(RenderBufferId.Color)

  when (io.controlBus.regWrite.valid) {
    switch(io.controlBus.regWrite.bits.address) {
      is (ControlRegisterIndex.TileWriteAddressL) {
        outputAddressReg(0) := io.controlBus.regWrite.bits.data
      }
      is (ControlRegisterIndex.TileWriteAddressH) {
        outputAddressReg(1) := io.controlBus.regWrite.bits.data
      }
      is (ControlRegisterIndex.TileWriteStride) {
        outputStrideReg := io.controlBus.regWrite.bits.data
      }
      is (ControlRegisterIndex.TileWriteStart) {
        writeStart := true.B
        startBufferSel := io.controlBus.regWrite.bits.data(0).asTypeOf(RenderBufferId())
      }
      is (ControlRegisterIndex.TileClearStart) {
        clearStart := true.B
        startBufferSel := io.controlBus.regWrite.bits.data(0).asTypeOf(RenderBufferId())
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
  val clearCountReg = RegInit(0.U(log2Ceil(cfg.totalTilePixels).W))
  val upsizer = Module(new AxiDataUpsizer)

  io.writePort.burst.valid := stateReg === State.IssueBurst
  io.writePort.burst.bits.address := burstAddressReg
  io.writePort.burst.bits.length := (wordsPerRow - 1).U
  val writing = stateReg === State.WriteData
  upsizer.io.flushData.valid := io.flushData.valid && writing
  upsizer.io.flushData.bits := io.flushData.bits.color.toArgb32
  io.flushData.ready := stateReg === State.Clear ||
    (writing && upsizer.io.flushData.ready)
  io.writePort.data <> upsizer.io.data
  io.done := stateReg === State.Idle

  switch (stateReg) {
    is (State.Idle) {
      when (writeStart) {
        assert(outputAddressReg.asUInt(2, 0) === 0.U,
          "TileWriter output address must be 8-byte aligned")
        assert(outputStrideReg(2, 0) === 0.U,
          "TileWriter output stride must be 8-byte aligned")
        assert(outputStrideReg >= (cfg.tileSizePixels * 4).U,
          "TileWriter output stride must fit a complete tile row")
        assert(startBufferSel === RenderBufferId.Color,
          "TileWriter currently supports only color buffer writes")
        rowReg := 0.U
        burstAddressReg := outputAddressReg.asUInt
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
      when (io.writePort.data.fire) {
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

class AxiDataUpsizer(implicit val cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val flushData = Flipped(Decoupled(UInt(32.W)))
    val data = Decoupled(UInt(cfg.busDataBits.W))
  })

  val lowReg = Reg(UInt(32.W))
  val hasLow = RegInit(false.B)

  io.flushData.ready := !hasLow || io.data.ready

  io.data.valid := hasLow && io.flushData.valid
  io.data.bits := io.flushData.bits ## lowReg

  when(io.flushData.fire) {
    when(hasLow) {
      hasLow := false.B
    }.otherwise {
      lowReg := io.flushData.bits
      hasLow := true.B
    }
  }
}
