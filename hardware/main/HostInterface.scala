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

object GpuHostRegisters {
  val Scratchpad0 = 0xfffffff0L
  val Scratchpad1 = 0xfffffff4L
  val Scratchpad2 = 0xfffffff8L
  val Scratchpad3 = 0xfffffffcL
  val HaltCommandProcessor = 0xffffffe0L
  val LoadProgram = 0xffffffe4L
}

/** This exposes a register interface to the host for controlling GPU functions.
  */
class HostInterface extends Module {
  val io = IO(new Bundle {
    val axiSubordinate = Flipped(new Axi4LiteIO)

    val readScratchpadAddr = Input(UInt(2.W))
    val readScratchpadData = Output(UInt(32.W))
    val writeScratchpad = Flipped(Valid(new Bundle {
      val addr = UInt(2.W)
      val data = UInt(32.W)
    }))

    val haltCommandProcessor = Output(Bool())
    val loadProgram = Valid(UInt(32.W))
  })

  val scratchpadRegs = RegInit(VecInit.fill(4)(0.U(32.W)))
  val haltCommandProcessorReg = RegInit(true.B)
  io.haltCommandProcessor := haltCommandProcessorReg
  io.loadProgram.bits := io.axiSubordinate.writeData.bits.data
  io.loadProgram.valid := false.B

  io.readScratchpadData := scratchpadRegs(io.readScratchpadAddr)
  when (io.writeScratchpad.valid) {
    scratchpadRegs(io.writeScratchpad.bits.addr) := io.writeScratchpad.bits.data
  }

  // Write interface
  val writeResponseValidReg = RegInit(false.B)
  val regWriteEn = io.axiSubordinate.writeRequest.valid && io.axiSubordinate.writeData.valid &&
           (!writeResponseValidReg || io.axiSubordinate.writeResponse.ready)

  io.axiSubordinate.writeRequest.ready := regWriteEn
  io.axiSubordinate.writeData.ready := regWriteEn
  when (regWriteEn) {
    switch (io.axiSubordinate.writeRequest.bits.address) {
      is (GpuHostRegisters.Scratchpad0.U) {
        scratchpadRegs(0) := io.axiSubordinate.writeData.bits.data
      }
      is (GpuHostRegisters.Scratchpad1.U) {
        scratchpadRegs(1) := io.axiSubordinate.writeData.bits.data
      }
      is (GpuHostRegisters.Scratchpad2.U) {
        scratchpadRegs(2) := io.axiSubordinate.writeData.bits.data
      }
      is (GpuHostRegisters.Scratchpad3.U) {
        scratchpadRegs(3) := io.axiSubordinate.writeData.bits.data
      }
      is (GpuHostRegisters.HaltCommandProcessor.U) {
        haltCommandProcessorReg := io.axiSubordinate.writeData.bits.data(0)
      }
      is (GpuHostRegisters.LoadProgram.U) {
        io.loadProgram.valid := true.B
      }
    }
  }

  when (regWriteEn) {
    writeResponseValidReg := true.B
  }.elsewhen (io.axiSubordinate.writeResponse.fire) {
    writeResponseValidReg := false.B
  }

  io.axiSubordinate.writeResponse.valid := writeResponseValidReg

  // Read interface
  val rDataReg = RegInit(0.U(32.W))
  val rValidReg = RegInit(false.B)

  io.axiSubordinate.readRequest.ready := !rValidReg
  io.axiSubordinate.readData.valid := rValidReg
  io.axiSubordinate.readData.bits.data := rDataReg
  when (io.axiSubordinate.readRequest.fire) {
    rDataReg := MuxLookup(io.axiSubordinate.readRequest.bits.address, 0.U)(Seq(
      GpuHostRegisters.Scratchpad0.U -> scratchpadRegs(0),
      GpuHostRegisters.Scratchpad1.U -> scratchpadRegs(1),
      GpuHostRegisters.Scratchpad2.U -> scratchpadRegs(2),
      GpuHostRegisters.Scratchpad3.U -> scratchpadRegs(3)
    ))
    rValidReg := true.B
  }.elsewhen(io.axiSubordinate.readData.fire) {
    rValidReg := false.B
  }
}
