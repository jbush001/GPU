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

object ControlRegisterIndex {
  val width = 8

  val ClearColor        = 0.U
  val ClearDepth        = 1.U
  val TileWriteFlags    = 2.U
  val ShaderAddressL    = 4.U
  val ShaderAddressH    = 5.U
  val NumVaryings       = 6.U
  val TileOffsetX       = 7.U
  val TileOffsetY       = 8.U
  val TileWriteAddressL = 9.U
  val TileWriteAddressH = 10.U
  val TileWriteStride   = 11.U // bytes between the start of two adjacent rows
  val TileWriteStart    = 12.U
  val TileClearStart    = 13.U
}

class ControlBus extends Bundle {
  val regWrite = Valid(new Bundle {
    val address = UInt(ControlRegisterIndex.width.W)
    val data    = UInt(32.W)
  })
}

class CpMmio extends Bundle {
  val regWrite = Valid(new Bundle {
    val address = UInt(32.W)
    val data    = UInt(32.W)
  })

  val regReadAddr = Valid(UInt(32.W))
  val regReadData = Input(UInt(32.W))
}

/** Interfaces between the host CPU and the GPU.
  * This reads commands from shared memory, and coordinates activity on the GPU
  * pipeline units
  *
  * This contains a small RISC pipeline running firmware that is dynamically
  * loaded from the host. Command packets are variable length, starting with a
  * single byte opcode. These are documented in [[docs/command-format.md]]
  *
  * @note Loads must insert one cycle delay slot before the value can be used.
  */
class CommandProcessor(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val memReadPort = new MemReadPort
    val controlBus = new ControlBus
    val halt = Input(Bool())
    val loadProgram = Flipped(Valid(UInt(32.W)))
  })

  object RegAddresses {
    val DmaAddrL = 0xfffffe00L.U
    val DmaAddrH = 0xfffffe01L.U
    val DmaLength = 0xfffffe02L.U
    val DmaStatus = 0xfffffe03L.U
    val DmaData = 0xfffffe04L.U
  }

  val pipeline = Module(new CpPipeline)
  val dma = Module(new CpDma)
  dma.io.memReadPort <> io.memReadPort

  val dmaStartAddress = RegInit(VecInit(Seq.fill(cfg.busAddressBits / 32)(0.U(32.W))))
  dma.io.startTransfer.bits.startAddress := dmaStartAddress.asUInt

  // Since the length write kicks off the transfer, we can use the payload here.
  // (note divide the length by half since the word has half the bus data width)
  dma.io.startTransfer.bits.length := pipeline.io.mmio.regWrite.bits.data(4, 1)

  pipeline.io.halt := io.halt
  pipeline.io.loadProgram := io.loadProgram
  val mmioAddr = pipeline.io.mmio.regWrite.bits.address
  io.controlBus.regWrite.bits.address := mmioAddr(ControlRegisterIndex.width - 1, 0)
  io.controlBus.regWrite.bits.data := pipeline.io.mmio.regWrite.bits.data
  io.controlBus.regWrite.valid := false.B
  dma.io.startTransfer.valid := false.B
  when (pipeline.io.mmio.regWrite.valid) {
    // MMIO writes
    when (mmioAddr(31, 8) === 0xffffff.U) {
      io.controlBus.regWrite.valid := true.B
    }.otherwise {
      switch (mmioAddr) {
        is(RegAddresses.DmaAddrL) {
          dmaStartAddress(0) := pipeline.io.mmio.regWrite.bits.data
        }
        is(RegAddresses.DmaAddrH) {
          dmaStartAddress(1) := pipeline.io.mmio.regWrite.bits.data
        }
        is(RegAddresses.DmaLength) {
          // A side effect of writing the DMA length is the start of a transfer.
          // The length is hardwired above.
          dma.io.startTransfer.valid := true.B
        }
      }
    }
  }

  val mmioReadResponse = Reg(UInt(32.W))
  dma.io.dmaData.ready := false.B
  when (pipeline.io.mmio.regReadAddr.valid) {
    switch (pipeline.io.mmio.regReadAddr.bits) {
      is (RegAddresses.DmaStatus) {
        mmioReadResponse := dma.io.dmaData.valid.asUInt
      }

      is (RegAddresses.DmaData) {
        mmioReadResponse := dma.io.dmaData.bits
        dma.io.dmaData.ready := true.B
      }
    }
  }

  pipeline.io.mmio.regReadData := mmioReadResponse
}

/** Reads data from system memory and provides a Decoupled interface. */
class CpDma(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val memReadPort = new MemReadPort
    val startTransfer = Flipped(Valid(new Bundle {
      val startAddress = UInt(cfg.busAddressBits.W)
      val length = UInt(4.W)
    }))

    val dmaData = Decoupled(UInt(32.W))
  })

  val readFifo = Queue(io.memReadPort.data, entries = 16)
  val downsizer = Module(new DecoupledDownsizer)
  downsizer.io.wide <> readFifo
  io.dmaData <> downsizer.io.narrow

  // If the FIFO is not empty, don't start a new burst.
  io.memReadPort.burst.valid := io.startTransfer.valid && !readFifo.valid
  io.memReadPort.burst.bits.address := io.startTransfer.bits.startAddress
  io.memReadPort.burst.bits.length := io.startTransfer.bits.length
}

/** Adapts the 64-bit data bus to a 32-bit Decoupled interface. */
class DecoupledDownsizer(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val wide = Flipped(Decoupled(UInt(cfg.busDataBits.W)))
    val narrow = Decoupled(UInt(32.W))
  })

  val selectUpper = RegInit(false.B)

  io.narrow.valid := io.wide.valid
  io.wide.ready  := io.narrow.ready && selectUpper
  io.narrow.bits := Mux(selectUpper, io.wide.bits(63, 32), io.wide.bits(31, 0))
  when(io.narrow.fire) {
    selectUpper := !selectUpper
  }
}

// Note: we've chosen an encoding where arithmetic instrutions
// that have an immediate operand have the high bit set, and
// the immediate and register forms of instructions have the same
// four low bits. This basically makes immediate forms free
// in terms of decoder hardware.
object CpOpcode {
  val AND    = 0.U
  val OR     = 1.U
  val XOR    = 2.U
  val ADD    = 3.U
  val SUB    = 4.U
  val SLTU   = 5.U
  val SLL    = 6.U
  val SRL    = 7.U
  val BEQ    = 8.U
  val BNE    = 9.U
  val ANDI   = 16.U
  val ORI    = 17.U
  val XORI   = 18.U
  val ADDI   = 19.U
  val SUBI   = 20.U
  val SLTUI  = 21.U
  val SLLI   = 22.U
  val SRLI   = 23.U
  val BEQI   = 24.U
  val BNEI   = 25.U
  val LW     = 26.U
  val SW     = 27.U
  val MOVEHI = 28.U
  val MOVEI  = 29.U
  val JAL    = 30.U
  val JALR   = 31.U
}

/**
  * The command processor pipeline.
  * This is a four-stage RISC Processor.
  */
class CpPipeline extends Module {
  val io = IO(new Bundle {
    val halt = Input(Bool())
    val loadProgram = Flipped(Valid(UInt(32.W)))
    val mmio = new CpMmio
  })

  val dataWidth:          Int = 32
  val instructionMemSize: Int = 1024 // words
  val dataMemSize:        Int = 1024 // words
  val numRegisters:       Int = 8

  require(isPow2(dataMemSize))

  class Instruction extends Bundle {
    val imm = SInt(18.W)   // 31-14
    val rs2 = UInt(3.W)    // 13-11
    val rs1 = UInt(3.W)    // 10-8
    val rd  = UInt(3.W)    // 7-5
    val opcode = UInt(5.W) // 4-0
  }

  val instructionMem = SyncReadMem(instructionMemSize, new Instruction)
  val dataMem = SyncReadMem(dataMemSize, UInt(dataWidth.W))
  val registerFile = RegInit(VecInit(Seq.fill(numRegisters)(0.U(dataWidth.W))))
  val pcReg = RegInit(0.U(log2Up(instructionMemSize).W))

  // Load code into instruction memory
  val imLoadAddress = RegInit(0.U(log2Up(instructionMemSize).W))
  when (io.halt) {
    when (io.loadProgram.valid) {
      instructionMem.write(imLoadAddress, io.loadProgram.bits.asTypeOf(new Instruction))
      imLoadAddress := imLoadAddress + 1.U
    }

    pcReg := 0.U // When this exits halt, program will restart
  }.otherwise {
    imLoadAddress := 0.U
  }

  val branchTaken = WireInit(false.B)
  val branchPc = WireInit(0.U(log2Up(instructionMemSize).W))
  val writebackIndex = WireInit(0.U(3.W))
  val writebackData = WireInit(0.U(dataWidth.W))
  val writebackValid = WireInit(false.B)

  def bypassOperand(regIndex: UInt, regData: UInt): UInt = {
    Mux(writebackValid && writebackIndex === regIndex, writebackData, regData)
  }

  ///////////////////////////////////////////////////////////////////
  // Stage 1: Instruction fetch
  ///////////////////////////////////////////////////////////////////
  val ifNextPc = WireInit(pcReg + 1.U)
  when (io.halt) {
    ifNextPc := ~0.U(log2Up(instructionMemSize).W)
  }.elsewhen(branchTaken) {
    ifNextPc := branchPc
  }

  val ifValid = RegNext(!io.halt, init = false.B)
  val ifInstruction = instructionMem.read(ifNextPc)
  pcReg := ifNextPc

  ///////////////////////////////////////////////////////////////////
  // Stage 2: Instruction decode/register fetch
  ///////////////////////////////////////////////////////////////////
  val idValid = RegNext(ifValid && !branchTaken, init = false.B)
  val idPc = RegNext(pcReg)
  val idInstruction = RegNext(ifInstruction)
  val idOperand1Reg = RegNext(bypassOperand(ifInstruction.rs1, registerFile(ifInstruction.rs1)))
  val idOperand2Reg = RegNext(bypassOperand(ifInstruction.rs2, registerFile(ifInstruction.rs2)))
  when (writebackValid) {
    registerFile(writebackIndex) := writebackData
  }

  ///////////////////////////////////////////////////////////////////
  // Stage 3: Execute/data memory access
  ///////////////////////////////////////////////////////////////////
  branchPc := (idPc + idInstruction.imm.asUInt)(log2Up(instructionMemSize) - 1, 0)
  val exAluOp1 = bypassOperand(idInstruction.rs1, idOperand1Reg)
  val exOp2IsImm = (idInstruction.opcode === CpOpcode.ADDI
    || idInstruction.opcode === CpOpcode.SRLI
    || idInstruction.opcode === CpOpcode.SLLI
    || idInstruction.opcode === CpOpcode.SLTUI)
  val exAluOp2 = Mux(exOp2IsImm,
    idInstruction.imm.pad(dataWidth).asUInt,
    bypassOperand(idInstruction.rs2, idOperand2Reg)
  )

  val exMemoryAddress = exAluOp1 + idInstruction.imm.pad(dataWidth).asUInt
  val addrIsMmio = exMemoryAddress(31, log2Up(dataMemSize)).orR
  io.mmio.regWrite.bits.address := exMemoryAddress
  io.mmio.regWrite.bits.data := exAluOp2
  io.mmio.regWrite.valid := false.B
  io.mmio.regReadAddr.bits := exMemoryAddress
  io.mmio.regReadAddr.valid := false.B

  val exAluResult = WireInit(0.U(dataWidth.W))
  val exWritebackMmio = WireInit(false.B)
  when (idValid) {
    switch (idInstruction.opcode) {
      is (CpOpcode.AND, CpOpcode.ANDI) { exAluResult := exAluOp1 & exAluOp2 }
      is (CpOpcode.OR, CpOpcode.ORI) { exAluResult := exAluOp1 | exAluOp2 }
      is (CpOpcode.XOR, CpOpcode.XORI) { exAluResult := exAluOp1 ^ exAluOp2 }
      is (CpOpcode.ADD, CpOpcode.ADDI) { exAluResult := exAluOp1 + exAluOp2 }
      is (CpOpcode.SUB, CpOpcode.SUBI) { exAluResult := exAluOp1 - exAluOp2 }
      is (CpOpcode.SLTU, CpOpcode.SLTUI) { exAluResult := (exAluOp1 < exAluOp2).asUInt }
      is (CpOpcode.SLL, CpOpcode.SLLI) { exAluResult := exAluOp1 << exAluOp2(4, 0) }
      is (CpOpcode.SRL, CpOpcode.SRLI) { exAluResult := exAluOp1 >> exAluOp2(4, 0) }
      is (CpOpcode.BEQ, CpOpcode.BEQI) {
        when (exAluOp1 === exAluOp2) {
          branchTaken := true.B
        }
      }
      is (CpOpcode.BNE, CpOpcode.BNEI) {
        when (exAluOp1 =/= exAluOp2) {
          branchTaken := true.B
        }
      }
      is (CpOpcode.LW) {
        when (addrIsMmio) {
          io.mmio.regReadAddr.valid := true.B
          exWritebackMmio := true.B
        }
      }
      is (CpOpcode.SW) {
        when (addrIsMmio) {
          io.mmio.regWrite.valid := true.B
        }.otherwise {
          dataMem.write(exMemoryAddress, exAluOp2)
        }
      }
      is (CpOpcode.MOVEHI) { exAluResult := idInstruction.imm.asUInt ## exAluOp1(13, 0) }
      is (CpOpcode.MOVEI) { exAluResult := idInstruction.imm.pad(dataWidth).asUInt  }
      is (CpOpcode.JAL) {
        branchTaken := true.B
      }
      is (CpOpcode.JALR) {
        branchTaken := true.B
        branchPc := exMemoryAddress(log2Up(instructionMemSize) - 1, 0)
      }
    }
  }

  val exValid = RegNext(idValid, init = false.B)
  val exPc = RegNext(idPc)
  val exInstruction = RegNext(idInstruction)
  val exMemoryResult = dataMem.read(exMemoryAddress)
  val exAluResultReg = RegNext(exAluResult)
  val exWritebackMmioReg = RegNext(exWritebackMmio)

  ///////////////////////////////////////////////////////////////////
  // Stage 4: Writeback
  ///////////////////////////////////////////////////////////////////
  when (exValid) {
    writebackIndex := exInstruction.rd
    switch(exInstruction.opcode) {
      is(CpOpcode.ADD, CpOpcode.ADDI, CpOpcode.SUB, CpOpcode.AND,
        CpOpcode.OR, CpOpcode.SRL, CpOpcode.SRLI, CpOpcode.MOVEHI,
        CpOpcode.MOVEI, CpOpcode.XOR, CpOpcode.SLTU, CpOpcode.SLTUI,
        CpOpcode.SLL, CpOpcode.SLLI) {
        writebackData := exAluResultReg
        writebackValid := true.B
      }
      is(CpOpcode.LW) {
        writebackValid := true.B
        when (exWritebackMmioReg) {
          writebackData := io.mmio.regReadData
        }.otherwise {
          writebackData := exMemoryResult
        }
      }
      is(CpOpcode.JAL, CpOpcode.JALR) {
        writebackData := exPc + 1.U
        writebackValid := true.B
      }
    }
  }
}

