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

object CpOpcode {
  val AND    = 0.U
  val OR     = 1.U
  val XOR    = 2.U
  val ADD    = 3.U
  val SUB    = 4.U
  val SLTU   = 5.U
  val SLL    = 6.U
  val SRL    = 8.U
  val ADDI   = 9.U
  val SLTUI  = 10.U
  val SLLI   = 11.U
  val SRLI   = 12.U
  val LW     = 13.U
  val SW     = 14.U
  val MOVEHI = 15.U
  val MOVEI  = 16.U
  val BEQ    = 17.U
  val BNE    = 18.U
  val JAL    = 19.U
  val JALR   = 20.U
}

object ControlRegisterIndex {
  val width = 8

  val ClearColor     = 0.U
  val ClearDepth     = 1.U
  val TileWriteFlags = 2.U
  val StartFlush     = 3.U
  val ShaderAddressL = 4.U
  val ShaderAddressH = 5.U
  val NumVaryings    = 6.U
  val TileOffsetX    = 7.U
  val TileOffsetY    = 8.U
}

class ControlBus extends Bundle {
  val writeValue = Valid(new Bundle {
    val address = UInt(ControlRegisterIndex.width.W)
    val data    = UInt(32.W)
  })
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
    val readPort = new MemReadPort
    val controlBus = new ControlBus
    val halt = Input(Bool())
    val loadProgram = Flipped(Valid(UInt(32.W)))
  })

  val dataWidth:          Int = 32
  val instructionMemSize: Int = 1024 // words
  val dataMemSize:        Int = 1024 // words
  val numRegisters:       Int = 8

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
  io.controlBus.writeValue.bits.address := exMemoryAddress(ControlRegisterIndex.width - 1, 0)
  io.controlBus.writeValue.bits.data := exAluOp2
  io.controlBus.writeValue.valid := false.B

  val exAluResult = WireInit(0.U(dataWidth.W))
  when (idValid) {
    switch (idInstruction.opcode) {
      is (CpOpcode.ADD, CpOpcode.ADDI) { exAluResult := exAluOp1 + exAluOp2 }
      is (CpOpcode.SUB) { exAluResult := exAluOp1 - exAluOp2 }
      is (CpOpcode.AND) { exAluResult := exAluOp1 & exAluOp2 }
      is (CpOpcode.OR) { exAluResult := exAluOp1 | exAluOp2 }
      is (CpOpcode.XOR) { exAluResult := exAluOp1 ^ exAluOp2 }
      is (CpOpcode.SRL, CpOpcode.SRLI) { exAluResult := exAluOp1 >> exAluOp2(4, 0) }
      is (CpOpcode.SLL, CpOpcode.SLLI) { exAluResult := exAluOp1 << exAluOp2(4, 0) }
      is (CpOpcode.SLTU, CpOpcode.SLTUI) { exAluResult := (exAluOp1 < exAluOp2).asUInt }
      is (CpOpcode.MOVEHI) { exAluResult := idInstruction.imm.asUInt ## exAluOp1(13, 0) }
      is (CpOpcode.MOVEI) { exAluResult := idInstruction.imm.pad(dataWidth).asUInt  }
      is (CpOpcode.SW) {
        when (exMemoryAddress(31, 8) === 0xffffff.U) {
          io.controlBus.writeValue.valid := true.B // Control bus transfer
        }.otherwise {
          dataMem.write(exMemoryAddress, exAluOp2)
        }
      }
      is (CpOpcode.BEQ) {
        when (exAluOp1 === exAluOp2) {
          branchTaken := true.B
        }
      }
      is (CpOpcode.BNE) {
        when (exAluOp1 =/= exAluOp2) {
          branchTaken := true.B
        }
      }
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
        writebackData := exMemoryResult
        writebackValid := true.B
      }
      is(CpOpcode.JAL, CpOpcode.JALR) {
        writebackData := exPc + 1.U
        writebackValid := true.B
      }
    }
  }

  // XXX Stub need to implement this.
  io.readPort.burst.valid := false.B
  io.readPort.burst.bits.address := 0.U
  io.readPort.burst.bits.length := 0.U
  io.readPort.data.ready := true.B
}
