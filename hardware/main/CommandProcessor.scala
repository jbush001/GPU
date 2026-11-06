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
  val AND    = 1.U(5.W)
  val SUB    = 2.U(5.W)
  val ADD    = 3.U(5.W)
  val OR     = 4.U(5.W)
  val SRL    = 5.U(5.W)
  val LW     = 6.U(5.W)
  val SW     = 7.U(5.W)
  val SRLI   = 8.U(5.W)
  val ADDI   = 9.U(5.W)
  val MOVEHI = 10.U(5.W)
  val MOVE   = 11.U(5.W)
  val BEQ    = 12.U(5.W)
  val BNE    = 13.U(5.W)
  val JAL    = 14.U(5.W)
  val JALR   = 15.U(5.W)
}

object ControlRegister extends ChiselEnum {
  val ClearColor,
    ClearDepth,
    TileWriteFlags,
    StartFlush,
    ShaderAddress,
    NumVaryings,
    TileOffsetX,
    TileOffsetY = Value
}

/** Configuration and control for all units from the command processor.
  */
class ControlBus extends Bundle {
  val writeValue = Valid(new Bundle {
    val address = ControlRegister()
    val data = UInt(32.W)
  })
}

/** The Command Processor interfaces between the host CPU and the GPU
  * execution units. It reads sequential command streams from shared memory,
  * configures hardware state, and orchestrates downstream work pipelines.
  *
  * Command packets are variable length, starting with a single byte opcode.
  * These are fully documented in [[docs/command-format.md]]
  *
  * This uses a minimal RISC pipeline with four stages.
  * - This has one branch delay slot.
  * - Loads must insert one cycle delay slot before the value can be used.
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
  }.otherwise {
    imLoadAddress := 0.U
  }

  val branchTaken = WireInit(false.B)
  val branchPc = WireInit(0.U(log2Up(instructionMemSize).W))
  val writebackIndex = WireInit(0.U(3.W))
  val writebackData = WireInit(0.U(dataWidth.W))
  val writebackValid = WireInit(false.B)

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

  def bypassOperand(regIndex: UInt, regData: UInt): UInt = {
    Mux(writebackValid && writebackIndex === regIndex, writebackData, regData)
  }

  ///////////////////////////////////////////////////////////////////
  // Stage 2: Instruction decode/register fetch
  ///////////////////////////////////////////////////////////////////
  val idValid = RegNext(ifValid, init = false.B)
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
  val exBranchTarget = (idPc + idInstruction.imm.asUInt)(log2Up(instructionMemSize) - 1, 0)
  val exAluOp1 = bypassOperand(idInstruction.rs1, idOperand1Reg)
  val exOp2IsImm = (idInstruction.opcode === CpOpcode.ADDI
    || idInstruction.opcode === CpOpcode.SRLI)
  val exAluOp2 = Mux(exOp2IsImm,
    idInstruction.imm.pad(dataWidth).asUInt,
    bypassOperand(idInstruction.rs2, idOperand2Reg)
  )

  val exMemoryAddress = exAluOp1 + idInstruction.imm.pad(dataWidth).asUInt
  io.controlBus.writeValue.bits.address := (exMemoryAddress(ControlRegister.getWidth - 1, 0)
    .asTypeOf(ControlRegister()))
  io.controlBus.writeValue.bits.data := exAluOp2
  io.controlBus.writeValue.valid := false.B

  val exAluResult = WireInit(0.U(dataWidth.W))
  when (idValid) {
    switch (idInstruction.opcode) {
      is (CpOpcode.ADD, CpOpcode.ADDI) { exAluResult := exAluOp1 + exAluOp2 }
      is (CpOpcode.SUB) { exAluResult := exAluOp1 - exAluOp2 }
      is (CpOpcode.AND) { exAluResult := exAluOp1 & exAluOp2 }
      is (CpOpcode.OR) { exAluResult := exAluOp1 | exAluOp2 }
      is (CpOpcode.SRL, CpOpcode.SRLI) { exAluResult := exAluOp1 >> exAluOp2(4, 0) }
      is (CpOpcode.MOVEHI) { exAluResult := idInstruction.imm.asUInt ## exAluOp1(13, 0) }
      is (CpOpcode.MOVE) { exAluResult := idInstruction.imm.pad(dataWidth).asUInt  }
      is (CpOpcode.SW) {
        when (exMemoryAddress(31)) {
          io.controlBus.writeValue.valid := true.B // Control bus transfer
        }.otherwise {
          dataMem.write(exMemoryAddress, exAluOp2)
        }
      }
      is (CpOpcode.BEQ) {
        when (exAluOp1 === exAluOp2) {
          branchTaken := true.B
          branchPc := exBranchTarget
        }
      }
      is (CpOpcode.BNE) {
        when (exAluOp1 =/= exAluOp2) {
          branchTaken := true.B
          branchPc := exBranchTarget
        }
      }
      is (CpOpcode.JAL) {
        branchTaken := true.B
        branchPc := exBranchTarget
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
        CpOpcode.MOVE) {
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
