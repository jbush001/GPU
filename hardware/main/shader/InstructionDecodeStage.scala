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

package gpu.shader

import chisel3._
import chisel3.util._
import gpu._


//               31      27         21 20         14 13          7 6           0
//              +-------+-------------+-------------+-------------+-------------+
// R: Arith     |       |   rs2 (7)   |    rs1 (7)  |    rd (7)   |  opcode (7) |
//              +-------+-------------+-+-----------+-------------+-------------+
// B: Branch    |       offset[18:7]    |  rs1 (6)  | offset[6:0] |  opcode (7) |
//              +-----------------------+-----------+-------------+-------------+
// X: Inherent  |                    unused (25)                  |  opcode (7) |
//              +-------------------------------+---+-------------+-------------+
// K: Constant  |            value (16)         |   |    rd (7)   |  opcode (7) |
//              +-------------------------------+---+-------------+-------------+
//

class WritebackRequest(implicit cfg: GpuConfig) extends Bundle {
  val thread = UInt(log2Up(cfg.shaderThreads).W)
  val regId = UInt(7.W)
  val value = Vec(cfg.shaderVectorLanes, UInt(32.W))
  val mask = UInt(cfg.shaderVectorLanes.W)
}

/**
  * Instruction decode stage. This stage decodes the instruction and reads the
  * source registers. It also handles writing back to registers.
  * @todo registers 32-63 are reserved for special purposes. Currently not handled.
  */
class InstructionDecodeStage(implicit val cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val input = Flipped(Valid(new FetchResponse))

    val output = Valid(new Bundle {
      val opcode = UInt(7.W)
      val destReg = UInt(7.W)
      val operand1 = Vec(cfg.shaderVectorLanes, UInt(32.W))
      val operand2 = Vec(cfg.shaderVectorLanes, UInt(32.W))
      val pc = UInt(cfg.busAddressBits.W)
      val thread = UInt(log2Up(cfg.shaderThreads).W)
    })

    val writeback = Flipped(Valid(new WritebackRequest))
  })

  val numRegisters = 64

  val scalarRegisters = SyncReadMem(cfg.shaderThreads * numRegisters,
    UInt(32.W))
  val vectorRegisters = SyncReadMem(cfg.shaderThreads * numRegisters,
    Vec(cfg.shaderVectorLanes, UInt(32.W)))

  case class InstEntry(
    mnenomic: String = "unknown",
    hasDest: Boolean = false,
    isBranch: Boolean = false,
    isLoadConst: Boolean = false
  )

  val instructionTable: Map[Int, InstEntry] = Map(
    // opcode      mnemonic   hasDest isBranch isLoadConst
    0 -> InstEntry("halt",    false,  false,   false),
    1 -> InstEntry("and",     true,   false,   false),
    2 -> InstEntry("or",      true,   false,   false),
    3 -> InstEntry("xor",     true,   false,   false),
    4 -> InstEntry("addi",    true,   false,   false),
    5 -> InstEntry("subi",    true,   false,   false),
    6 -> InstEntry("muli",    true,   false,   false),
    7 -> InstEntry("mulh",    true,   false,   false),
    8 -> InstEntry("lsl",     true,   false,   false),
    9 -> InstEntry("asr",     true,   false,   false),
    10 -> InstEntry("lsr",    true,   false,   false),
    11 -> InstEntry("addf",   true,   false,   false),
    12 -> InstEntry("subf",   true,   false,   false),
    13 -> InstEntry("mulf",   true,   false,   false),
    14 -> InstEntry("recip",  true,   false,   false),
    15 -> InstEntry("ftoi",   true,   false,   false),
    16 -> InstEntry("itof",   true,   false,   false),
    17 -> InstEntry("setgtf", true,   false,   false),
    18 -> InstEntry("setltf", true,   false,   false),
    19 -> InstEntry("setgei", true,   false,   false),
    20 -> InstEntry("setlti", true,   false,   false),
    21 -> InstEntry("setgeu", true,   false,   false),
    22 -> InstEntry("setltu", true,   false,   false),
    23 -> InstEntry("seteq",  true,   false,   false),
    24 -> InstEntry("setne",  true,   false,   false),
    25 -> InstEntry("bnz",    false,  true,    false),
    26 -> InstEntry("bz",     false,  true,    false),
    27 -> InstEntry("j",      false,  true,    false),
    28 -> InstEntry("loadlo", true,   false,   true),
    29 -> InstEntry("loadhi", true,   false,   true),
  )

  // This will presumably synthesize into random logic.
  val hasDestLookup = VecInit((0 until 32).map { i =>
    instructionTable.getOrElse(i, InstEntry()).hasDest.B
  })

  val isBranchLookup = VecInit((0 until 32).map { i =>
    instructionTable.getOrElse(i, InstEntry()).isBranch.B
  })

  val isLoadConstLookup = VecInit((0 until 32).map { i =>
    instructionTable.getOrElse(i, InstEntry()).isLoadConst.B
  })

  val operand1Reg = Wire(UInt(7.W))
  when (isLoadConstLookup(io.input.bits.instruction(4, 0))) {
    // The dest reg is the first operand for load constant instructions.
    operand1Reg := io.input.bits.instruction(13, 7)
  }.otherwise {
    operand1Reg := io.input.bits.instruction(20, 14)
  }

  when (operand1Reg(6)) {
    io.output.bits.operand1 := vectorRegisters.read(Cat(io.input.bits.thread, operand1Reg(5, 0)))
  }.otherwise {
    // Need to duplicate this across lanes
    io.output.bits.operand1 := VecInit(Seq.fill(cfg.shaderVectorLanes)(scalarRegisters.read(Cat(io.input.bits.thread, operand1Reg(5, 0)))))
  }

  val operand2Reg = io.input.bits.instruction(27, 21)
  when (operand2Reg(6)) {
    io.output.bits.operand2 := vectorRegisters.read(Cat(io.input.bits.thread, operand2Reg(5, 0)))
  }.otherwise {
    // Need to duplicate this across lanes
    io.output.bits.operand2 := VecInit(Seq.fill(cfg.shaderVectorLanes)(scalarRegisters.read(
      Cat(io.input.bits.thread, operand2Reg(5, 0)))))
  }

  when (io.writeback.valid) {
    when (io.writeback.bits.regId(6)) {
      vectorRegisters.write(Cat(io.writeback.bits.thread, io.writeback.bits.regId(5, 0)),
        io.writeback.bits.value, io.writeback.bits.mask.asBools)
    }.otherwise {
      scalarRegisters.write(Cat(io.writeback.bits.thread, io.writeback.bits.regId(5, 0)),
        io.writeback.bits.value(0))
    }
  }

  io.output.valid := RegNext(io.input.valid)
  io.output.bits.opcode := RegNext(io.input.bits.instruction(6, 0))
  io.output.bits.pc := RegNext(io.input.bits.pc)
  io.output.bits.thread := RegNext(io.input.bits.thread)
  io.output.bits.destReg := RegNext(io.input.bits.instruction(13, 7))
}
