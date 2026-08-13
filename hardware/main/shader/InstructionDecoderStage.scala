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

class DecodedInstruction (implicit val cfg: GpuConfig) extends Bundle {
  val numRegisters = 64
  val regIdWidth = log2Up(numRegisters)

  val pc = UInt(cfg.busAddressBits.W)
  val thread = UInt(log2Up(cfg.shaderThreads).W)

  val hasDest = Bool()
  val destReg = UInt(regIdWidth.W)
  val hasSource1 = Bool()
  val isCmp = Bool()
  val source1Reg = UInt(regIdWidth.W)
  val hasSource2 = Bool()
  val source2Reg = UInt(regIdWidth.W)
  val opcode = UInt(7.W)
  val immediateValue = UInt(19.W)
}

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
class InstructionDecodeStage(implicit val cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val input = Flipped(Valid(new FetchResponse))
    val output = Valid(new DecodedInstruction)
  })

  // Decode instruction
  val decoded = Wire(new DecodedInstruction)

  case class InstEntry(
    mnenomic: String = "unknown",
    hasDest: Boolean = false,
    hasSrc1: Boolean = false,
    isCmp: Boolean = false,
    hasSrc2: Boolean = false,
    isBranch: Boolean = false,
    isLoadConst: Boolean = false
  )

  val instructionTable: Map[Int, InstEntry] = Map(
    // opcode      mnemonic   hasDest hasSrc1 hasSrc2 isBranch isLoadConst
    0 -> InstEntry("halt",    false,  false,  false,  false,   false),
    1 -> InstEntry("and",     true,   true,   true,   false,   false),
    2 -> InstEntry("or",      true,   true,   true,   false,   false),
    3 -> InstEntry("xor",     true,   true,   true,   false,   false),
    4 -> InstEntry("addi",    true,   true,   true,   false,   false),
    5 -> InstEntry("subi",    true,   true,   true,   false,   false),
    6 -> InstEntry("muli",    true,   true,   true,   false,   false),
    7 -> InstEntry("mulh",    true,   true,   true,   false,   false),
    8 -> InstEntry("lsl",     true,   true,   true,   false,   false),
    9 -> InstEntry("asr",     true,   true,   true,   false,   false),
    10 -> InstEntry("lsr",    true,   true,   true,   false,   false),
    11 -> InstEntry("addf",   true,   true,   true,   false,   false),
    12 -> InstEntry("subf",   true,   true,   true,   false,   false),
    13 -> InstEntry("mulf",   true,   true,   true,   false,   false),
    14 -> InstEntry("recip",  true,   true,   false,  false,   false),
    15 -> InstEntry("ftoi",   true,   true,   false,  false,   false),
    16 -> InstEntry("itof",   true,   true,   false,  false,   false),
    17 -> InstEntry("setgtf", true,   true,   true,   false,   false),
    18 -> InstEntry("setltf", true,   true,   true,   false,   false),
    19 -> InstEntry("setgei", true,   true,   true,   false,   false),
    20 -> InstEntry("setlti", true,   true,   true,   false,   false),
    21 -> InstEntry("setgeu", true,   true,   true,   false,   false),
    22 -> InstEntry("setltu", true,   true,   true,   false,   false),
    23 -> InstEntry("seteq",  true,   true,   true,   false,   false),
    24 -> InstEntry("setne",  true,   true,   true,   false,   false),
    25 -> InstEntry("bnz",    false,  true,   false,  true,    false),
    26 -> InstEntry("bz",     false,  true,   false,  true,    false),
    27 -> InstEntry("j",      false,  false,  false,  true,    false),
    28 -> InstEntry("loadlo", true,   false,  false,  false,   true),
    29 -> InstEntry("loadhi", true,   false,  false,  false,   true),
  )

  // This will presumably synthesize into random logic.
  decoded.opcode := io.input.bits.instruction(6, 0)
  val hasDestLookup = VecInit((0 until 32).map { i =>
    instructionTable.getOrElse(i, InstEntry()).hasDest.B
  })

  val hasSource1Lookup = VecInit((0 until 32).map { i =>
    instructionTable.getOrElse(i, InstEntry()).hasSrc1.B
  })

  val hasSource2Lookup = VecInit((0 until 32).map { i =>
    instructionTable.getOrElse(i, InstEntry()).hasSrc2.B
  })

  val isBranchLookup = VecInit((0 until 32).map { i =>
    instructionTable.getOrElse(i, InstEntry()).isBranch.B
  })

  val isLoadConstLookup = VecInit((0 until 32).map { i =>
    instructionTable.getOrElse(i, InstEntry()).isLoadConst.B
  })

  decoded.hasDest := hasDestLookup(io.input.bits.instruction(4, 0))
  decoded.hasSource1 := hasSource1Lookup(io.input.bits.instruction(4, 0))
  decoded.hasSource2 := hasSource2Lookup(io.input.bits.instruction(4, 0))
  decoded.source2Reg := io.input.bits.instruction(27, 21)

  when (isLoadConstLookup(io.input.bits.instruction(4, 0))) {
    decoded.source1Reg := decoded.destReg
  }.otherwise {
    decoded.source1Reg := io.input.bits.instruction(20, 14)
  }

  when (isBranchLookup(io.input.bits.instruction(4, 0))) {
    decoded.immediateValue := Cat(io.input.bits.instruction(31, 20), io.input.bits.instruction(13, 7))
  }.otherwise {
    decoded.immediateValue := io.input.bits.instruction(31, 16).pad(19)
  }

  decoded.pc := io.input.bits.pc
  decoded.thread := io.input.bits.thread

  io.output.valid := true.B
  io.output.bits := RegNext(decoded)
}
