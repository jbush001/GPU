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
import scala.annotation.nowarn

//  Instruction formats:
//
//               31             23 22           15 14            7 6           0
//              +-+---------------+---------------+---------------+-------------+
// R: Arith     | |     rs2 (8)   |     rs1 (8)   |    rd (8)     |  opcode (7) |
//              +-+---------------+-+-------------+---------------+-------------+
// B: Branch    |    offset[17:8]   |    rs1 (7)  |   offset[7:0] |  opcode (7) |
//              +-------------------+-------------+---------------+-------------+
// X: Inherent  |                    unused (25)                  |  opcode (7) |
//              +-------------------------------+-+---------------+-------------+
// K: Constant  |            value (16)         | |    rd (8)     |  opcode (7) |
//              +-------------------------------+-+---------------+-------------+

object OpCode extends ChiselEnum {
  val Halt = Value(0.U)
  val And = Value(1.U)
  val Or = Value(2.U)
  val Xor = Value(3.U)
  val Addi = Value(4.U)
  val Subi = Value(5.U)
  val Muli = Value(6.U)
  val Mulih = Value(7.U)
  val Mulihu = Value(8.U)
  val Shl = Value(9.U)
  val Shr = Value(10.U)
  val Shru = Value(11.U)
  val Addf = Value(12.U)
  val Subf = Value(13.U)
  val Mulf = Value(14.U)
  val Fmin = Value(15.U)
  val Fmax = Value(16.U)
  val Fabs = Value(17.U)
  val Recip = Value(18.U)
  val Ftoi = Value(19.U)
  val Itof = Value(20.U)
  val Setgtf = Value(32.U)
  val Setgei = Value(33.U)
  val Setlti = Value(34.U)
  val Setgeu = Value(35.U)
  val Setltu = Value(36.U)
  val Seteq = Value(37.U)
  val Setne = Value(38.U)
  val Bnz = Value(48.U)
  val Bz = Value(49.U)
  val Jump = Value(50.U)
  val LoadLo = Value(56.U)
  val LoadHi = Value(57.U)

  // Force width to 7 bits to match instruction encoding.
  @nowarn("msg=never used")
  private val _reserveWidth = Value(127.U)
}

object SpecialReg {
  val Const0          = 116
  val Const1          = 117
  val ConstNeg1       = 118
  val Const2          = 119
  val Const4          = 120
  val Const0_5f       = 121
  val ConstNeg0_5f    = 122
  val Const1_0f       = 123
  val ConstNeg1_0f    = 124
  val Const2_0f       = 125
  val ConstNeg2_0f    = 126

  val ExecMask        = 127

  val LaneId          = 223

  // There are 16 slots reserved for special register reads
  val Lambda0         = 224
  val Lambda1         = 225
  val Varying         = 226
  val TexelR          = 227
  val TexelG          = 228
  val TexelB          = 229
  val TexelA          = 230

  // 16 slots reserved for special register writes
  val OutputR         = 240
  val OutputG         = 241
  val OutputB         = 242
  val OutputA         = 243
  val TexelS          = 244
  val TexelT          = 245
}

/**
  * Decoded instruction fields
  */
class InstructionMetadata(implicit cfg: GpuConfig) extends Bundle {
  val pc = UInt(cfg.busAddressBits.W)
  val thread = UInt(log2Up(cfg.shaderThreads).W)
  val jobId = UInt(cfg.shaderJobIdBits.W)
  val opcode = OpCode()
  val hasWriteback = Bool()
  val destReg = UInt(8.W)
  val immediateValue = UInt(18.W)
}

class WritebackRequest(implicit cfg: GpuConfig) extends Bundle {
  val thread = UInt(log2Up(cfg.shaderThreads).W)
  val jobId = UInt(cfg.shaderJobIdBits.W)
  val destReg = UInt(8.W)
  val value = Vec(cfg.shaderVectorLanes, UInt(32.W))
}

/**
  * Instruction decode stage.
  * - This stage decodes the instruction and reads the source registers.
  * - Handles writing back to registers.
  * - Handles special register access
  */
class InstructionDecodeStage(implicit val cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    // From InstructionFetchStage. Input instruction and PC.
    val fetchedInstruction = Flipped(Valid(new FetchedInstruction))

    // To ExecuteStage. Output instruction metadata and operands.
    val decodedInstruction = Valid(new Bundle{
      val meta = new InstructionMetadata
      val operand1 = Vec(cfg.shaderVectorLanes, UInt(32.W))
      val operand2 = Vec(cfg.shaderVectorLanes, UInt(32.W))
    })

    // From ExecuteStage. Write results back to registers.
    val writeback = Flipped(Valid(new WritebackRequest))

    // From FetchSelectStage, data for newly started threads.
    val resetThread = Flipped(Valid(UInt(log2Up(cfg.shaderThreads).W)))

    val ioWaitThread = Valid(UInt(log2Up(cfg.shaderThreads).W))

    val regRead = Valid(new Bundle {
      val jobId = UInt(cfg.shaderJobIdBits.W)
      val addr = UInt(4.W)
    })

    val regReadData = Flipped(Valid(Vec(cfg.shaderVectorLanes, UInt(32.W))))

    val regWrite = Valid(new Bundle {
      val jobId = UInt(cfg.shaderJobIdBits.W)
      val addr = UInt(4.W)
      val data = Vec(cfg.shaderVectorLanes, UInt(32.W))
    })
  })

  val scalarRegisters = SyncReadMem(cfg.shaderThreads * cfg.numRegisters,
    UInt(32.W), SyncReadMem.Undefined)
  val vectorRegisters = SyncReadMem(cfg.shaderThreads * cfg.numRegisters,
    Vec(cfg.shaderVectorLanes, UInt(32.W)), SyncReadMem.Undefined)
  val execMask = RegInit(VecInit.fill(cfg.shaderThreads)(~0.U(cfg.shaderVectorLanes.W)))
  when (io.resetThread.valid) {
    execMask(io.resetThread.bits) := ~0.U(cfg.shaderVectorLanes.W)
  }

  def isLoadConst(opcode: OpCode.Type): Bool = {
    opcode === OpCode.LoadLo || opcode === OpCode.LoadHi
  }

  def isBranch(opcode: OpCode.Type): Bool = {
    opcode === OpCode.Bnz || opcode === OpCode.Bz || opcode === OpCode.Jump
  }

  val decodedMetadata = Wire(new InstructionMetadata)

  // @todo the second parameter indicates if this maps cleanly to a known opcode.
  val (decodedOpcode, _) = OpCode.safe(io.fetchedInstruction.bits.instruction(6, 0))
  decodedMetadata.opcode := decodedOpcode
  decodedMetadata.pc := io.fetchedInstruction.bits.pc
  decodedMetadata.thread := io.fetchedInstruction.bits.thread
  decodedMetadata.jobId := io.fetchedInstruction.bits.jobId
  decodedMetadata.destReg := io.fetchedInstruction.bits.instruction(14, 7)

  val decodeTable: Seq[(OpCode.Type, UInt)] = Seq(
    // Opcode         hasWriteback    hasReg1Op
    OpCode.And    -> Cat(true.B,         true.B),
    OpCode.Or     -> Cat(true.B,         true.B),
    OpCode.Xor    -> Cat(true.B,         true.B),
    OpCode.Addi   -> Cat(true.B,         true.B),
    OpCode.Subi   -> Cat(true.B,         true.B),
    OpCode.Muli   -> Cat(true.B,         true.B),
    OpCode.Mulih  -> Cat(true.B,         true.B),
    OpCode.Mulihu -> Cat(true.B,         true.B),
    OpCode.Shl    -> Cat(true.B,         true.B),
    OpCode.Shr    -> Cat(true.B,         true.B),
    OpCode.Shru   -> Cat(true.B,         true.B),
    OpCode.Addf   -> Cat(true.B,         true.B),
    OpCode.Subf   -> Cat(true.B,         true.B),
    OpCode.Mulf   -> Cat(true.B,         true.B),
    OpCode.Recip  -> Cat(true.B,         true.B),
    OpCode.Ftoi   -> Cat(true.B,         true.B),
    OpCode.Itof   -> Cat(true.B,         true.B),
    OpCode.Setgtf -> Cat(true.B,         true.B),
    OpCode.Setgei -> Cat(true.B,         true.B),
    OpCode.Setlti -> Cat(true.B,         true.B),
    OpCode.Setgeu -> Cat(true.B,         true.B),
    OpCode.Setltu -> Cat(true.B,         true.B),
    OpCode.Seteq  -> Cat(true.B,         true.B),
    OpCode.Setne  -> Cat(true.B,         true.B),
    OpCode.LoadLo -> Cat(true.B,         true.B),
    OpCode.LoadHi -> Cat(true.B,         true.B),
    OpCode.Fmin   -> Cat(true.B,         true.B),
    OpCode.Fmax   -> Cat(true.B,         true.B),
    OpCode.Fabs   -> Cat(true.B,         true.B),
    OpCode.Bnz    -> Cat(false.B,        true.B),
    OpCode.Bz     -> Cat(false.B,        true.B),
    OpCode.LoadHi -> Cat(true.B,         false.B),
    OpCode.LoadLo -> Cat(true.B,         false.B),
    OpCode.Fmin   -> Cat(true.B,         true.B),
    OpCode.Fmax   -> Cat(true.B,         true.B),
    OpCode.Fabs   -> Cat(true.B,         true.B)
  )

  val tableLookup = MuxLookup(decodedOpcode, Cat(false.B, false.B))(decodeTable)
  decodedMetadata.hasWriteback := tableLookup(1)
  val hasReg1Op = tableLookup(0)
  val hasReg1OpStage2 = RegNext(hasReg1Op, init = false.B)

  when (isBranch(decodedOpcode)) {
    decodedMetadata.immediateValue := Cat(io.fetchedInstruction.bits.instruction(31, 20),
      io.fetchedInstruction.bits.instruction(13, 7))
  }.otherwise {
    decodedMetadata.immediateValue := io.fetchedInstruction.bits.instruction(31, 16).pad(19)
  }

  val decodedMetadataStage2 = RegNext(decodedMetadata)

  def broadcast(v: UInt): Vec[UInt] = VecInit.fill(cfg.shaderVectorLanes)(v)

  val operand1Reg = Mux(isLoadConst(decodedOpcode),
    io.fetchedInstruction.bits.instruction(14, 7), // Dest reg is first operand for load const.
    io.fetchedInstruction.bits.instruction(22, 15)
  )

  val operand2Reg = io.fetchedInstruction.bits.instruction(30, 23)

  val scalarRead1 = scalarRegisters.read(Cat(io.fetchedInstruction.bits.thread,
    operand1Reg(cfg.gprIndexBits - 1, 0)))
  val scalarRead2 = scalarRegisters.read(Cat(io.fetchedInstruction.bits.thread,
    operand2Reg(cfg.gprIndexBits - 1, 0)))
  val vectorRead1 = vectorRegisters.read(Cat(io.fetchedInstruction.bits.thread,
    operand1Reg(cfg.gprIndexBits - 1, 0)))
  val vectorRead2 = vectorRegisters.read(Cat(io.fetchedInstruction.bits.thread,
    operand2Reg(cfg.gprIndexBits - 1, 0)))

  // Should not write and read the same register at the same time
  // (fetch select prevents this)
  assert(!(io.writeback.valid && io.fetchedInstruction.valid
    && (io.writeback.bits.destReg === operand1Reg
    || io.writeback.bits.destReg === operand2Reg)
    && io.writeback.bits.thread === io.fetchedInstruction.bits.thread),
    "Cannot read and write the same register at the same time")

  val operand1RegStage2 = RegNext(operand1Reg)
  val operand2RegStage2 = RegNext(operand2Reg)
  val validCycle2 = RegNext(io.fetchedInstruction.valid, init = false.B)

  /** Is this a register in the [[PixelShaderConductor]] */
  def isInputReg(regId: UInt): Bool = {
    regId(7, 4) === "b1110".U
  }

  /** Is this a register in the [[PixelShaderConductor]] */
  def isOutputReg(regId: UInt): Bool = {
    regId(7, 4) === "b1111".U
  }

  io.regRead.valid := isInputReg(operand1Reg) && io.fetchedInstruction.valid && hasReg1Op
  io.regRead.bits.addr := operand1Reg(cfg.gprIndexBits - 1, 0)
  io.regRead.bits.jobId := decodedMetadata.jobId


  def resolveOperand(regId: UInt, scalarData: UInt, vectorData: Vec[UInt]): Vec[UInt] = {
    val result = Wire(Vec(cfg.shaderVectorLanes, UInt(32.W)))

    when (regId(7, 6) === 0.U) {
      // 0-63: scalar general purpose registers
      result := broadcast(scalarData)
    }.elsewhen (regId(7, 6) === 2.U) {
      // 128-192: vector general purpose registers
      result := vectorData
    }.elsewhen (isInputReg(regId)) {
      // 224-230: special purpose input registers
      result := io.regReadData.bits
    } .otherwise {
      result := DontCare  // Default
      switch (regId) {
        // Special registers
        is(SpecialReg.ExecMask.U)     { result := broadcast(execMask(decodedMetadataStage2.thread)) }
        is(SpecialReg.LaneId.U)       { result := VecInit((0 until cfg.shaderVectorLanes).map(_.U(32.W))) }
        is(SpecialReg.Const0.U)       { result := broadcast(0.U(32.W)) }
        is(SpecialReg.Const1.U)       { result := broadcast(1.U(32.W)) }
        is(SpecialReg.ConstNeg1.U)    { result := broadcast((-1).S(32.W).asUInt) }
        is(SpecialReg.Const2.U)       { result := broadcast(2.U(32.W)) }
        is(SpecialReg.Const4.U)       { result := broadcast(4.U(32.W)) }
        is(SpecialReg.Const0_5f.U)    { result := broadcast(Float32(0.5).raw) }
        is(SpecialReg.ConstNeg0_5f.U) { result := broadcast(Float32(-0.5).raw) }
        is(SpecialReg.Const1_0f.U)    { result := broadcast(Float32(1.0).raw) }
        is(SpecialReg.ConstNeg1_0f.U) { result := broadcast(Float32(-1.0).raw) }
        is(SpecialReg.Const2_0f.U)    { result := broadcast(Float32(2.0).raw) }
        is(SpecialReg.ConstNeg2_0f.U) { result := broadcast(Float32(-2.0).raw) }
      }
    }

    result
  }

  io.decodedInstruction.bits.operand1 := resolveOperand(operand1RegStage2, scalarRead1, vectorRead1)
  io.decodedInstruction.bits.operand2 := resolveOperand(operand2RegStage2, scalarRead2, vectorRead2)

  io.ioWaitThread.valid := validCycle2 && !io.regReadData.valid && isInputReg(operand1RegStage2) && hasReg1OpStage2
  io.ioWaitThread.bits := io.decodedInstruction.bits.meta.thread

  io.regWrite.bits.jobId := io.writeback.bits.jobId
  io.regWrite.bits.addr := io.writeback.bits.destReg(3, 0)
  io.regWrite.bits.data := io.writeback.bits.value
  io.regWrite.valid := false.B
  when (io.writeback.valid) {
    val destReg = io.writeback.bits.destReg
    val gprIndex = Cat(io.writeback.bits.thread, destReg(cfg.gprIndexBits - 1, 0))

    if (cfg.traceEnable) {
      when (destReg < 128.U) {
        printf(cf"r$destReg%0d[th${io.writeback.bits.thread}%0d] = ${io.writeback.bits.value(0)}%0x\n")
      }.otherwise {
        printf(cf"v${destReg - 128.U}%0d[th${io.writeback.bits.thread}%0d](${execMask(io.writeback.bits.thread)}%b) = ${io.writeback.bits.value}\n")
      }
    }

    when (destReg(7, 6) === 0.U) {
      // 0-128: scalar general purpose registers
      scalarRegisters.write(gprIndex, io.writeback.bits.value(0))
    }.elsewhen (destReg(7, 6) === 2.U) {
      // 128-191: vector general purpose registers
      vectorRegisters.write(gprIndex,
        io.writeback.bits.value, execMask(io.writeback.bits.thread).asBools)
    }.elsewhen (isOutputReg(destReg)) {
      // 240-255: special purpose output registers
      io.regWrite.valid := true.B
    }.elsewhen (destReg === SpecialReg.ExecMask.U) {
      execMask(io.writeback.bits.thread) := io.writeback.bits.value(0)(cfg.shaderVectorLanes - 1, 0)
    }
  }

  io.decodedInstruction.valid := validCycle2 && !io.ioWaitThread.valid
  io.decodedInstruction.bits.meta := decodedMetadataStage2
}
