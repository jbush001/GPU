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

object OpCode extends ChiselEnum {
  val Halt = Value(0.U)
  val And = Value(1.U)
  val Or = Value(2.U)
  val Xor = Value(3.U)
  val Addi = Value(4.U)
  val Subi = Value(5.U)
  val Muli = Value(6.U)
  val Mulih = Value(7.U)
  val Lsl = Value(8.U)
  val Asr = Value(9.U)
  val Lsr = Value(10.U)
  val Addf = Value(11.U)
  val Subf = Value(12.U)
  val Mulf = Value(13.U)
  val Recip = Value(14.U)
  val Ftoi = Value(15.U)
  val Itof = Value(16.U)
  val Setgtf = Value(17.U)
  val Setgei = Value(19.U)
  val Setlti = Value(20.U)
  val Setgeu = Value(21.U)
  val Setltu = Value(22.U)
  val Seteq = Value(23.U)
  val Setne = Value(24.U)
  val Bnz = Value(25.U)
  val Bz = Value(26.U)
  val Jump = Value(27.U)
  val LoadLo = Value(28.U)
  val LoadHi = Value(29.U)
  val Fmin = Value(30.U)
  val Fmax = Value(31.U)
  val Fabs = Value(32.U)
  val Mulihu = Value(33.U)

  // Force width to 7 bits to match instruction encoding.
  @nowarn("msg=never used")
  private val _reserveWidth = Value(127.U)
}

class InstructionMetadata(implicit cfg: GpuConfig) extends Bundle {
  val pc = UInt(cfg.busAddressBits.W)
  val thread = UInt(log2Up(cfg.shaderThreads).W)
  val opcode = OpCode()
  val hasWriteback = Bool()
  val destReg = UInt(7.W)
  val immediateValue = UInt(19.W)
}

class WritebackRequest(implicit cfg: GpuConfig) extends Bundle {
  val thread = UInt(log2Up(cfg.shaderThreads).W)
  val destReg = UInt(7.W)
  val value = Vec(cfg.shaderVectorLanes, UInt(32.W))
}

/**
  * Instruction decode stage. This stage decodes the instruction and reads the
  * source registers. It also handles writing back to registers.
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
    val startParams = Input(new ShaderParams)

    // A bit of a debug hack for now
    val result = Valid(Vec(cfg.shaderVectorLanes, UInt(32.W)))
  })

  val numRegisters = 32

  val scalarRegisters = SyncReadMem(cfg.shaderThreads * numRegisters,
    UInt(32.W), SyncReadMem.Undefined)
  val vectorRegisters = SyncReadMem(cfg.shaderThreads * numRegisters,
    Vec(cfg.shaderVectorLanes, UInt(32.W)), SyncReadMem.Undefined)
  val execMask = RegInit(VecInit(Seq.fill(cfg.shaderThreads)(~0.U(cfg.shaderVectorLanes.W))))
  val params = RegInit(VecInit(Seq.fill(cfg.shaderThreads)(0.U.asTypeOf(new ShaderParams))))

  when (io.resetThread.valid) {
    execMask(io.resetThread.bits) := ~0.U(cfg.shaderVectorLanes.W)
    params(io.resetThread.bits) := io.startParams
  }

  when (io.resetThread.valid) {
    execMask(io.resetThread.bits) := ~0.U(cfg.shaderVectorLanes.W)
    params(io.resetThread.bits) := io.startParams
  }

  def isLoadConst(inst: UInt): Bool = {
    val opcode = inst(6, 0)

    opcode === 28.U || // loadlo
    opcode === 29.U    // loadhi
  }

  val decodedMetadata = Wire(new InstructionMetadata)

  // @todo the second parameter indicates if this maps cleanly to a known opcode.
  val (decodedOpcode, _) = OpCode.safe(io.fetchedInstruction.bits.instruction(6, 0))
  decodedMetadata.opcode := decodedOpcode
  decodedMetadata.pc := io.fetchedInstruction.bits.pc
  decodedMetadata.thread := io.fetchedInstruction.bits.thread
  decodedMetadata.destReg := io.fetchedInstruction.bits.instruction(13, 7)

  decodedMetadata.hasWriteback := MuxLookup(decodedOpcode, false.B)(
    Seq(
      OpCode.And -> true.B,
      OpCode.Or -> true.B,
      OpCode.Xor -> true.B,
      OpCode.Addi -> true.B,
      OpCode.Subi -> true.B,
      OpCode.Muli -> true.B,
      OpCode.Mulih -> true.B,
      OpCode.Lsl -> true.B,
      OpCode.Asr -> true.B,
      OpCode.Lsr -> true.B,
      OpCode.Addf -> true.B,
      OpCode.Subf -> true.B,
      OpCode.Mulf -> true.B,
      OpCode.Recip -> true.B,
      OpCode.Ftoi -> true.B,
      OpCode.Itof -> true.B,
      OpCode.Setgtf -> true.B,
      OpCode.Setgei -> true.B,
      OpCode.Setlti -> true.B,
      OpCode.Setgeu -> true.B,
      OpCode.Setltu -> true.B,
      OpCode.Seteq -> true.B,
      OpCode.Setne -> true.B,
      OpCode.LoadLo -> true.B,
      OpCode.LoadHi -> true.B,
      OpCode.Fmin -> true.B,
      OpCode.Fmax -> true.B,
      OpCode.Fabs -> true.B
    )
  )

  when (decodedOpcode === OpCode.Bnz || decodedOpcode === OpCode.Bz || decodedOpcode === OpCode.Jump) {
      decodedMetadata.immediateValue := Cat(io.fetchedInstruction.bits.instruction(31, 20),
        io.fetchedInstruction.bits.instruction(13, 7))
  }.otherwise {
    decodedMetadata.immediateValue := io.fetchedInstruction.bits.instruction(31, 16).pad(19)
  }

  object SpecialReg {
    val ExecMask        = 32.U
    val LpmReadAddr     = 33.U
    val LpmWriteAddr    = 34.U
    val UniformAddr     = 35.U
    val UniformVal      = 36.U

    val Const0          = 53.U
    val Const1          = 54.U
    val ConstNeg1       = 55.U
    val Const2          = 56.U
    val Const4          = 57.U
    val Const0_5f       = 58.U
    val ConstNeg0_5f    = 59.U
    val Const1_0f       = 60.U
    val ConstNeg1_0f    = 61.U
    val Const2_0f       = 62.U
    val ConstNeg2_0f    = 63.U

    val Param0          = 96.U
    val Param1          = 97.U

    val StorePixelRed   = 105.U
    val StorePixelGreen = 106.U
    val StorePixelBlue  = 107.U
    val StorePixelAlpha = 108.U
    val LpmReadValue    = 109.U
    val LpmWriteValue   = 110.U
    val LaneId          = 111.U
  }

  def constFloat(f: Float): UInt = {
    val bits = java.lang.Float.floatToIntBits(f)
    (bits.toLong & 0xFFFFFFFFL).U(32.W)
  }

  def broadcast(v: UInt): Vec[UInt] = VecInit(Seq.fill(cfg.shaderVectorLanes)(v))

  val operand1Reg = Mux(isLoadConst(io.fetchedInstruction.bits.instruction),
    io.fetchedInstruction.bits.instruction(13, 7), // Dest reg is first operand for load const.
    io.fetchedInstruction.bits.instruction(20, 14)
  )

  val operand2Reg = io.fetchedInstruction.bits.instruction(27, 21)

  val scalarRead1 = scalarRegisters.read(Cat(io.fetchedInstruction.bits.thread, operand1Reg(4, 0)))
  val scalarRead2 = scalarRegisters.read(Cat(io.fetchedInstruction.bits.thread, operand2Reg(4, 0)))
  val vectorRead1 = vectorRegisters.read(Cat(io.fetchedInstruction.bits.thread, operand1Reg(4, 0)))
  val vectorRead2 = vectorRegisters.read(Cat(io.fetchedInstruction.bits.thread, operand2Reg(4, 0)))

  // Should not write and read the same register at the same time
  // (fetch select prevents this)
  assert(!(io.writeback.valid && io.fetchedInstruction.valid
    && (io.writeback.bits.destReg === operand1Reg
    || io.writeback.bits.destReg === operand2Reg)
    && io.writeback.bits.thread === io.fetchedInstruction.bits.thread),
    "Cannot read and write the same register at the same time")

  val operand1RegCycle2 = RegNext(operand1Reg)
  val operand2RegCycle2 = RegNext(operand2Reg)
  val validCycle2 = RegNext(io.fetchedInstruction.valid, init = false.B)
  val threadStage2 = RegNext(io.fetchedInstruction.bits.thread)

  def resolveOperand(regId: UInt, scalarData: UInt, vectorData: Vec[UInt]): Vec[UInt] = {
    val result = Wire(Vec(cfg.shaderVectorLanes, UInt(32.W)))

    when (regId(6, 5) === 0.U) {
      // 0-31: scalar general purpose registers
      result := broadcast(scalarData)
    }.elsewhen (regId(6, 5) === 2.U) {
      // 64-95: vector general purpose registers
      result := vectorData
    } .otherwise {
      result := DontCare  // Default
      switch (regId) {
        // Special registers
        is(SpecialReg.ExecMask)     { result := broadcast(execMask(threadStage2)) }
        is(SpecialReg.LaneId)       { result := VecInit((0 until cfg.shaderVectorLanes).map(_.U(32.W))) }
        is(SpecialReg.Param0)       { result := params(threadStage2).params(0) }
        is(SpecialReg.Param1)       { result := params(threadStage2).params(1) }
        is(SpecialReg.Const0)       { result := broadcast(0.U(32.W)) }
        is(SpecialReg.Const1)       { result := broadcast(1.U(32.W)) }
        is(SpecialReg.ConstNeg1)    { result := broadcast((-1).S(32.W).asUInt) }
        is(SpecialReg.Const2)       { result := broadcast(2.U(32.W)) }
        is(SpecialReg.Const4)       { result := broadcast(4.U(32.W)) }
        is(SpecialReg.Const0_5f)    { result := broadcast(constFloat(0.5f)) }
        is(SpecialReg.ConstNeg0_5f) { result := broadcast(constFloat(-0.5f)) }
        is(SpecialReg.Const1_0f)    { result := broadcast(constFloat(1.0f)) }
        is(SpecialReg.ConstNeg1_0f) { result := broadcast(constFloat(-1.0f)) }
        is(SpecialReg.Const2_0f)    { result := broadcast(constFloat(2.0f)) }
        is(SpecialReg.ConstNeg2_0f) { result := broadcast(constFloat(-2.0f)) }
      }
    }

    result
  }

  io.decodedInstruction.bits.operand1 := resolveOperand(operand1RegCycle2, scalarRead1, vectorRead1)
  io.decodedInstruction.bits.operand2 := resolveOperand(operand2RegCycle2, scalarRead2, vectorRead2)

  io.result.valid := false.B
  io.result.bits := DontCare
  when (io.writeback.valid) {
    val destReg = io.writeback.bits.destReg
    val gprIndex = Cat(io.writeback.bits.thread, destReg(4, 0))

    if (cfg.traceEnable) {
      when (destReg < 64.U) {
        printf(cf"r$destReg%0d[th${io.writeback.bits.thread}%0d] = ${io.writeback.bits.value(0)}%0x\n")
      }.otherwise {
        printf(cf"v${destReg - 64.U}%0d[th${io.writeback.bits.thread}%0d](${execMask(io.writeback.bits.thread)}%b) = ${io.writeback.bits.value}\n")
      }
    }

    when (destReg(6, 5) === 0.U) {
      // 0-31: scalar general purpose registers
      scalarRegisters.write(gprIndex, io.writeback.bits.value(0))
    }.elsewhen (destReg(6, 5) === 2.U) {
      // 64-95: vector general purpose registers
      vectorRegisters.write(gprIndex,
        io.writeback.bits.value, execMask(io.writeback.bits.thread).asBools)
    } .otherwise {
      switch (destReg) {
        is(SpecialReg.ExecMask) {
          execMask(io.writeback.bits.thread) := io.writeback.bits.value(0)(cfg.shaderVectorLanes - 1, 0)
        }

        is(SpecialReg.StorePixelRed) {
          io.result.bits := io.writeback.bits.value
          io.result.valid := true.B
        }
        is(SpecialReg.StorePixelGreen) { /* TODO... */ }
        is(SpecialReg.StorePixelBlue)  { /* TODO... */ }
        is(SpecialReg.StorePixelAlpha) { /* TODO... */ }
        is(SpecialReg.LpmWriteValue)   { /* TODO... */ }
      }
    }
  }

  io.decodedInstruction.valid := validCycle2
  io.decodedInstruction.bits.meta := RegNext(decodedMetadata)
}
