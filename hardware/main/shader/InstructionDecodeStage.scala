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

class WritebackRequest(implicit cfg: GpuConfig) extends Bundle {
  val thread = UInt(log2Up(cfg.shaderThreads).W)
  val regId = UInt(7.W)
  val value = Vec(cfg.shaderVectorLanes, UInt(32.W))
}

/**
  * Instruction decode stage. This stage decodes the instruction and reads the
  * source registers. It also handles writing back to registers.
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
    val resetThread = Flipped(Valid(UInt(log2Up(cfg.shaderThreads).W)))
  })

  val numRegisters = 32

  val scalarRegisters = SyncReadMem(cfg.shaderThreads * numRegisters,
    UInt(32.W))
  val vectorRegisters = SyncReadMem(cfg.shaderThreads * numRegisters,
    Vec(cfg.shaderVectorLanes, UInt(32.W)))
  val execMask = RegInit(VecInit(Seq.fill(cfg.shaderThreads)(~0.U(cfg.shaderVectorLanes.W))))

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

  def isLoadConst(inst: UInt): Bool = {
    val opcode = inst(6, 0)

    opcode === 28.U || // loadlo
    opcode === 29.U    // loadhi
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

  def readOperand(regId: UInt): Vec[UInt] = {
    val result = Wire(Vec(cfg.shaderVectorLanes, UInt(32.W)))
    val gprIndex = Cat(io.input.bits.thread, regId(4, 0))

    when (regId(6, 5) === 0.U) {
      // 0-31: scalar general purpose registers
      result := broadcast(scalarRegisters.read(gprIndex))
    } .elsewhen (regId(6, 5) === 2.U) {
      // 64-95: vector general purpose registers
      result := vectorRegisters.read(gprIndex)
    } .otherwise {
      result := DontCare  // Default
      switch (regId) {
        // Special registers
        is(SpecialReg.ExecMask)     { result := broadcast(execMask(io.input.bits.thread)) }
        is(SpecialReg.LaneId)       { result := VecInit((0 until cfg.shaderVectorLanes).map(_.U(32.W))) }
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

  val operand1Reg = Mux(isLoadConst(io.input.bits.instruction),
    io.input.bits.instruction(13, 7), // Dest reg is first operand for load const.
    io.input.bits.instruction(20, 14)
  )

  val operand2Reg = io.input.bits.instruction(27, 21)

  io.output.bits.operand1 := readOperand(operand1Reg)
  io.output.bits.operand2 := readOperand(operand2Reg)

  when (io.writeback.valid) {
    val regId = io.writeback.bits.regId
    val gprIndex = Cat(io.writeback.bits.thread, regId(4, 0))

    when (regId(6, 5) === 0.U) {
      // 0-31: scalar general purpose registers
      scalarRegisters.write(gprIndex, io.writeback.bits.value(0))
    }.elsewhen (regId(6, 5) === 2.U) {
      // 64-95: vector general purpose registers
      vectorRegisters.write(gprIndex,
        io.writeback.bits.value, execMask(io.writeback.bits.thread).asBools)
    } .otherwise {
      switch (regId) {
        is(SpecialReg.ExecMask) {
          execMask(io.writeback.bits.thread) := io.writeback.bits.value(0)(cfg.shaderVectorLanes - 1, 0)
        }

        is(SpecialReg.StorePixelRed)   { /* TODO... */ }
        is(SpecialReg.StorePixelGreen) { /* TODO... */ }
        is(SpecialReg.StorePixelBlue)  { /* TODO... */ }
        is(SpecialReg.StorePixelAlpha) { /* TODO... */ }
        is(SpecialReg.LpmWriteValue)   { /* TODO... */ }
      }
    }
  }

  when (io.resetThread.valid) {
    execMask(io.resetThread.bits) := ~0.U(cfg.shaderVectorLanes.W)
  }

  io.output.valid := RegNext(io.input.valid)
  io.output.bits.opcode := RegNext(io.input.bits.instruction(6, 0))
  io.output.bits.pc := RegNext(io.input.bits.pc)
  io.output.bits.thread := RegNext(io.input.bits.thread)
  io.output.bits.destReg := RegNext(io.input.bits.instruction(13, 7))
}
