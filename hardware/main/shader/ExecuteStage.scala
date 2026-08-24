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

/**
  * Final stage in the instruction pipeline, responsible for:
  * - Performing arithmetic and logical operations.
  * - Handling branches.
  * - Writing results back to the register file.
  * - Handling halt instruction.
  */
class ExecuteStage(implicit val cfg: GpuConfig) extends Module {
  def VectorResult(): Vec[UInt] = Vec(cfg.shaderVectorLanes, UInt(32.W))

  val io = IO(new Bundle {

    // From InstructionDecodeStage. Input instruction and operands.
    val in = Flipped(Valid(new Bundle{
      val meta = new InstructionMetadata
      val operand1 = Vec(cfg.shaderVectorLanes, UInt(32.W))
      val operand2 = Vec(cfg.shaderVectorLanes, UInt(32.W))
    }))

    // To InstructionDecodeStage. Write results back to registers.
    val writeback = Valid(new WritebackRequest)

    // To multiple stages. These handle control flow changes, such as
    // branches or halts.
    val squashThread = Valid(UInt(log2Up(cfg.shaderThreads).W))
    val haltRequest = Valid(UInt(log2Up(cfg.shaderThreads).W))
    val rollback = Valid(new Bundle{
      val thread = UInt(log2Up(cfg.shaderThreads).W)
      val pc = UInt(cfg.busAddressBits.W)
    })
  })

  // Shadow instruction pipeline to align with results.
  val inst0 = Wire(Valid(new InstructionMetadata))
  inst0.valid := io.in.valid
  inst0.bits := io.in.bits.meta
  val inst1 = RegNext(inst0, init = 0.U.asTypeOf(Valid(new InstructionMetadata)))
  val inst2 = RegNext(inst1, init = 0.U.asTypeOf(Valid(new InstructionMetadata)))
  val inst3 = RegNext(inst2, init = 0.U.asTypeOf(Valid(new InstructionMetadata)))

  // Long latency pipelines (3 cycles)
  val fpAddSubResult = Wire(VectorResult())
  val fpMulResult = Wire(VectorResult())
  for (lane <- 0 until cfg.shaderVectorLanes) {
    val fpAddSub = Module(new FpAddSub)
    val fpMul = Module(new FpMul)

    fpAddSub.io.subtract := io.in.bits.meta.opcode === OpCode.Subf
    fpAddSub.io.operand1 := Float32(io.in.bits.operand1(lane))
    fpAddSub.io.operand2 := Float32(io.in.bits.operand2(lane))
    fpAddSubResult(lane) := fpAddSub.io.result.raw

    fpMul.io.operand1 := Float32(io.in.bits.operand1(lane))
    fpMul.io.operand2 := Float32(io.in.bits.operand2(lane))
    fpMulResult(lane) := fpMul.io.result.raw
  }

  // Short latency operations (1 cycle)
  def vecOp(a: Vec[UInt], b: Vec[UInt])(f: (Int, UInt, UInt) => UInt): Vec[UInt] =
    VecInit((0 until a.length).map(i => f(i, a(i), b(i))))

  // Floating point comparison. This is used for Fmin/Fmax and Setgtf
  // instructions, so hoist it explicitly.
  val fpGreater = vecOp(io.in.bits.operand1, io.in.bits.operand2)((_, a, b) => Float32(a).greaterThan(Float32(b)))

  val binOps = Seq[(OpCode.Type, (Int, UInt, UInt) => UInt)](
    OpCode.And -> ((_, a, b) => a & b),
    OpCode.Or -> ((_, a, b) => a | b),
    OpCode.Xor -> ((_, a, b) => a ^ b),
    OpCode.Addi -> ((_, a, b) => a + b),
    OpCode.Subi -> ((_, a, b) => a - b),
    OpCode.Muli -> ((_, a, b) => (a * b)(31, 0)),
    OpCode.Mulih -> ((_, a, b) => (a.asSInt * b.asSInt)(63, 32).asUInt),
    OpCode.Lsl -> ((_, a, b) => (a << b(4, 0))(31, 0)),
    OpCode.Asr -> ((_, a, b) => (a.asSInt >> b(4, 0)).asUInt),
    OpCode.Lsr -> ((_, a, b) => a >> b(4, 0)),
    OpCode.LoadLo -> ((_, a, _) => Cat(a(31, 16), io.in.bits.meta.immediateValue(15, 0))),
    OpCode.LoadHi -> ((_, a, _) => Cat(io.in.bits.meta.immediateValue(15, 0), a(15, 0))),
    OpCode.Ftoi -> ((_, a, _) => Float32(a).toSInt().asUInt),
    OpCode.Itof -> ((_, a, _) => Float32.fromSInt(a.asSInt).raw),
    OpCode.Fabs -> ((_, a, _) => Float32(a).abs().raw),
    OpCode.Fmin -> ((i, a, b) => Mux(!fpGreater(i).asBool, a, b)),
    OpCode.Fmax -> ((i, a, b) => Mux(fpGreater(i).asBool, a, b))
  )

  val singleCycleResult = MuxLookup(io.in.bits.meta.opcode, WireInit(VectorResult(), DontCare))(binOps.map {
    case (op, f) => op -> vecOp(io.in.bits.operand1, io.in.bits.operand2)(f)
  })

  val singleCycleResult1 = RegNext(singleCycleResult)
  val singleCycleResult2 = RegNext(singleCycleResult1)
  val singleCycleResult3 = RegNext(singleCycleResult2)

  // Single cycle floating point.
  // The reciprocal estimate block has one cycle of latency; the output
  // is registered.
  val recipResult1 = Wire(VectorResult())
  for (lane <- 0 until cfg.shaderVectorLanes) {
    val recipEstimate = Module(new FpReciprocalEstimate)
    recipEstimate.io.operand := Float32(io.in.bits.operand1(lane))
    recipResult1(lane) := recipEstimate.io.result.raw
  }

  val recipResult2 = RegNext(recipResult1)
  val recipResult3 = RegNext(recipResult2)

  // Comparison operations
  def vecCompare(a: Vec[UInt], b: Vec[UInt])(f: (Int, UInt, UInt) => Bool): UInt =
    Cat((0 until cfg.shaderVectorLanes).reverse.map(i => f(i, a(i), b(i))))

  val cmpOps: Seq[(OpCode.Type, (Int, UInt, UInt) => Bool)] = Seq(
    OpCode.Setgtf -> ((i, _, _) => fpGreater(i).asBool),
    OpCode.Setgei -> ((_, a, b) => a.asSInt >= b.asSInt),
    OpCode.Setlti -> ((_, a, b) => a.asSInt < b.asSInt),
    OpCode.Setgeu -> ((_, a, b) => a >= b),
    OpCode.Setltu -> ((_, a, b) => a < b),
    OpCode.Seteq  -> ((_, a, b) => a === b),
    OpCode.Setne  -> ((_, a, b) => a =/= b),
  )

  val comparisonResult = MuxLookup(io.in.bits.meta.opcode, WireInit(UInt(cfg.shaderVectorLanes.W), DontCare)) (
    cmpOps.map { case (op, f) => op -> vecCompare(io.in.bits.operand1, io.in.bits.operand2)(f) }
  )

  val comparisonResult1 = RegNext(comparisonResult)
  val comparisonResult2 = RegNext(comparisonResult1)
  val comparisonResult3 = RegNext(comparisonResult2)

  val compareAsVec = WireInit(VecInit(Seq.fill(cfg.shaderVectorLanes)(0.U(32.W))))
  compareAsVec(0) := comparisonResult3

  // Branch check
  val branchTaken0 = Wire(Bool())
  branchTaken0 := false.B
  when (io.in.valid) {
    switch (io.in.bits.meta.opcode) {
      is (OpCode.Bnz) { branchTaken0 := io.in.bits.operand1(0) =/= 0.U }
      is (OpCode.Bz) { branchTaken0 := io.in.bits.operand1(0) === 0.U }
      is (OpCode.Jump) { branchTaken0 := true.B }
    }
  }

  val branchTaken1 = RegNext(branchTaken0, init = false.B)
  val branchTaken2 = RegNext(branchTaken1, init = false.B)
  val branchTaken3 = RegNext(branchTaken2, init = false.B)

  // Final result multiplexer
  val resultTable: Seq[(OpCode.Type, Vec[UInt])] =
    Seq(
      OpCode.Addf -> fpAddSubResult,
      OpCode.Subf -> fpAddSubResult,
      OpCode.Mulf -> fpMulResult,
      OpCode.Recip -> recipResult3
    ) ++
    binOps.map { case (op, _) => op -> singleCycleResult3 } ++
    cmpOps.map { case (op, _) => op -> compareAsVec }

  val result = MuxLookup(inst3.bits.opcode, WireInit(VectorResult(), DontCare))(resultTable)

  io.writeback.valid := inst3.valid && inst3.bits.hasWriteback
  io.writeback.bits.thread := inst3.bits.thread
  io.writeback.bits.value := result
  io.writeback.bits.destReg := inst3.bits.destReg

  io.squashThread.valid := false.B
  io.squashThread.bits := inst3.bits.thread
  io.haltRequest.valid := false.B
  io.haltRequest.bits := inst3.bits.thread
  io.rollback.valid := false.B
  io.rollback.bits.thread := DontCare
  io.rollback.bits.pc := DontCare

  // Halt handling
  when (inst3.valid && inst3.bits.opcode === OpCode.Halt) {
    io.haltRequest.valid := true.B
    io.squashThread.valid := true.B
  }

  // Branch handling
  when (branchTaken3) {
    io.rollback.valid := true.B
    io.rollback.bits.thread := inst3.bits.thread
    io.rollback.bits.pc := (inst3.bits.pc.asSInt + 4.S + (inst3.bits.immediateValue.asSInt << 2)).asUInt(cfg.busAddressBits - 1, 0)
    if (cfg.traceEnable) {
      printf(cf"branch taken: thread ${inst3.bits.thread}%0d, pc = ${inst3.bits.pc}%0x, target = ${io.rollback.bits.pc}%0x\n")
    }

    io.squashThread.valid := true.B
  }
}
