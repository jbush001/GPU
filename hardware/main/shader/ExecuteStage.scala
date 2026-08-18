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

object OpCode {
  val Halt = 0
  val And = 1
  val Or = 2
  val Xor = 3
  val Addi = 4
  val Subi = 5
  val Muli = 6
  val Mulih = 7
  val Lsl = 8
  val Asr = 9
  val Lsr = 10
  val Addf = 11
  val Subf = 12
  val Mulf = 13
  val Recip = 14
  val Ftoi = 15
  val Itof = 16
  val Setgtf = 17
  val Setgei = 19
  val Setlti = 20
  val Setgeu = 21
  val Setltu = 22
  val Seteq = 23
  val Setne = 24
  val Bnz = 25
  val Bz = 26
  val Jump = 27
  val LoadLo = 28
  val LoadHi = 29
}

class ExecuteStage(implicit val cfg: GpuConfig) extends Module {
  def VectorResult(): Vec[UInt] = Vec(cfg.shaderVectorLanes, UInt(32.W))

  val io = IO(new Bundle {
    val in = Flipped(Valid(new DecodedInstruction))
    val result = Valid(VectorResult())
  })

  class Instruction extends Bundle {
    val valid = Bool()
    val opcode = UInt(7.W)
  }

  val inst0 = Wire(new Instruction)
  inst0.valid := io.in.valid
  inst0.opcode := io.in.bits.opcode
  val inst1 = RegNext(inst0, init = 0.U.asTypeOf(new Instruction))
  val inst2 = RegNext(inst1, init = 0.U.asTypeOf(new Instruction))
  val inst3 = RegNext(inst2, init = 0.U.asTypeOf(new Instruction))

  def f32(u: UInt): Float32 = u.asTypeOf(new Float32)

  // Long latency pipelines (3 cycles)
  val fpAddSubResult = Wire(VectorResult())
  val fpMulResult = Wire(VectorResult())
  for (lane <- 0 until cfg.shaderVectorLanes) {
    val fpAddSub = Module(new FpAddSub)
    val fpMul = Module(new FpMul)

    fpAddSub.io.subtract := io.in.bits.opcode === OpCode.Subf.U
    fpAddSub.io.operand1 := f32(io.in.bits.operand1(lane))
    fpAddSub.io.operand2 := f32(io.in.bits.operand2(lane))
    fpAddSubResult(lane) := fpAddSub.io.result.raw

    fpMul.io.operand1 := f32(io.in.bits.operand1(lane))
    fpMul.io.operand2 := f32(io.in.bits.operand2(lane))
    fpMulResult(lane) := fpMul.io.result.raw
  }

  // Short latency operations (1 cycle)
  def vecOp(a: Vec[UInt], b: Vec[UInt])(f: (UInt, UInt) => UInt): Vec[UInt] =
    VecInit((0 until a.length).map(i => f(a(i), b(i))))

  val binOps = Seq[(UInt, (UInt, UInt) => UInt)](
    OpCode.And.U -> ((a, b) => a & b),
    OpCode.Or.U -> ((a, b) => a | b),
    OpCode.Xor.U -> ((a, b) => a ^ b),
    OpCode.Addi.U -> ((a, b) => a + b),
    OpCode.Subi.U -> ((a, b) => a - b),
    OpCode.Muli.U -> ((a, b) => (a * b)(31, 0)),
    OpCode.Mulih.U -> ((a, b) => (a.asSInt * b.asSInt)(63, 32).asUInt),
    OpCode.Lsl.U -> ((a, b) => (a << b(4, 0))(31, 0)),
    OpCode.Asr.U -> ((a, b) => (a.asSInt >> b(4, 0)).asUInt),
    OpCode.Lsr.U -> ((a, b) => a >> b(4, 0))
  )

  val singleCycleResult = MuxLookup(io.in.bits.opcode, WireInit(VectorResult(), DontCare))(binOps.map {
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
    recipEstimate.io.operand.raw := io.in.bits.operand1(lane)
    recipResult1(lane) := recipEstimate.io.result.raw
  }

  val recipResult2 = RegNext(recipResult1)
  val recipResult3 = RegNext(recipResult2)

  // Comparison operations
  def vecCompare(a: Vec[UInt], b: Vec[UInt])(f: (UInt, UInt) => Bool): UInt =
    Cat((0 until cfg.shaderVectorLanes).reverse.map(i => f(a(i), b(i))))

  val cmpOps: Seq[(UInt, (UInt, UInt) => Bool)] = Seq(
    OpCode.Setgtf.U -> ((a, b) => f32(a).greaterThan(f32(b))),
    OpCode.Setgei.U -> ((a, b) => a.asSInt >= b.asSInt),
    OpCode.Setlti.U -> ((a, b) => a.asSInt < b.asSInt),
    OpCode.Setgeu.U -> ((a, b) => a >= b),
    OpCode.Setltu.U -> ((a, b) => a < b),
    OpCode.Seteq.U  -> ((a, b) => a === b),
    OpCode.Setne.U  -> ((a, b) => a =/= b),
  )

  val comparisonResult = MuxLookup(io.in.bits.opcode, WireInit(UInt(cfg.shaderVectorLanes.W), DontCare)) (
    cmpOps.map { case (op, f) => op -> vecCompare(io.in.bits.operand1, io.in.bits.operand2)(f) }
  )

  val comparisonResult1 = RegNext(comparisonResult)
  val comparisonResult2 = RegNext(comparisonResult1)
  val comparisonResult3 = RegNext(comparisonResult2)

  val compareAsVec = WireInit(VecInit(Seq.fill(cfg.shaderVectorLanes)(0.U(32.W))))
  compareAsVec(0) := comparisonResult3

  // result
  val resultTable: Seq[(UInt, Vec[UInt])] =
    Seq(
      OpCode.Addf.U -> fpAddSubResult,
      OpCode.Subf.U -> fpAddSubResult,
      OpCode.Mulf.U -> fpMulResult,
      OpCode.Recip.U -> recipResult3
    ) ++
    binOps.map { case (op, _) => op -> singleCycleResult3 } ++
    cmpOps.map { case (op, _) => op -> compareAsVec }

  io.result.bits := MuxLookup(inst3.opcode, WireInit(VectorResult(), DontCare))(resultTable)

  io.result.valid := inst3.valid
}
