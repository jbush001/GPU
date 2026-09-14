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

/**
  * This has 3 cycles of latency
  */
class FpAddSub extends Module {
  val io = IO(new Bundle {
    val result = Output(Float32())
    val operand1 = Input(Float32())
    val operand2 = Input(Float32())
    val subtract = Input(Bool())
  })

  //
  // - Determine which operand has the larger absolute value and swap
  //   conditionally into proper lanes
  // - Compute alignment shift count, shift smaller value to align
  // - Check for special cases: inf/NaN
  //
  val stage1 = new {
    val op1IsLarger = io.operand1.absGreaterThan(io.operand2)
    val exponentDiff = Mux(op1IsLarger,
      io.operand1.exponent - io.operand2.exponent,
      io.operand2.exponent - io.operand1.exponent)

    val maxShift = (Float32.fractionWidth + 1).U
    val alignShift = Mux(exponentDiff > maxShift, maxShift, exponentDiff)

    val largerFractionNext = Mux(op1IsLarger, io.operand1.fullFraction, io.operand2.fullFraction)
    val smallerFraction = Mux(op1IsLarger, io.operand2.fullFraction, io.operand1.fullFraction)
    val smallerFractionAlignedNext = smallerFraction >> alignShift

    val logicalSubtractNext = io.operand1.negative ^ io.operand2.negative ^ io.subtract
    val isNanNext = (io.operand1.isNaN || io.operand2.isNaN
      || (io.operand1.isInf && io.operand2.isInf && logicalSubtractNext))
    val isNaN = RegNext(isNanNext, false.B)
    val isInf = RegNext(!isNanNext && (io.operand1.isInf || io.operand2.isInf), false.B)
    val logicalSubtract = RegNext(logicalSubtractNext, false.B)
    val resultExponent = RegNext(Mux(op1IsLarger, io.operand1.exponent, io.operand2.exponent), 0.U)

    // Value with larger magnitude wins
    val resultNegative = RegNext(Mux(op1IsLarger, io.operand1.negative, io.operand2.negative ^ io.subtract), false.B)
    val largerFraction = RegNext(largerFractionNext, 0.U)
    val smallerFractionAligned = RegNext(smallerFractionAlignedNext, 0.U(24.W))
  }

  val sumResultWidth = Float32.fractionWidth + 2

  //
  // - Add/subtract aligned fractions
  //
  val stage2 = new {
    val sumResult = RegNext(Mux(stage1.logicalSubtract,
      stage1.largerFraction.pad(sumResultWidth) - stage1.smallerFractionAligned.pad(sumResultWidth),
      stage1.largerFraction.pad(sumResultWidth) + stage1.smallerFractionAligned.pad(sumResultWidth)),
      0.U(sumResultWidth.W))
    val exponent = RegNext(stage1.resultExponent, 0.U)
    val resultNegative = RegNext(stage1.resultNegative, false.B)
    val isNaN = RegNext(stage1.isNaN, false.B)
    val isInf = RegNext(stage1.isInf, false.B)
  }

  //
  // - Find leading zero, shift to renormalize
  //
  val stage3 = new {
    val isZeroResult = stage2.sumResult === 0.U
    val normalizeShift = PriorityEncoder(Reverse(stage2.sumResult(sumResultWidth - 1, 0)))
    val normalizedSum = (stage2.sumResult << normalizeShift)(Float32.fractionWidth, 1)

    val resultFraction = WireInit(0.U(Float32.fractionWidth.W))
    val resultExponent = WireInit(0.U(Float32.exponentWidth.W))
    when (stage2.isInf) {
      resultFraction := 0.U
      resultExponent :=  0xff.U(Float32.exponentWidth.W)
    }.elsewhen (stage2.isNaN) {
      resultFraction := 0x400000.U
      resultExponent := 0xff.U(Float32.exponentWidth.W)
    }.elsewhen (isZeroResult) {
      resultFraction := 0.U
      resultExponent := 0.U
    }.otherwise {
      resultFraction := normalizedSum
      resultExponent := stage2.exponent + 1.U - normalizeShift
    }

    val resultNegative = stage2.resultNegative && !isZeroResult

    io.result := RegNext(Float32(resultNegative, resultExponent, resultFraction))
  }
}

object FpAddSub {
  def apply(operand1: Float32, operand2: Float32, subtract: Bool): Float32 = {
    val addSub = Module(new FpAddSub())
    addSub.io.operand1 := operand1
    addSub.io.operand2 := operand2
    addSub.io.subtract := subtract
    addSub.io.result
  }
}
