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
class FpAdd extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
    val result = Output(Float32())
    val addend1 = Input(Float32())
    val addend2 = Input(Float32())
  })

  val align = Module(new FpAddAlign)
  val sum = Module(new FpAddSum)
  val normalize = Module(new FpAddNormalize)

  align.io.en := io.en
  align.io.addend1 := io.addend1
  align.io.addend2 := io.addend2

  sum.io.en := io.en
  sum.io.logicalSubtract := align.io.logicalSubtract
  sum.io.largerFraction := align.io.largerFraction
  sum.io.smallerFractionAligned := align.io.smallerFractionAligned
  sum.io.resultExponent1 := align.io.resultExponent1
  sum.io.resultNegative1 := align.io.resultNegative1
  sum.io.isNaN1 := align.io.isNaN1
  sum.io.isInf1 := align.io.isInf1

  normalize.io.en := io.en
  normalize.io.sumResult := sum.io.sumResult
  normalize.io.resultExponent2 := sum.io.resultExponent2
  normalize.io.resultNegative2 := sum.io.resultNegative2
  normalize.io.isNaN2 := sum.io.isNaN2
  normalize.io.isInf2 := sum.io.isInf2

  io.result := normalize.io.result
}

/**
  * - Compare magnitudes and route larger and smaller absolute values to
  *   appropriate lanes.
  * - Compute alignment shift count, shift smaller value to align decimal
  *   points.
  * - Check for special cases: Inf/NaN
  */
class FpAddAlign extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
    val addend1 = Input(Float32())
    val addend2 = Input(Float32())

    val isNaN1 = Output(Bool())
    val isInf1 = Output(Bool())
    val logicalSubtract = Output(Bool())
    val resultExponent1 = Output(UInt(Float32.exponentWidth.W))
    val resultNegative1 = Output(Bool())
    val largerFraction = Output(UInt((Float32.fractionWidth + 1).W))
    val smallerFractionAligned = Output(UInt((Float32.fractionWidth + 1).W))
  })

  val op1IsLarger = io.addend1.absGreaterThan(io.addend2)
  val exponentDiff = Mux(op1IsLarger,
    io.addend1.exponent - io.addend2.exponent,
    io.addend2.exponent - io.addend1.exponent)

  val maxShift = (Float32.fractionWidth + 1).U
  val alignShift = Mux(exponentDiff > maxShift, maxShift, exponentDiff)

  val largerFractionNext = Mux(op1IsLarger, io.addend1.fullFraction, io.addend2.fullFraction)
  val smallerFraction = Mux(op1IsLarger, io.addend2.fullFraction, io.addend1.fullFraction)
  val smallerFractionAlignedNext = smallerFraction >> alignShift

  val logicalSubtractNext = io.addend1.isNegative ^ io.addend2.isNegative
  val isNanNext = (io.addend1.isNaN || io.addend2.isNaN
    || (io.addend1.isInf && io.addend2.isInf && logicalSubtractNext))
  io.isNaN1 := RegEnable(isNanNext, false.B, io.en)
  io.isInf1 := RegEnable(!isNanNext && (io.addend1.isInf || io.addend2.isInf), false.B, io.en)
  io.logicalSubtract := RegEnable(logicalSubtractNext, false.B, io.en)
  io.resultExponent1 := RegEnable(Mux(op1IsLarger, io.addend1.exponent, io.addend2.exponent), 0.U, io.en)

  // Value with larger magnitude wins
  io.resultNegative1 := RegEnable(Mux(op1IsLarger, io.addend1.isNegative, io.addend2.isNegative), false.B, io.en)
  io.largerFraction := RegEnable(largerFractionNext, 0.U, io.en)
  io.smallerFractionAligned := RegEnable(smallerFractionAlignedNext, 0.U(24.W), io.en)
}

/**
  * Add/subtract aligned fractions
  */
class FpAddSum extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
    val logicalSubtract = Input(Bool())
    val largerFraction = Input(UInt((Float32.fractionWidth + 1).W))
    val smallerFractionAligned = Input(UInt((Float32.fractionWidth + 1).W))
    val resultExponent1 = Input(UInt(Float32.exponentWidth.W))
    val resultNegative1 = Input(Bool())
    val isNaN1 = Input(Bool())
    val isInf1 = Input(Bool())

    val sumResult = Output(UInt((Float32.fractionWidth + 2).W))
    val resultExponent2 = Output(UInt(Float32.exponentWidth.W))
    val resultNegative2 = Output(Bool())
    val isNaN2 = Output(Bool())
    val isInf2 = Output(Bool())
  })

  val sumResultWidth = Float32.fractionWidth + 2

  io.sumResult := RegEnable(Mux(io.logicalSubtract,
    io.largerFraction.pad(sumResultWidth) - io.smallerFractionAligned.pad(sumResultWidth),
    io.largerFraction.pad(sumResultWidth) + io.smallerFractionAligned.pad(sumResultWidth)),
    0.U(sumResultWidth.W), io.en)
  io.resultExponent2 := RegEnable(io.resultExponent1, 0.U, io.en)
  io.resultNegative2 := RegEnable(io.resultNegative1, false.B, io.en)
  io.isNaN2 := RegEnable(io.isNaN1, false.B, io.en)
  io.isInf2 := RegEnable(io.isInf1, false.B, io.en)
}

/**
  * Find leading zero, shift to renormalize
  */
class FpAddNormalize extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
    val sumResult = Input(UInt((Float32.fractionWidth + 2).W))
    val resultExponent2 = Input(UInt(Float32.exponentWidth.W))
    val resultNegative2 = Input(Bool())
    val isNaN2 = Input(Bool())
    val isInf2 = Input(Bool())

    val result = Output(Float32())
  })

  val sumResultWidth = Float32.fractionWidth + 2

  val isZeroResult = io.sumResult === 0.U
  val normalizeShift = PriorityEncoder(Reverse(io.sumResult(sumResultWidth - 1, 0)))
  val normalizedSum = (io.sumResult << normalizeShift)(Float32.fractionWidth, 1)
  val normalizedExponent = io.resultExponent2 + 1.U - normalizeShift

  val resultFraction = WireInit(0.U(Float32.fractionWidth.W))
  val resultExponent = WireInit(0.U(Float32.exponentWidth.W))
  when (io.isInf2) {
    resultFraction := 0.U
    resultExponent :=  0xff.U(Float32.exponentWidth.W)
  }.elsewhen (io.isNaN2) {
    resultFraction := 0x400000.U
    resultExponent := 0xff.U(Float32.exponentWidth.W)
  }.elsewhen (isZeroResult) {
    resultFraction := 0.U
    resultExponent := 0.U
  }.elsewhen (normalizedExponent >= 0xff.U) {
    resultFraction := 0.U
    resultExponent := 0xff.U
  }.otherwise {
    resultFraction := normalizedSum
    resultExponent := normalizedExponent
  }

  val resultNegative = io.resultNegative2 && !isZeroResult

  io.result := RegEnable(Float32(resultNegative, resultExponent, resultFraction), io.en)
}

object FpAdd {
  val latency = 3

  def apply(addend1: Float32, addend2: Float32, en: Bool = true.B): Float32 = {
    val add = Module(new FpAdd())
    add.io.addend1 := addend1
    add.io.addend2 := addend2
    add.io.en := en
    add.io.result
  }
}
