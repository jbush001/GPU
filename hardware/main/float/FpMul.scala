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

/**
 * This has 2 cycles of latency
 */
class FpMul extends Module {
  val io = IO(new Bundle {
    val product = Output(Float32())
    val multiplier = Input(Float32())
    val multiplicand = Input(Float32())
  })

  val multiply = Module(new FpMulMultiply())
  val normalize = Module(new FpMulNormalize())

  multiply.io.multiplier := io.multiplier
  multiply.io.multiplicand := io.multiplicand

  normalize.io.fractionProduct := multiply.io.fractionProduct
  normalize.io.mulExponent := multiply.io.exponent
  normalize.io.isNaN := multiply.io.isNaN
  normalize.io.isInf := multiply.io.isInf
  normalize.io.isZero := multiply.io.isZero
  normalize.io.isNegative := multiply.io.isNegative

  io.product := normalize.io.product
}

/**
 * Add exponents, multiply fractions
 */
class FpMulMultiply extends Module {
  val io = IO(new Bundle {
    val multiplier = Input(Float32())
    val multiplicand = Input(Float32())
    val exponent = Output(UInt(Float32.exponentWidth.W))
    val fractionProduct = Output(UInt((Float32.fractionWidth + 2).W))
    val isZero = Output(Bool())
    val isNaN = Output(Bool())
    val isInf = Output(Bool())
    val isNegative = Output(Bool())
  })

  val isNanNext = (io.multiplier.isNaN || io.multiplicand.isNaN
    || (io.multiplier.isInf && io.multiplicand.isZero)
    || (io.multiplier.isZero && io.multiplicand.isInf))

  val mulExpSum = io.multiplier.exponent.pad(10) + io.multiplicand.exponent.pad(10) -
    Float32.exponentBias
  val mulExponentUnderflow = mulExpSum(Float32.exponentWidth + 1)
  val mulExponentCarry = mulExpSum(Float32.exponentWidth)
  val mulExponentNext = mulExpSum(Float32.exponentWidth - 1, 0)

  val isInfNext = (io.multiplier.isInf || io.multiplicand.isInf
    || (mulExponentCarry && !mulExponentUnderflow))
  val isZeroNext = io.multiplier.isZero || io.multiplicand.isZero || mulExponentUnderflow

  val fractionProductNext = (io.multiplier.fullFraction * io.multiplicand.fullFraction)(47, 23)

  io.isZero := RegNext(isZeroNext, false.B)
  io.isNaN := RegNext(isNanNext, false.B)
  io.isInf := RegNext(isInfNext, false.B)
  io.isNegative := RegNext(io.multiplier.negative ^ io.multiplicand.negative, false.B)
  io.exponent := RegNext(mulExponentNext, 0.U)
  io.fractionProduct := RegNext(fractionProductNext, 0.U)
}

class FpMulNormalize extends Module {
  val io = IO(new Bundle {
    val fractionProduct = Input(UInt((Float32.fractionWidth + 2).W))
    val mulExponent = Input(UInt(Float32.exponentWidth.W))
    val isNaN = Input(Bool())
    val isInf = Input(Bool())
    val isZero = Input(Bool())
    val isNegative = Input(Bool())
    val product = Output(Float32())
  })

  // One position shift to normalize if the product has overflown
  val normShift = io.fractionProduct(24)
  val normalizedFraction = Mux(normShift,
    io.fractionProduct(Float32.fractionWidth, 1),
    io.fractionProduct(Float32.fractionWidth - 1, 0))
  val adjustedExponent = Mux(normShift, io.mulExponent + 1.U, io.mulExponent)

  val productNext = Wire(Float32())
  when (io.isNaN) {
    productNext := Float32.NaN
  }.elsewhen (io.isInf) {
    productNext := Float32(io.isNegative, 0xff.U, 0.U)
  }.elsewhen (io.isZero) {
    productNext := Float32(io.isNegative, 0.U, 0.U)
  }.otherwise {
    productNext := Float32(io.isNegative, adjustedExponent, normalizedFraction)
  }

  io.product := RegNext(productNext)
}

object FpMul {
  def apply(multiplier: Float32, operand2: Float32): Float32 = {
    val mul = Module(new FpMul())
    mul.io.multiplier := multiplier
    mul.io.multiplicand := operand2
    mul.io.product
  }
}
