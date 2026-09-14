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
 * This has 3 cycles of latency
 */
class FpMul extends Module {
  val io = IO(new Bundle {
    val result = Output(Float32())
    val operand1 = Input(Float32())
    val operand2 = Input(Float32())
  })

  //
  // Add exponents, multiply fractions
  //
  val stage1 = new {
    val isNanNext = (io.operand1.isNaN || io.operand2.isNaN
      || (io.operand1.isInf && io.operand2.isZero)
      || (io.operand1.isZero && io.operand2.isInf))

    val mulExpSum = io.operand1.exponent.pad(10) + io.operand2.exponent.pad(10) -
      Float32.exponentBias
    val mulExponentUnderflow = mulExpSum(Float32.exponentWidth + 1)
    val mulExponentCarry = mulExpSum(Float32.exponentWidth)
    val mulExponentNext = mulExpSum(Float32.exponentWidth - 1, 0)

    val isInfNext = (io.operand1.isInf || io.operand2.isInf
      || (mulExponentCarry && !mulExponentUnderflow))
    val isZeroNext = io.operand1.isZero || io.operand2.isZero || mulExponentUnderflow

    val fractionProductNext = (io.operand1.fullFraction * io.operand2.fullFraction)(47, 23)

    val isZero = RegNext(isZeroNext, false.B)
    val isNaN = RegNext(isNanNext, false.B)
    val isInf = RegNext(isInfNext, false.B)
    val isNegative = RegNext(io.operand1.negative ^ io.operand2.negative, false.B)
    val mulExponent = RegNext(mulExponentNext, 0.U)
    val fractionProduct = RegNext(fractionProductNext, 0.U)
  }

  //
  // This stage is a passthrough. Synthesis tools like Vivado can absorb
  // registers (specifically fractionProduct in this case) into the DSP
  // multiplier blocks to take advantage of their pipelining capabilities.
  // (e.g. Vivado Design Suite User Guide UG901, chapter 4)
  //
  val stage2  = new {
    val isZero = RegNext(stage1.isZero, false.B)
    val isNaN = RegNext(stage1.isNaN, false.B)
    val isInf = RegNext(stage1.isInf, false.B)
    val isNegative = RegNext(stage1.isNegative, false.B)
    val mulExponent = RegNext(stage1.mulExponent, 0.U)
    val fractionProduct = RegNext(stage1.fractionProduct, 0.U)
  }

  val stage3 = new {
    // One position shift to normalize if the product has overflown
    val normShift = stage2.fractionProduct(24)
    val normalizedFraction = Mux(normShift,
      stage2.fractionProduct(Float32.fractionWidth, 1),
      stage2.fractionProduct(Float32.fractionWidth - 1, 0))
    val adjustedExponent = Mux(normShift, stage2.mulExponent + 1.U, stage2.mulExponent)

    val resultNext = Wire(Float32())
    when (stage2.isNaN) {
      resultNext := Float32.NaN
    }.elsewhen (stage2.isInf) {
      resultNext := Float32(stage2.isNegative, 0xff.U, 0.U)
    }.elsewhen (stage2.isZero) {
      resultNext := Float32(stage2.isNegative, 0.U, 0.U)
    }.otherwise {
      resultNext := Float32(stage2.isNegative, adjustedExponent, normalizedFraction)
    }

    io.result := RegNext(resultNext)
  }
}

object FpMul {
  def apply(operand1: Float32, operand2: Float32): Float32 = {
    val multiplier = Module(new FpMul())
    multiplier.io.operand1 := operand1
    multiplier.io.operand2 := operand2
    multiplier.io.result
  }
}
