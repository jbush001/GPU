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
 * Module to compute the reciprocal of a 32-bit floating point number.
 * Uses an initial reciprocal estimate x_0 of the denominator d and
 * refines it with two Newton-Raphson iterations of the form:
 *
 *   x_{n+1} = x_n * (2 - d * x_n)
 *
 * [[https://en.wikipedia.org/wiki/Division_algorithm#Newton%E2%80%93Raphson_division]]
 *
 * This has 5 cycles of latency.
 */
class FpReciprocal extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
    val divisor = Input(Float32())
    val reciprocal = Output(Float32())
  })

  val stage1 = new {
    val estimate = RegEnable(Cat(1.U, ReciprocalLut(io.divisor.fraction(22, 17))), 0.U, io.en) // 7 bits
    val sign = RegEnable(io.divisor.isNegative, false.B, io.en)
    val exponent = RegEnable(253.U - io.divisor.exponent, 0.U, io.en)
    val divisor = RegEnable(io.divisor.fullFraction, 0.U, io.en) // 24 bits
    val resultIsInf = RegEnable(io.divisor.isZero, false.B, io.en)
    val resultIsNaN = RegEnable(io.divisor.isNaN, false.B, io.en)
    val resultIsZero = RegEnable(io.divisor.isInf, false.B, io.en)
  }

  val stage2 = new {
    val sign = RegEnable(stage1.sign, false.B, io.en)
    val exponent = RegEnable(stage1.exponent, 0.U, io.en)
    val divisor = RegEnable(stage1.divisor, 0.U, io.en)
    val estimate = RegEnable(stage1.estimate, 0.U, io.en) // 7 bits

    // Note: we do a reduced precision multiply here to save gates.
    val product = (stage1.estimate * stage1.divisor) // 31 bits
    val productRounded = product(30, 7) + product(6)

    // This is equivalent to 2 - product
    val error = RegEnable((~productRounded + 1.U), 0.U, io.en)

    val resultIsInf = RegEnable(stage1.resultIsInf, false.B, io.en)
    val resultIsNaN = RegEnable(stage1.resultIsNaN, false.B, io.en)
    val resultIsZero = RegEnable(stage1.resultIsZero, false.B, io.en)
  }

  val stage3 = new {
    val sign = RegEnable(stage2.sign, false.B, io.en)
    val exponent = RegEnable(stage2.exponent, 0.U, io.en)
    val divisor = RegEnable(stage2.divisor, 0.U, io.en)

    val product = stage2.estimate * stage2.error // 7 + 24 = 31 bits
    val productRounded = product(30, 7) + product(6)
    val estimate = RegEnable(productRounded, 0.U, io.en) // 24 bits
    val resultIsInf = RegEnable(stage2.resultIsInf, false.B, io.en)
    val resultIsNaN = RegEnable(stage2.resultIsNaN, false.B, io.en)
    val resultIsZero = RegEnable(stage2.resultIsZero, false.B, io.en)
  }

  val stage4 = new {
    val sign = RegEnable(stage3.sign, false.B, io.en)
    val exponent = RegEnable(stage3.exponent, 0.U, io.en)
    val estimate = RegEnable(stage3.estimate, 0.U, io.en)
    val product = stage3.estimate * stage3.divisor // 24 + 24 = 48 bits
    val productRounded = (product(46, 23) + product(22))(23, 0)

    val error = RegEnable((~productRounded + 1.U), 0.U, io.en)
    val resultIsInf = RegEnable(stage3.resultIsInf, false.B, io.en)
    val resultIsNaN = RegEnable(stage3.resultIsNaN, false.B, io.en)
    val resultIsZero = RegEnable(stage3.resultIsZero, false.B, io.en)
  }

  val stage5 = new {
    val sign = RegEnable(stage4.sign, false.B, io.en)
    val exponent = RegEnable(stage4.exponent, 0.U, io.en)
    val product = stage4.estimate * stage4.error // 24 + 24 = 48 bits
    val productRounded = (product(46, 23) + product(22))(23, 0)
    val estimate = RegEnable(productRounded, 0.U, io.en)
    val resultIsInf = RegEnable(stage4.resultIsInf, false.B, io.en)
    val resultIsNaN = RegEnable(stage4.resultIsNaN, false.B, io.en)
    val resultIsZero = RegEnable(stage4.resultIsZero, false.B, io.en)
  }

  // Special case: when the estimate is exactly 1.0, need to adjust
  // the exponent and fraction.
  val isOne = stage5.estimate(23)
  val finalExponent = Mux(isOne, stage5.exponent + 1.U, stage5.exponent)
  val finalFraction = Mux(isOne, stage5.estimate(22, 0),
    Cat(stage5.estimate(21, 0), 0.U))

  when (stage5.resultIsNaN) {
    io.reciprocal := Float32.NaN
  }.elsewhen (stage5.resultIsInf) {
    io.reciprocal := Float32(stage5.sign, 0xff.U, 0.U)
  }.elsewhen (stage5.resultIsZero) {
    io.reciprocal := Float32(stage5.sign, 0.U, 0.U)
  }.otherwise {
    io.reciprocal := Float32(stage5.sign, finalExponent, finalFraction)
  }
}

object FpReciprocal {
  val latency = 5

  def apply(divisor: Float32, en: Bool = true.B): Float32 = {
    val recip = Module(new FpReciprocal())
    recip.io.divisor := divisor
    recip.io.en := en
    recip.io.reciprocal
  }
}
