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
    val divisor = Input(Float32())
    val reciprocal = Output(Float32())
  })

  val stage1 = new {
    val estimate = RegNext(Cat(1.U, ReciprocalLut(io.divisor.fraction(22, 17)))) // 7 bits
    val sign = RegNext(io.divisor.isNegative)
    val exponent = RegNext(253.U - io.divisor.exponent)
    val divisor = RegNext(io.divisor.fullFraction) // 24 bits
    val resultIsInf = RegNext(io.divisor.isZero)
    val resultIsNaN = RegNext(io.divisor.isNaN)
    val resultIsZero = RegNext(io.divisor.isInf)
  }

  val stage2 = new {
    val sign = RegNext(stage1.sign)
    val exponent = RegNext(stage1.exponent)
    val divisor = RegNext(stage1.divisor)
    val estimate = RegNext(stage1.estimate) // 7 bits

    // Note: we do a reduced precision multiply here to save gates.
    val product = (stage1.estimate * stage1.divisor) // 31 bits
    val productRounded = product(30, 7) + product(6)

    // This is equivalent to 2 - product
    val error = RegNext((~productRounded + 1.U))

    val resultIsInf = RegNext(stage1.resultIsInf)
    val resultIsNaN = RegNext(stage1.resultIsNaN)
    val resultIsZero = RegNext(stage1.resultIsZero)
  }

  val stage3 = new {
    val sign = RegNext(stage2.sign)
    val exponent = RegNext(stage2.exponent)
    val divisor = RegNext(stage2.divisor)

    val product = stage2.estimate * stage2.error // 7 + 24 = 31 bits
    val productRounded = product(30, 7) + product(6)
    val estimate = RegNext(productRounded) // 24 bits
    val resultIsInf = RegNext(stage2.resultIsInf)
    val resultIsNaN = RegNext(stage2.resultIsNaN)
    val resultIsZero = RegNext(stage2.resultIsZero)
  }

  val stage4 = new {
    val sign = RegNext(stage3.sign)
    val exponent = RegNext(stage3.exponent)
    val estimate = RegNext(stage3.estimate)
    val product = stage3.estimate * stage3.divisor // 24 + 24 = 48 bits
    val productRounded = (product(46, 23) + product(22))(23, 0)

    val error = RegNext((~productRounded + 1.U))
    val resultIsInf = RegNext(stage3.resultIsInf)
    val resultIsNaN = RegNext(stage3.resultIsNaN)
    val resultIsZero = RegNext(stage3.resultIsZero)
  }

  val stage5 = new {
    val sign = RegNext(stage4.sign)
    val exponent = RegNext(stage4.exponent)
    val product = stage4.estimate * stage4.error // 24 + 24 = 48 bits
    val productRounded = (product(46, 23) + product(22))(23, 0)
    val estimate = RegNext(productRounded)
    val resultIsInf = RegNext(stage4.resultIsInf)
    val resultIsNaN = RegNext(stage4.resultIsNaN)
    val resultIsZero = RegNext(stage4.resultIsZero)
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
