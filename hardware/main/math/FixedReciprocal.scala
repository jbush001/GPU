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
 * Fixed-point reciprocal module.
 * Computes a 0.24 reciprocal of an unsigned integer input.
 * @note This is not pipelined: 5 cycles of latency, accepts a new operation
 *  every 5 cycles.
 * @note The result is undefined for an input of zero.
 */
class FixedReciprocal(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val divisor = Flipped(Valid(UInt(cfg.edgeFunctionBits.W)))
    val result = Output(UInt(48.W))
    val done   = Output(Bool())
  })

  require(cfg.edgeFunctionBits >= 33)
  val fractionalBits = 40

  val cycleReg = RegInit(0.U(3.W))

  // Shared Registers
  val shift = Reg(UInt(log2Ceil(cfg.edgeFunctionBits).W))
  val divisor = Reg(UInt(33.W))
  val estimate = Reg(UInt((fractionalBits + 1).W))
  val error = Reg(UInt((fractionalBits + 1).W))

  // Normalization/Initial estimate
  val leadingZeros = PriorityEncoder(Reverse(io.divisor.bits))
  val normalizedDivisor = (io.divisor.bits << leadingZeros)(cfg.edgeFunctionBits - 1, cfg.edgeFunctionBits - 33)

  // Client must ensure it only enqueues a new request every 5 cycles.
  assert(!io.divisor.valid || cycleReg === 0.U)

  when(io.divisor.valid) {
    shift := leadingZeros
    divisor := normalizedDivisor
    estimate := ("b01".U ##
      FixedReciprocalLut(normalizedDivisor(31, 24)) ##
      0.U((fractionalBits - 9).W)
    )

    cycleReg := 1.U
  }

  // Refinement (Newton-Raphson iterations)
  val multOp1 = Mux(cycleReg === 1.U || cycleReg === 3.U, divisor, estimate)
  val multOp2 = Mux(cycleReg === 1.U || cycleReg === 3.U, estimate, error)

  val product = multOp1 * multOp2
  val productRounded = Mux(
    cycleReg === 1.U || cycleReg === 3.U,
    product(72, 32) + product(31),
    product((fractionalBits * 2), fractionalBits) + product(fractionalBits - 1)
  )
  val calcError =
    ((BigInt(1) << (fractionalBits + 1)).U - productRounded)(fractionalBits, 0)
  val calcEstimate = productRounded

  switch(cycleReg) {
    is(1.U) { // Compute Error 1 (Divisor * Estimate)
      error := calcError
      cycleReg := 2.U
    }
    is(2.U) { // Compute Estimate 2 (Estimate * Error 1)
      estimate := calcEstimate
      cycleReg := 3.U
    }
    is(3.U) { // Compute Error 2 (Divisor * Estimate 2)
      error := calcError
      cycleReg := 4.U
    }
    is(4.U) { // Compute Final Estimate (Estimate 2 * Error 2)
      estimate := calcEstimate
      cycleReg := 0.U
    }
  }

  // Output Alignment
  val adjustedEstimate = estimate - (estimate >> 24)
  val shiftOffset = fractionalBits + cfg.edgeFunctionBits - 49

  io.result := Mux(
    shift >= shiftOffset.U,
    adjustedEstimate.pad(48) << (shift - shiftOffset.U),
    adjustedEstimate >> (shiftOffset.U - shift)
  )

  io.done := (cycleReg === 4.U)
}

object FixedReciprocal {
  val latency = 5
}

object FixedReciprocalLut {
  private val entryWidth = 8
  private val numEntries = 1 << entryWidth
  private val numerator = numEntries * numEntries * 2
  private val romValues: Seq[Int] =
    ((1 << entryWidth) - 1) +:
      (1 until numEntries).map(index => (numerator / (numEntries + index)) & (numEntries - 1))

  def apply(index: UInt): UInt = {
    require(index.getWidth == entryWidth)
    VecInit(romValues.map(_.U(entryWidth.W)))(index)
  }
}
