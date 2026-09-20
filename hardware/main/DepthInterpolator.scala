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

class DepthInterpolatorCoeffs extends Bundle {
  val invW0 = Float32()
  val invW1 = Float32()
  val invW2 = Float32()
  val invdW1 = Float32()
  val invdW2 = Float32()
}

/** A [[RasterizedQuad]] augmented with per-pixel interpolated depth, produced
  * by [[DepthInterpolator]].
  */
class InterpolatedQuad(implicit cfg: GpuConfig) extends Bundle {
  val quad = new RasterizedQuad
  val depths = Vec(Consts.pixelsPerQuad, Float32())
}

/** Sits between [[Rasterizer]] and [[PixelShaderConductor]] and computes
  * per-pixel depth for each rasterized quad.
  * This has 17 cycles of latency.
  */
class DepthInterpolator(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val rasterizedQuad = Flipped(Decoupled(new RasterizedQuad))
    val interpolatedQuad = Decoupled(new InterpolatedQuad)
    val writeCoeffs = Flipped(Valid(new Bundle {
      val primitiveId = UInt(cfg.primitiveIdBits.W)
      val coeffs = new DepthInterpolatorCoeffs()
    }))
    val idle = Output(Bool())
  })

  val coeffs = RegInit(VecInit(Seq.fill(1 << cfg.primitiveIdBits)(0.U.asTypeOf(new DepthInterpolatorCoeffs()))))

  when (io.writeCoeffs.valid) {
    coeffs(io.writeCoeffs.bits.primitiveId) := io.writeCoeffs.bits.coeffs
  }

  val stall = io.interpolatedQuad.valid && !io.interpolatedQuad.ready

  val totalLatency = 17
  io.interpolatedQuad.bits.quad := ShiftRegister(io.rasterizedQuad.bits, totalLatency, !stall)

  val coeff0 = coeffs(io.rasterizedQuad.bits.primitiveId)
  val coeff5 = ShiftRegister(coeff0, 5, !stall)
  val coeff7 = ShiftRegister(coeff5, 2, !stall)
  val coeff15 = ShiftRegister(coeff7, 8, !stall)

  for (pixel <- 0 until Consts.pixelsPerQuad) {
    val lambda = io.rasterizedQuad.bits.lambda(pixel)
    val a = FpMul(coeff0.invdW1, lambda(0), !stall) // cycle 0
    val b = FpMul(coeff0.invdW2, lambda(1), !stall)
    val sumC = FpAdd(a, b, !stall) // cycle 2
    val sumD = FpAdd(sumC, coeff5.invW0, !stall) // cycle 5
    val w = FpReciprocal(sumD, !stall) // cycle 8

    val lambda0_13 = ShiftRegister(lambda(0), 13, !stall)
    val lambda1_13 = ShiftRegister(lambda(1), 13, !stall)
    val e0 = FpMul(w, lambda0_13, !stall) // cycle 13
    val e1 = FpMul(w, lambda1_13, !stall)

    // note: we replace the lambda values here with perspective corrected ones.
    io.interpolatedQuad.bits.quad.lambda(pixel)(0) := FpMul(e0, coeff15.invW1, !stall) // Cycle 15
    io.interpolatedQuad.bits.quad.lambda(pixel)(1) := FpMul(e1, coeff15.invW2, !stall)
    io.interpolatedQuad.bits.depths(pixel) := ShiftRegister(w, 4, !stall)
  }

  val validStages = RegInit(0.U(totalLatency.W))
  when (!stall) {
    validStages := Cat(validStages(totalLatency - 2, 0), io.rasterizedQuad.valid)
  }

  io.interpolatedQuad.valid := validStages(totalLatency - 1)
  io.rasterizedQuad.ready := !stall
  io.idle := validStages === 0.U
}
