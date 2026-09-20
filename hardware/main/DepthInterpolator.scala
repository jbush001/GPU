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
  * This has 13 cycles of latency.
  * @todo This does not yet create perspective corrected barycentric
  * coordinates. Those will be derived from the depth.
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

  for ((lambda, depth) <- io.rasterizedQuad.bits.lambda.zip(io.interpolatedQuad.bits.depths)) {
    // Compute W at pixel
    val coeff0 = coeffs(io.rasterizedQuad.bits.primitiveId)
    val coeff1 = ShiftRegister(coeff0, FpMul.latency + FpAdd.latency)

    val a = FpMul(coeff0.invdW1, lambda(0), !stall)
    val b = FpMul(coeff0.invdW2, lambda(1), !stall)
    val sumC = FpAdd(a, b, !stall)
    val sumD = FpAdd(sumC, coeff1.invW0, !stall)
    depth := FpReciprocal(sumD, !stall)
  }

  val totalLatency = FpMul.latency + FpAdd.latency * 2 + FpReciprocal.latency
  io.interpolatedQuad.bits.quad := ShiftRegister(io.rasterizedQuad.bits, totalLatency, !stall)
  val validStages = RegInit(0.U(totalLatency.W))
  when (!stall) {
    validStages := Cat(validStages(totalLatency - 2, 0), io.rasterizedQuad.valid)
  }

  io.interpolatedQuad.valid := validStages(totalLatency - 1)
  io.rasterizedQuad.ready := !stall
  io.idle := validStages === 0.U
}
