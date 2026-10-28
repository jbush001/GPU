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

class InterpolatedQuad(implicit cfg: GpuConfig) extends RasterizedQuad {
  val depths = Vec(Consts.pixelsPerQuad, Float32())
}

/** Sits between [[Rasterizer]] and [[PixelShaderConductor]] and computes
  * per-pixel depth and perspective corrected barycentric coordinates.
  * This is fully pipelined and can accept a new quad each cycle. It has 17
  * cycles of latency. It will stall when the interpolated quad output
  * deasserts its ready flag.
  * The formulae are fully documented in docs/maths.md
  */
class DepthInterpolator(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    // From Rasterizer
    val rasterizedQuad = Flipped(Decoupled(new RasterizedQuad))

    // To Pixel Shader Conductor
    val interpolatedQuad = Decoupled(new InterpolatedQuad)
  })

  val stall = io.interpolatedQuad.valid && !io.interpolatedQuad.ready

  val totalLatency = 17
  val delayedSourceQuad = ShiftRegister(io.rasterizedQuad.bits, totalLatency, !stall)

  // Connect all relevant fields to the destination
  io.interpolatedQuad.bits.elements.foreach { case (name, destData) =>
    delayedSourceQuad.elements.get(name).foreach { srcData =>
      destData := srcData
    }
  }

  // Need to delay these so they are available at the correct cycle for
  // each stage in the pipeline where they are used.
  val coeff0 = io.rasterizedQuad.bits.depthCoeffs
  val invW0_5 = ShiftRegister(coeff0.invW0, 5, !stall)
  val invW1_15 = ShiftRegister(coeff0.invW1, 15, !stall)
  val invW2_15 = ShiftRegister(coeff0.invW2, 15, !stall)

  for (pixel <- 0 until Consts.pixelsPerQuad) {
    // Compute inverse depth at each pixel
    // invW_pixel = invW1 * lambda0 + invW2 * lambda1 + invW0
    val lambda = io.rasterizedQuad.bits.lambda(pixel)
    val a = FloatMul(coeff0.invdW1, lambda(0), !stall) // cycle 0
    val b = FloatMul(coeff0.invdW2, lambda(1), !stall)
    val sumC = FloatAdd(a, b, !stall) // cycle 2
    val invW = FloatAdd(sumC, invW0_5, !stall) // cycle 5

    // Compute the depth at each pixel: depth = 1.0 / invW_pixel
    val w = FloatReciprocal(invW, !stall) // cycle 8

    // Compute perspective corrected barycentric coordinates
    // lambda_n_pixel_corrected = lambda_n_pixel * w_pixel * invW_n
    val lambda0_13 = ShiftRegister(lambda(0), 13, !stall)
    val lambda1_13 = ShiftRegister(lambda(1), 13, !stall)
    val e0 = FloatMul(w, lambda0_13, !stall) // cycle 13
    val e1 = FloatMul(w, lambda1_13, !stall)

    // Replace the lambda values in the interpolated quad with perspective corrected ones.
    io.interpolatedQuad.bits.lambda(pixel)(0) := FloatMul(e0, invW1_15, !stall) // Cycle 15
    io.interpolatedQuad.bits.lambda(pixel)(1) := FloatMul(e1, invW2_15, !stall)
    io.interpolatedQuad.bits.depths(pixel) := ShiftRegister(w, 4, !stall)
  }

  val validStages = RegInit(0.U(totalLatency.W))
  when (!stall) {
    validStages := Cat(validStages(totalLatency - 2, 0), io.rasterizedQuad.valid)
  }

  io.interpolatedQuad.valid := validStages(totalLatency - 1)
  io.rasterizedQuad.ready := !stall
}
