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

/** A [[RasterizedQuad]] augmented with per-pixel interpolated depth, produced
  * by [[DepthInterpolator]].
  */
class InterpolatedQuad(implicit cfg: GpuConfig) extends Bundle {
  val quad = new RasterizedQuad
  val depths = Vec(Consts.pixelsPerQuad, Float32())
}

/** Sits between [[Rasterizer]] and [[PixelShaderConductor]] and computes
  * per-pixel depth for each rasterized quad.
  * @todo This is currently a placeholder and doesn't do any computations.
  */
class DepthInterpolator(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val rasterizedQuad = Flipped(Decoupled(new RasterizedQuad))
    val interpolatedQuad = Decoupled(new InterpolatedQuad)
  })

  val result = Wire(new InterpolatedQuad)
  result.quad := io.rasterizedQuad.bits
  result.depths := VecInit(Seq.fill(Consts.pixelsPerQuad)(Float32(1.0f)))

  io.interpolatedQuad <> Queue(io.rasterizedQuad.map(_ => result), entries = 1)
}
