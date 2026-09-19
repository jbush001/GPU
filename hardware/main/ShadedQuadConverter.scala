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
  * Converts color from floating point representation to fixed point ARGB32.
  * Adapts from the shader output to the native tile buffer color format.
  */
class ShadedQuadConverter(implicit val cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val shadedQuad = Valid(new ShadedQuad)

    val floatQuad = Flipped(Valid(new Bundle {
      val location = Point2D()
      val mask = Bits(Consts.pixelsPerQuad.W)
      val colors = Vec(Consts.pixelsPerQuad, Vec(Color.numChannels, Float32()))
      val depths = Vec(Consts.pixelsPerQuad, Float32())
    }))
  })

  io.shadedQuad.bits.location := io.floatQuad.bits.location
  io.shadedQuad.bits.mask := io.floatQuad.bits.mask
  for (pixel <- 0 until Consts.pixelsPerQuad) {
    for (channel <- 0 until Color.numChannels) {
      io.shadedQuad.bits.colors(pixel).channels(channel) :=
        io.floatQuad.bits.colors(pixel)(channel).toUnorm(Color.channelBits)
    }
  }

  io.shadedQuad.bits.depths := io.floatQuad.bits.depths
  io.shadedQuad.valid := io.floatQuad.valid
}


