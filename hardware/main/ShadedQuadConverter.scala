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

/** Converts color from floating point representation to fixed point ARGB32.
  * Adapts from the shader output from [[PixelShaderConductor]]to the native
  * color format used by [[TileBuffer]].
  */
class ShadedQuadConverter(implicit val cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    // From PixelShaderConductor
    val shadedQuad = Flipped(Valid(new ShadedQuad))

    // To TileBuffer
    val colorQuad = Valid(new ColorQuad)
  })

  io.colorQuad.bits.location := io.shadedQuad.bits.location
  io.colorQuad.bits.mask := io.shadedQuad.bits.mask
  for (pixel <- 0 until Consts.pixelsPerQuad) {
    for (channel <- 0 until Color.numChannels) {
      io.colorQuad.bits.colors(pixel).channels(channel) :=
        io.shadedQuad.bits.colors(pixel)(channel).toUnorm(Color.channelBits)
    }
  }

  io.colorQuad.bits.depths := io.shadedQuad.bits.depths
  io.colorQuad.valid := io.shadedQuad.valid
}
