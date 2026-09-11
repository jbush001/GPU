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

/** Debug texture pattern generator. */
class TexturePatternGenerator(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    // From PixelShaderConductor
    val textureFetchRequest = Flipped(Decoupled(new TextureFetchRequest()))

    // To PixelShaderConductor
    val textureFetchResponse = Valid(new TextureFetchResponse())
  })

  val resultQueue = Module(new Queue(new TextureFetchResponse(), 4))

  val responseNext = WireInit(0.U.asTypeOf(new TextureFetchResponse))
  for (lane <- 0 until Consts.pixelsPerQuad) {
    val s = io.textureFetchRequest.bits.coord(0)(lane)
    val t = io.textureFetchRequest.bits.coord(1)(lane)

    val sInt = s.toFixedPoint(3)  // 0-8
    val tInt = t.toFixedPoint(3)  // 0-8
    val checker = (sInt + tInt)(0)
    responseNext.texels(0)(lane) := Mux(checker, Float32.One, s) // R
    responseNext.texels(1)(lane) := Mux(checker, Float32.One, t) // G
    responseNext.texels(2)(lane) := Mux(checker, Float32.One, Float32(0.2f)) // B
    responseNext.texels(3)(lane) := Float32.One // A
  }

  responseNext.requestId := io.textureFetchRequest.bits.requestId

  resultQueue.io.enq.valid := io.textureFetchRequest.valid
  io.textureFetchRequest.ready := resultQueue.io.enq.ready
  resultQueue.io.enq.bits := responseNext

  io.textureFetchResponse.valid := resultQueue.io.deq.valid
  io.textureFetchResponse.bits := resultQueue.io.deq.bits
  resultQueue.io.deq.ready := true.B
}
