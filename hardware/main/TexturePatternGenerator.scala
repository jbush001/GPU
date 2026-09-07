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

class TexturePatternGenerator(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    // From PixelShaderConductor
    val textureFetchRequest = Flipped(Decoupled(new TextureFetchRequest()))

    // To PixelShaderConductor
    val textureFetchResponse = Valid(new TextureFetchResponse())
  })

  val resultQueue = Module(new Queue(new TextureFetchResponse(), 4))

  // Create a 16x16 checkerboard where each square is one of 8 different colors.
  val responseNext = WireInit(0.U.asTypeOf(new TextureFetchResponse))
  for (lane <- 0 until cfg.shaderVectorLanes) {
    val s = io.textureFetchRequest.bits.coord(0)(lane)
    val t = io.textureFetchRequest.bits.coord(1)(lane)

    val sInt = s.toFixedPoint(16) >> 12 // 0-15
    val tInt = t.toFixedPoint(16) >> 12 // 0-15

    val colorIndex = (sInt + tInt)(2, 0) // 0-7
    responseNext.texels(0)(lane) := Mux(colorIndex(0), Float32.One, Float32.Zero) // R
    responseNext.texels(1)(lane) := Mux(colorIndex(1), Float32.One, Float32.Zero) // G
    responseNext.texels(2)(lane) := Mux(colorIndex(2), Float32.One, Float32.Zero) // B
    responseNext.texels(3)(lane) := Float32.One // A
  }

  responseNext.jobId := io.textureFetchRequest.bits.jobId

  resultQueue.io.enq.valid := io.textureFetchRequest.valid
  io.textureFetchRequest.ready := resultQueue.io.enq.ready
  resultQueue.io.enq.bits := responseNext

  io.textureFetchResponse.valid := resultQueue.io.deq.valid
  io.textureFetchResponse.bits := resultQueue.io.deq.bits
  resultQueue.io.deq.ready := true.B
}
