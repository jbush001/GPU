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
  * Stores raw and shaded vertex parameters, acting as a communication
  * channel between vertex processing stages and the [[TriangleSetup]].
  */
class VertexParameterMem(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val writeParam = Flipped(Valid(new Bundle{
      val addr = UInt(cfg.vertexParamIndexBits.W)
      val value = UInt(32.W)
    }))

    val readAddr = Flipped(Valid(UInt(cfg.vertexParamIndexBits.W)))
    val readValue = Output(UInt(32.W))
  })

  val mem = SyncReadMem(1 << cfg.vertexParamIndexBits, UInt(32.W))

  when (io.writeParam.valid) {
    mem.write(io.writeParam.bits.addr, io.writeParam.bits.value)
  }

  io.readValue := mem.read(io.readAddr.bits, io.readAddr.valid)
}

