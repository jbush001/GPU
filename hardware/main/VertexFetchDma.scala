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

/**
  * The Vertex Fetch DMA handles memory reads for vertex data, providing a
  * streaming interface to downstream processing units.
  *
  * @todo this is a stub.
  */
class VertexFetchDma(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val readPort = new MemReadPort
  })

  io.readPort.burst.valid := false.B
  io.readPort.burst.bits := DontCare
  io.readPort.data.ready := true.B
}

