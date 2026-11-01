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
  * Stores raw and shaded vertex parameters, acting as a communication channel
  * between vertex processing stages and [[TriangleSetup]].
  *
  * Memory is arranged as a 16 row by M column array. Each vertex shader
  * vector store writes a full column. Each vertex has an arbitrary number of
  * parameters.
  *
  *  [ v0a0   v0a1 ...  v0aN  v16a0 v16a1 ... v16aN ]
  *  [ v1a0   v1a1 ...  v1aN  v17a0 v17a1 ... v17aN ]
  *  [ v2a0   v2a1 ...  v2aN  v18a0 v18a1 ... v18aN ]
  *  ...
  *  [ v14a0 v14a1 ... v14aN  v30a0 v30a1 ... v30aN ]
  *  [ v15a0 v15a1 ... v15aN  v31a0 v31a1 ... v31aN ]
  *
  * The read address is formed by concatenating the column index and the row
  * index (so they are column-major)
  */
class VertexParameterMem(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val writeParam = Flipped(Valid(new Bundle{
      val column = UInt(cfg.vpmColumnIndexBits.W)
      val value = Vec(cfg.shaderVectorLanes, UInt(32.W))
    }))

    val readAddr = Flipped(Valid(UInt(cfg.vpmReadAddrBits.W)))
    val readValue = Output(UInt(32.W))
  })

  val mem = SyncReadMem(cfg.vpmColumns,
    Vec(cfg.shaderVectorLanes, UInt(32.W)))

  when (io.writeParam.valid) {
    mem.write(io.writeParam.bits.column, io.writeParam.bits.value)
  }

  val readColumn = io.readAddr.bits(cfg.vpmReadAddrBits - 1, log2Up(cfg.shaderVectorLanes))
  val readRow = RegNext(io.readAddr.bits(log2Up(cfg.shaderVectorLanes) - 1, 0))
  io.readValue := mem.read(readColumn, io.readAddr.valid)(readRow)
}
