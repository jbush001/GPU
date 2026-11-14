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

class VpmAddress(implicit cfg: GpuConfig) extends Bundle {
  val column = UInt(cfg.vpmColumnIndexBits.W)
  val row = UInt(log2Up(cfg.shaderVectorLanes).W)

  def columnOffset(index: UInt): VpmAddress = {
    val offset = Wire(new VpmAddress)
    offset.column := column + index
    offset.row := row
    offset
  }
}

/** Stores raw and shaded vertex parameters, acting as a communication channel
  * between vertex processing stages and [[TriangleSetup]].
  *
  * Memory is arranged as a 16 row by M column array.
  * Vertex shaders process one vertex per vector lane, so a vector store
  * writes one parameter for up to 16 vertices at once in a single column.
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
    // From [[ShaderCore]]
    val write0 = Flipped(Valid(new Bundle{
      val column = UInt(cfg.vpmColumnIndexBits.W)
      val value = Vec(cfg.shaderVectorLanes, UInt(32.W))
    }))

    // To [[ShaderCore]]
    val readAddr0 = Flipped(Valid(UInt(cfg.vpmColumnIndexBits.W)))
    val readValue0 = Output(Vec(cfg.shaderVectorLanes, UInt(32.W)))

    // From [[VertexFetcher]]
    val write1 = Flipped(Decoupled(new Bundle{
      val address = new VpmAddress()
      val value = UInt(32.W)
    }))

    // To [[TriangleSetup]]
    val readAddr1 = Flipped(Valid(new VpmAddress()))
    val readValue1 = Output(UInt(32.W))
  })

  val mem = SyncReadMem(cfg.vpmColumns,
    Vec(cfg.shaderVectorLanes, UInt(32.W)))

  // Writes from the shader take priority.
  io.write1.ready := !io.write0.valid
  val writeEnble = io.write0.valid || io.write1.valid
  val writeColumn = Mux(io.write0.valid, io.write0.bits.column, io.write1.bits.address.column)
  val writeValue = Mux(io.write0.valid,
    io.write0.bits.value,
    VecInit(Seq.fill(cfg.shaderVectorLanes)(io.write1.bits.value)))
  val writeMask = VecInit.tabulate(cfg.shaderVectorLanes) { i =>
    io.write0.valid || (io.write1.bits.address.row === i.U)
  }

  when (writeEnble) {
    mem.write(writeColumn, writeValue, writeMask)
  }

  io.readValue0 := mem.read(io.readAddr0.bits, io.readAddr0.valid)

  val readColumn = io.readAddr1.bits.column
  val readRowReg = RegNext(io.readAddr1.bits.row)
  io.readValue1 := mem.read(readColumn, io.readAddr1.valid)(readRowReg)
}
