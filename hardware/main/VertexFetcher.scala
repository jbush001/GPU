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

/** The Vertex Fetch DMA handles memory reads for vertex data, providing a
  * streaming interface to downstream processing units.
  *
  * @todo this is a stub.
  */
class VertexFetcher(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    // From [[MemoryArbiter]]
    val readPort = new MemReadPort

    // From [[CommandProcessor]]
    val controlBus = Flipped(new ControlBus)

    // To [[VertexParameterMem]]
    val vertexWrite = Decoupled(new Bundle{
      val column = UInt(cfg.vpmColumnIndexBits.W)
      val row = UInt(log2Up(cfg.shaderVectorLanes).W)
      val value = UInt(32.W)
    })
  })

  class AttrConfigWord extends Bundle {
    val reserved = UInt(5.W)  // [31:27]
    val enable = Bool()     // [26]
    val numComponents = UInt(2.W)  // [25:24]
    val format = UInt(8.W)  // [23:16]
    val stride = UInt(16.W) // [15:0]
  }

  class AttrArray extends Bundle {
    val config      = new AttrConfigWord()
    val baseAddress = Vec(2, UInt(32.W))
  }

  val attrArraysReg = RegInit(0.U.asTypeOf(Vec(cfg.maxVertexAttribs, new AttrArray)))
  val indexBaseReg = RegInit(VecInit(Seq.fill(cfg.busAddressBits / 32)(0.U(32.W))))
  val indexCountReg = RegInit(0.U(32.W))

  when(io.controlBus.regWrite.valid) {
    val data = io.controlBus.regWrite.bits.data
    val addr = io.controlBus.regWrite.bits.address

    val attrBase = 32.U
    val attrEnd = attrBase + (cfg.maxVertexAttribs * 4).U
    val attrIndexBits = log2Ceil(cfg.maxVertexAttribs)

    when (addr >= attrBase && addr < attrEnd) {
      val relativeAddr = addr - attrBase
      val attrIdx = relativeAddr(attrIndexBits + 1, 2)
      val wordOffset = relativeAddr(1, 0)
      switch(wordOffset) {
        is(0.U) {
          attrArraysReg(attrIdx).config := data.asTypeOf(new AttrConfigWord)
        }
        is(1.U) {
          attrArraysReg(attrIdx).baseAddress(0) := data
        }
        is(2.U) {
          attrArraysReg(attrIdx).baseAddress(1) := data
        }
      }
    }.otherwise {
      switch (addr) {
        is(ControlRegisterIndex.IndexBaseL) {
          indexBaseReg(0) := data
        }
        is(ControlRegisterIndex.IndexBaseH) {
          indexBaseReg(1) := data
        }
        is(ControlRegisterIndex.IndexCount) {
          indexCountReg := data
        }
      }
    }
  }
  io.readPort.burst.valid := false.B
  io.readPort.burst.bits := DontCare
  io.readPort.data.ready := true.B

  io.vertexWrite.valid := false.B
  io.vertexWrite.bits := DontCare
}

