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
  * Set up edge functions and other coefficints.
  * Parameter memory format:
  *  0 - x, y (12.4 fixed point each)
  *  1 - 1.0 / w (float)
  *  2 - varying 0...
  *
  */
class TriangleSetup(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val startTriangle = Flipped(Decoupled(new Bundle{
      val baseAddress = UInt(cfg.vertexParamIndexBits.W) // Base address
      val numVaryings = UInt(5.W)
    }))

    // To VertexParameterMem
    val readParam = Valid(UInt(cfg.vertexParamIndexBits.W))
    val readValue = Input(UInt(32.W))

    // To Rasterizer
    val rasterizerParams = Decoupled(new RasterizerParams)

    // From PixelShaderConductor
    val triangleFinished = Flipped(Valid(UInt(cfg.triangleIdBits.W)))

    // To PixelShaderConductor
    val writeVaryingCoeff = Valid(new Bundle {
      val triangleId = UInt(cfg.triangleIdBits.W)
      val index = UInt(cfg.varyingCoeffIndexBits.W)
      val value = Float32()
    })
  })

  val triangleIdFree = RegInit(VecInit.fill(cfg.maxConcurrentTriangles)(true.B))
  when (io.triangleFinished.valid) {
    assert(!triangleIdFree(io.triangleFinished.bits))
    triangleIdFree(io.triangleFinished.bits) := true.B
  }

  val nextTriangleId = PriorityEncoder(triangleIdFree)

  val rasterizerParamsReg = RegInit(0.U.asTypeOf(new RasterizerParams))
  io.rasterizerParams.bits := rasterizerParamsReg

  val state = RegInit(0.U(2.W))

  io.rasterizerParams.valid := false.B
  io.startTriangle.ready := false.B
  io.writeVaryingCoeff.valid := false.B
  io.writeVaryingCoeff.bits := DontCare
  io.writeVaryingCoeff.bits.triangleId := rasterizerParamsReg.triangleId
  io.readParam.valid := false.B
  io.readParam.bits := 0.U

  val x0 = Reg(UInt(16.W))
  val y0 = Reg(UInt(16.W))
  val x1 = Reg(UInt(16.W))
  val y1 = Reg(UInt(16.W))
  val x2 = Reg(UInt(16.W))
  val y2 = Reg(UInt(16.W))

  def readParam(index: UInt) = {
    io.readParam.valid := true.B
    io.readParam.bits := index
  }

  val baseAddress = Reg(UInt(cfg.vertexParamIndexBits.W))
  val numParams = Reg(UInt(5.W))

  switch (state) {
    is (0.U) {
      // Waiting for the next triangle
      io.startTriangle.ready := true.B
      when (io.startTriangle.fire) {
        state := 1.U
        assert(triangleIdFree(nextTriangleId))
        triangleIdFree(nextTriangleId) := false.B
        rasterizerParamsReg.triangleId := nextTriangleId
        baseAddress := io.startTriangle.bits.baseAddress
        numParams := io.startTriangle.bits.numVaryings
        readParam(io.startTriangle.bits.baseAddress)
      }
    }

    is (1.U) {
      x0 := io.readValue(31, 16)
      y0 := io.readValue(15, 0)
      readParam(baseAddress + 3.U + numParams)
      state := 2.U
    }

    is (2.U) {
      x1 := io.readValue(31, 16)
      y1 := io.readValue(15, 0)
      readParam(baseAddress + (3.U + numParams) << 1)
      state := 3.U
    }

    is (3.U) {
      x2 := io.readValue(31, 16)
      y2 := io.readValue(15, 0)
      state := 4.U
    }

    is (4.U) {
      // Finish
      state := 0.U
      io.rasterizerParams.valid := true.B
    }
  }
}
