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

class SetupParams(implicit cfg: GpuConfig) extends Bundle {
  val vertexBaseAddr = Vec(3, UInt(cfg.vertexParamIndexBits.W))
  val numVaryings = UInt(5.W)
  val bbLeft = SInt(cfg.coordinateBits.W)
  val bbTop = SInt(cfg.coordinateBits.W)
}

/**
  * Set up edge functions and other coefficints.
  * Parameter memory format:
  *  0 - x, y (12.4 fixed point each)
  *  1 - 1.0 / w (float)
  *  2 - varying 0...
  *
  *  XXX WIP, incomplete
  *  - Does not normalize edge equations
  *  - Does not write varying coefficients.
  *  - Does not implement top-left rule for pixel hits.
  */
class TriangleSetup(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val startTriangle = Flipped(Decoupled(new SetupParams))

    // To VertexParameterMem
    val paramReadAddr = Valid(UInt(cfg.vertexParamIndexBits.W))
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

  val state = RegInit(0.U(5.W))
  val setupParamsReg = Reg(new SetupParams)

  io.rasterizerParams.valid := false.B
  io.startTriangle.ready := state === 0.U && triangleIdFree.reduce(_ || _)
  io.writeVaryingCoeff.valid := false.B
  io.writeVaryingCoeff.bits := DontCare
  io.writeVaryingCoeff.bits.triangleId := rasterizerParamsReg.triangleId
  io.paramReadAddr.valid := false.B
  io.paramReadAddr.bits := 0.U

  val x0 = Reg(SInt(16.W))
  val y0 = Reg(SInt(16.W))
  val x1 = Reg(SInt(16.W))
  val y1 = Reg(SInt(16.W))
  val x2 = Reg(SInt(16.W))
  val y2 = Reg(SInt(16.W))

  val fpAdd = Module(new FpAdd) // 3 cycles of latency
  fpAdd.io.en := true.B
  fpAdd.io.addend1 := DontCare
  fpAdd.io.addend2 := DontCare

  val a = setupParamsReg.bbLeft
  val b = WireInit(0.S(16.W))
  val c = WireInit(0.S(16.W))
  val d = WireInit(0.S(16.W))
  val e = setupParamsReg.bbTop
  val f = WireInit(0.S(16.W))
  val g = WireInit(0.S(16.W))
  val h = WireInit(0.S(16.W))

  val d0 = c - d
  val d1 = g - h
  val determinant = RegNext(((a - b) * d0) - ((e - f) * d1))
  val d0Reg = RegNext(d0)
  val d1Reg = RegNext(d1)

  val varyingIndex = Reg(UInt(5.W))
  val c0 = Reg(Float32())
  val area2 = RegInit(0.S(32.W))

  /**
   * Initiate read for a value from the parameter memory.
   * Has one cycle of latency.
   */
  def readParameterMem(vertex: UInt, value: UInt) = {
    io.paramReadAddr.valid := true.B
    val params = Mux(state === 0.U, io.startTriangle.bits, setupParamsReg)
    io.paramReadAddr.bits := params.vertexBaseAddr(vertex) + value
  }

  switch (state) {
    is (0.U) {
      // Waiting for the next triangle
      when (io.startTriangle.fire) {
        assert(triangleIdFree(nextTriangleId))
        triangleIdFree(nextTriangleId) := false.B
        rasterizerParamsReg.triangleId := nextTriangleId
        setupParamsReg := io.startTriangle.bits
        varyingIndex := 0.U
        readParameterMem(0.U, 0.U) // read X0/Y0
        state := 1.U
      }
    }

    is (1.U) {
      x0 := io.readValue(31, 16).asSInt
      y0 := io.readValue(15, 0).asSInt
      readParameterMem(1.U, 0.U) // Read X1/Y1
      state := 2.U
    }

    is (2.U) {
      val readX = io.readValue(31, 16).asSInt
      val readY = io.readValue(15, 0).asSInt
      x1 := readX
      y1 := readY

      b := x0
      c := y0
      d := readY
      f := y0
      g := x0
      h := readX

      readParameterMem(2.U, 0.U) // Read X2/Y2
      state := 3.U
    }

    is (3.U) {
      val readX = io.readValue(31, 16).asSInt
      val readY = io.readValue(15, 0).asSInt

      x2 := readX
      y2 := readY

      rasterizerParamsReg.edges(0).xStep := d0Reg
      rasterizerParamsReg.edges(0).yStep := d1Reg
      rasterizerParamsReg.edges(0).initialValue := determinant
      area2 := determinant

      b := x1
      c := y1
      d := readY
      f := y1
      g := x1
      h := readX

      readParameterMem(0.U, 1.U) // read invW0
      state := 4.U
    }

    is (4.U) {
      rasterizerParamsReg.depthCoeffs.invW0 := Float32(io.readValue)

      rasterizerParamsReg.edges(1).xStep := d0Reg
      rasterizerParamsReg.edges(1).yStep := d1Reg
      rasterizerParamsReg.edges(1).initialValue := determinant
      area2 := area2 + determinant

      b := x2
      c := y2
      d := y0
      f := y2
      g := x2
      h := x0

      readParameterMem(1.U, 1.U) // read invW1
      state := 5.U
    }

    is (5.U) {
      rasterizerParamsReg.depthCoeffs.invW1 := Float32(io.readValue)
      fpAdd.io.addend1 := Float32(io.readValue)
      fpAdd.io.addend2 := rasterizerParamsReg.depthCoeffs.invW0.neg
      state := 6.U

      rasterizerParamsReg.edges(2).xStep := d0Reg
      rasterizerParamsReg.edges(2).yStep := d1Reg
      rasterizerParamsReg.edges(2).initialValue := determinant
      area2 := area2 + determinant

      // XXX perform backface/sliver culling if area2 <= some small value.
      // Kick off reciprocal calculation for area2 to normalize

      readParameterMem(2.U, 1.U) // Read invW2
      state := 6.U
    }

    is (6.U) {
      rasterizerParamsReg.depthCoeffs.invW2 := Float32(io.readValue)
      fpAdd.io.addend1 := Float32(io.readValue)
      fpAdd.io.addend2 := rasterizerParamsReg.depthCoeffs.invW0.neg
      when (setupParamsReg.numVaryings =/= 0.U) {
        readParameterMem(0.U, 2.U) // Read first varying value C0
      }

      state := 7.U
    }

    is (7.U) {
      when (setupParamsReg.numVaryings =/= 0.U) {
        io.writeVaryingCoeff.valid := true.B
        io.writeVaryingCoeff.bits.index := 0.U
        io.writeVaryingCoeff.bits.value := Float32(io.readValue) // Save C0
        c0 := Float32(io.readValue)
        readParameterMem(1.U, 2.U) // Read varying value C1
      }

      state := 8.U
    }

    is (8.U) {
      rasterizerParamsReg.depthCoeffs.invdW1 := fpAdd.io.sum
      when (setupParamsReg.numVaryings =/= 0.U) {
        readParameterMem(2.U, 2.U) // Read varying value C2
      }

      fpAdd.io.addend1 := Float32(io.readValue)
      fpAdd.io.addend2 := c0.neg
      state := 9.U
    }

    is (9.U) {
      rasterizerParamsReg.depthCoeffs.invdW2 := fpAdd.io.sum
      when (setupParamsReg.numVaryings > 1.U) {
        readParameterMem(0.U, 3.U)
        state := 10.U
      }.otherwise {
        state := 14.U
      }

      fpAdd.io.addend1 := Float32(io.readValue)
      fpAdd.io.addend2 := c0.neg
    }

    // Param read loop
    is (10.U) {
      state := 11.U
    }

    is (11.U) {
      io.writeVaryingCoeff.valid := true.B
      io.writeVaryingCoeff.bits.index := 1.U
      io.writeVaryingCoeff.bits.value := fpAdd.io.sum // Write dC1
      state := 12.U
    }

    is (12.U) {
      io.writeVaryingCoeff.valid := true.B
      io.writeVaryingCoeff.bits.index := 1.U
      io.writeVaryingCoeff.bits.value := fpAdd.io.sum // Write dC2
      state := 13.U
    }

    is (13.U) {
      // XXX keep reading varyings...
      state := 14.U
    }

    is (14.U) {
      // Finish
      io.rasterizerParams.valid := true.B
      when (io.rasterizerParams.ready) {
        state := 0.U
      }
    }
  }
}
