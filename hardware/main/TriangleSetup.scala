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

class TriangleSetupParams(implicit cfg: GpuConfig) extends Bundle {
  val offset = Point2D()
  val lastTriangle = Bool()
  val vertexBaseAddr = Vec(Consts.triangleVertices, UInt(cfg.vertexParamIndexBits.W))
  val numVaryings = UInt(5.W)
  val boundingBox = BoundingBox()
}

/**
  * Compute edge functions and other coefficients for rasterization
  * and interpolation.
  * Parameter memory format (for each vertex)
  *  0 - x, y (16 bit point each)
  *  1 - 1.0 / w (float)
  *  2 - varying 0...
  *
  *  @todo Does not implement top-left rule for pixel hits.
  */
class TriangleSetup(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val startTriangle = Flipped(Decoupled(new TriangleSetupParams))

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
  val setupParamsReg = Reg(new TriangleSetupParams)

  // Varying Pipeline Valid Signals
  val varyingStage1Valid = RegInit(false.B)
  val varyingStage2Valid = RegInit(false.B)
  val varyingStage3Valid = RegInit(false.B)
  val varyingStage4Valid = RegInit(false.B)

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

  val recip = Module(new FixedPointReciprocal) // 3 cycles of latency

  val a = setupParamsReg.boundingBox.top
  val b = WireInit(0.S(16.W))
  val c = WireInit(0.S(16.W))
  val d = WireInit(0.S(16.W))
  val e = setupParamsReg.boundingBox.left
  val f = WireInit(0.S(16.W))
  val h = WireInit(0.S(16.W))
  val g = WireInit(0.S(16.W))

  val xStep = g - h
  val yStep = c - d
  val determinant = RegNext(((a - b) * yStep) + ((e - f) * xStep))
  val d0Reg = RegNext(xStep)
  val d1Reg = RegNext(yStep)

  val area2 = RegInit(0.S(cfg.edgeFunctionBits.W))
  recip.io.in := area2 + determinant

  /**
   * Initiate read for e value from the parameter memory.
   * Has one cycle of latency.
   */
  def readParameterMem(vertex: UInt, value: UInt) = {
    io.paramReadAddr.valid := true.B
    val params = Mux(state === 0.U, io.startTriangle.bits, setupParamsReg)
    io.paramReadAddr.bits := params.vertexBaseAddr(vertex) + value
  }

  def normalizeAndRound(value: SInt, multiplier: SInt): SInt = {
    val product = value * multiplier
    val result = (product >> 24) + Mux(product(47), 1.S(48.W), 0.S)
    result
  }

  val varyingSetupActive = RegInit(false.B)
  val varyingVertex = RegInit(0.U(2.W))
  val varyingIndex = RegInit(0.U(5.W))

  switch (state) {
    is (0.U) {
      // Waiting for the next triangle
      when (io.startTriangle.fire) {
        assert(triangleIdFree(nextTriangleId))
        triangleIdFree(nextTriangleId) := false.B
        rasterizerParamsReg.triangleId := nextTriangleId
        rasterizerParamsReg.offset := io.startTriangle.bits.offset
        rasterizerParamsReg.boundingBox := io.startTriangle.bits.boundingBox
        rasterizerParamsReg.lastTriangle := io.startTriangle.bits.lastTriangle
        setupParamsReg := io.startTriangle.bits
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

      b := y0
      c := x0
      d := readX
      f := x0
      g := readY
      h := y0

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

      b := y1
      c := x1
      d := readX
      f := x1
      g := readY
      h := y1

      readParameterMem(0.U, 1.U) // read invW0
      state := 4.U
    }

    is (4.U) {
      rasterizerParamsReg.depthCoeffs.invW0 := Float32(io.readValue)

      rasterizerParamsReg.edges(1).xStep := d0Reg
      rasterizerParamsReg.edges(1).yStep := d1Reg
      rasterizerParamsReg.edges(1).initialValue := determinant
      area2 := area2 + determinant

      b := y2
      c := x2
      d := x0
      f := x2
      g := y0
      h := y2

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

      when (area2 + determinant <= 0.S) {
        // Backface/edge-on culling
        triangleIdFree(rasterizerParamsReg.triangleId) := true.B
        state := 0.U
      }.otherwise {
        // XXX this is where area2 gets enqueued in the reciprocal pipeline

        readParameterMem(2.U, 1.U) // Read invW2
        state := 6.U
      }
    }

    is (6.U) {
      rasterizerParamsReg.depthCoeffs.invW2 := Float32(io.readValue)
      fpAdd.io.addend1 := Float32(io.readValue)
      fpAdd.io.addend2 := rasterizerParamsReg.depthCoeffs.invW0.neg
      when (setupParamsReg.numVaryings =/= 0.U) {
        varyingSetupActive := true.B
        varyingVertex := 0.U
        varyingIndex := 0.U
      }

      state := 7.U
    }

    is (7.U) {
      state := 8.U
    }

    is (8.U) {
      // Pull the result that was enqueued in the reciprocal pipeline in cycle 5
      val multiplier = recip.io.result
      for (edge <- 0 until 3) {
        rasterizerParamsReg.edges(edge).xStep := normalizeAndRound(rasterizerParamsReg.edges(edge).xStep, multiplier)
        rasterizerParamsReg.edges(edge).yStep := normalizeAndRound(rasterizerParamsReg.edges(edge).yStep, multiplier)
        rasterizerParamsReg.edges(edge).initialValue := normalizeAndRound(rasterizerParamsReg.edges(edge).initialValue, multiplier)
      }

      rasterizerParamsReg.depthCoeffs.invdW1 := fpAdd.io.sum
      state := 9.U
    }

    is (9.U) {
      rasterizerParamsReg.depthCoeffs.invdW2 := fpAdd.io.sum
      state := 10.U
    }

    is (10.U) {
      // Finish
      when (!varyingSetupActive && !varyingStage1Valid && !varyingStage2Valid && !varyingStage3Valid && !varyingStage4Valid) {
        io.rasterizerParams.valid := true.B
        when (io.rasterizerParams.ready) {
          state := 0.U
        }
      }
    }
  }

  // Varying setup pipeline
  // Default values
  io.writeVaryingCoeff.valid := false.B
  io.writeVaryingCoeff.bits  := DontCare

  val c0 = Reg(Float32())

  // Pipelined Metadata Registers
  val stage1Vertex = RegInit(0.U(2.W))
  val writeVaryingIndex0 = WireInit(0.U(cfg.varyingCoeffIndexBits.W))

  // Stage 1: Issue fetch address
  when (varyingSetupActive) {
    varyingStage1Valid  := true.B
    stage1Vertex := varyingVertex
    readParameterMem(varyingVertex, varyingIndex + 2.U)

    when (varyingVertex === 2.U) {
      varyingVertex := 0.U
      when (varyingIndex === setupParamsReg.numVaryings - 1.U) {
        varyingSetupActive := false.B // Disable fetch engine, let pipeline drain
      } .otherwise {
        varyingIndex := varyingIndex + 1.U
      }
    } .otherwise {
      varyingVertex := varyingVertex + 1.U
    }
  } .otherwise {
    varyingStage1Valid := false.B
  }

  // These are stored in varying coefficient memory in the order dC1, dC2, d0,
  // but we read them out of vertex parameter memory as d0, dC1, dC2, so we
  // need to adjust the destination address here.
  val writeVaryingIndex = varyingIndex * 3.U + MuxLookup(
    varyingVertex, 0.U)(Seq(
      0.U -> 2.U,
      1.U -> 0.U,
      2.U -> 1.U
    )
  )

  // Stage 2: memory read result
  varyingStage2Valid := varyingStage1Valid

  val readVal = Float32(io.readValue)

  when (varyingStage1Valid) {
    when (stage1Vertex === 0.U) {
      // Latch base parameter C0 and write out directly
      c0 := readVal
      fpAdd.io.addend1 := readVal
      fpAdd.io.addend2 := Float32.Zero
    } .otherwise {
      // Launch dC1 (vertex 1) or dC2 (vertex 2) into FPAdd
      fpAdd.io.addend1 := readVal
      fpAdd.io.addend2 := c0.neg
    }
  }

  varyingStage3Valid := varyingStage2Valid
  varyingStage4Valid := varyingStage3Valid

  // Write out completed coefficients
  io.writeVaryingCoeff.valid := varyingStage4Valid
  io.writeVaryingCoeff.bits.triangleId := rasterizerParamsReg.triangleId
  io.writeVaryingCoeff.bits.index := ShiftRegister(writeVaryingIndex, 4)
  io.writeVaryingCoeff.bits.value := fpAdd.io.sum
}

// XXX this is a stub.
// 3 cycles of latency
class FixedPointReciprocal(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val in = Input(SInt(cfg.edgeFunctionBits.W))
    val result = Output(SInt(48.W))
  })

  io.result := ShiftRegister(0xffffff000000L.S / io.in, 3)
}