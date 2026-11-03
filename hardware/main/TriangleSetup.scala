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
  val lastTriangle = Bool()
  val vertexBaseAddr = Vec(Consts.triangleVertices, UInt(cfg.vpmReadAddrBits.W))
  val numVaryings = UInt(5.W)
  val tileOffset = Point2D()
}

/** Compute edge functions and other coefficients for rasterization
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
    val paramReadAddr = Valid(UInt(cfg.vpmReadAddrBits.W))
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

  val triangleIdFreeReg = RegInit(VecInit.fill(cfg.maxConcurrentTriangles)(true.B))
  when (io.triangleFinished.valid) {
    assert(!triangleIdFreeReg(io.triangleFinished.bits))
    triangleIdFreeReg(io.triangleFinished.bits) := true.B
  }

  val nextTriangleId = PriorityEncoder(triangleIdFreeReg)

  val rasterizerParamsReg = RegInit(0.U.asTypeOf(new RasterizerParams))
  io.rasterizerParams.bits := rasterizerParamsReg

  val stateReg = RegInit(0.U(5.W))
  val setupParamsReg = Reg(new TriangleSetupParams)

  // Varying Pipeline Valid Signals
  val varyingStage1ValidReg = RegInit(false.B)
  val varyingStage2ValidReg = RegInit(false.B)
  val varyingStage3ValidReg = RegInit(false.B)
  val varyingStage4ValidReg = RegInit(false.B)

  io.rasterizerParams.valid := false.B
  io.startTriangle.ready := stateReg === 0.U && triangleIdFreeReg.reduce(_ || _)
  io.writeVaryingCoeff.valid := false.B
  io.writeVaryingCoeff.bits := DontCare
  io.writeVaryingCoeff.bits.triangleId := rasterizerParamsReg.triangleId
  io.paramReadAddr.valid := false.B
  io.paramReadAddr.bits := 0.U

  val x0Reg = Reg(SInt(16.W))
  val y0Reg = Reg(SInt(16.W))
  val x1Reg = Reg(SInt(16.W))
  val y1Reg = Reg(SInt(16.W))
  val x2Reg = Reg(SInt(16.W))
  val y2Reg = Reg(SInt(16.W))

  val fpAdd = Module(new FloatAdd) // 3 cycles of latency
  fpAdd.io.en := true.B
  fpAdd.io.addend1 := DontCare
  fpAdd.io.addend2 := DontCare

  val recip = Module(new FixedReciprocal) // 5 cycles of latency

  val a = setupParamsReg.tileOffset.y
  val b = WireInit(0.S(16.W))
  val c = WireInit(0.S(16.W))
  val d = WireInit(0.S(16.W))
  val e = setupParamsReg.tileOffset.x
  val f = WireInit(0.S(16.W))
  val h = WireInit(0.S(16.W))
  val g = WireInit(0.S(16.W))

  val xStep = g - h
  val yStep = c - d
  val determinantReg = RegNext(((a - b) * yStep) + ((e - f) * xStep))
  val d0Reg = RegNext(xStep)
  val d1Reg = RegNext(yStep)

  val area2Reg = RegInit(0.S(cfg.edgeFunctionBits.W))
  recip.io.divisor.valid := stateReg === 5.U && area2Reg + determinantReg > 0.S
  recip.io.divisor.bits := (area2Reg + determinantReg).asUInt

  /** Initiate read for e value from the parameter memory.
    * Has one cycle of latency.
    */
  def readParameterMem(vertex: UInt, index: UInt) = {
    io.paramReadAddr.valid := true.B
    val params = Mux(stateReg === 0.U, io.startTriangle.bits, setupParamsReg)
    io.paramReadAddr.bits := (params.vertexBaseAddr(vertex)
      + (index * cfg.shaderVectorLanes.U))
  }

  // XXX This is not technically correct. I'm not exactly sure why the
  // addition works, but there are gaps between triangles without it.
  def normalizeAndRound(value: SInt, multiplier: SInt): SInt = {
    val product = value * multiplier
    val result = (product >> 24) + Mux(product(47), 1.S(48.W), 0.S)
    result
  }

  val varyingSetupActiveReg = RegInit(false.B)
  val varyingVertexReg = RegInit(0.U(2.W))
  val varyingIndexReg = RegInit(0.U(5.W))

  switch (stateReg) {
    is (0.U) {
      // Waiting for the next triangle
      when (io.startTriangle.fire) {
        assert(triangleIdFreeReg(nextTriangleId))
        triangleIdFreeReg(nextTriangleId) := false.B
        rasterizerParamsReg.triangleId := nextTriangleId
        rasterizerParamsReg.offset := io.startTriangle.bits.tileOffset
        rasterizerParamsReg.boundingBox.left := io.startTriangle.bits.tileOffset.x
        rasterizerParamsReg.boundingBox.top := io.startTriangle.bits.tileOffset.y
        rasterizerParamsReg.boundingBox.right := io.startTriangle.bits.tileOffset.x + (cfg.tileSizePixels - 2).S
        rasterizerParamsReg.boundingBox.bottom := io.startTriangle.bits.tileOffset.y + (cfg.tileSizePixels - 2).S
        rasterizerParamsReg.lastTriangle := io.startTriangle.bits.lastTriangle
        setupParamsReg := io.startTriangle.bits
        readParameterMem(0.U, 0.U) // read X0/Y0
        stateReg := 1.U
      }
    }

    is (1.U) {
      x0Reg := io.readValue(31, 16).asSInt
      y0Reg := io.readValue(15, 0).asSInt
      readParameterMem(1.U, 0.U) // Read X1/Y1
      stateReg := 2.U
    }

    is (2.U) {
      val readX = io.readValue(31, 16).asSInt
      val readY = io.readValue(15, 0).asSInt
      x1Reg := readX
      y1Reg := readY

      b := y0Reg
      c := x0Reg
      d := readX
      f := x0Reg
      g := readY
      h := y0Reg

      readParameterMem(2.U, 0.U) // Read X2/Y2
      stateReg := 3.U
    }

    is (3.U) {
      val readX = io.readValue(31, 16).asSInt
      val readY = io.readValue(15, 0).asSInt

      x2Reg := readX
      y2Reg := readY

      rasterizerParamsReg.edges(0).xStep := d0Reg
      rasterizerParamsReg.edges(0).yStep := d1Reg
      rasterizerParamsReg.edges(0).initialValue := determinantReg
      area2Reg := determinantReg

      b := y1Reg
      c := x1Reg
      d := readX
      f := x1Reg
      g := readY
      h := y1Reg

      readParameterMem(0.U, 1.U) // read invW0
      stateReg := 4.U
    }

    is (4.U) {
      rasterizerParamsReg.depthCoeffs.invW0 := Float32(io.readValue)

      rasterizerParamsReg.edges(1).xStep := d0Reg
      rasterizerParamsReg.edges(1).yStep := d1Reg
      rasterizerParamsReg.edges(1).initialValue := determinantReg
      area2Reg := area2Reg + determinantReg

      b := y2Reg
      c := x2Reg
      d := x0Reg
      f := x2Reg
      g := y0Reg
      h := y2Reg

      readParameterMem(1.U, 1.U) // read invW1
      stateReg := 5.U
    }

    is (5.U) {
      rasterizerParamsReg.depthCoeffs.invW1 := Float32(io.readValue)
      fpAdd.io.addend1 := Float32(io.readValue)
      fpAdd.io.addend2 := rasterizerParamsReg.depthCoeffs.invW0.neg
      stateReg := 6.U

      rasterizerParamsReg.edges(2).xStep := d0Reg
      rasterizerParamsReg.edges(2).yStep := d1Reg
      rasterizerParamsReg.edges(2).initialValue := determinantReg
      area2Reg := area2Reg + determinantReg

      when (area2Reg + determinantReg <= 0.S) {
        // Backface/edge-on culling
        triangleIdFreeReg(rasterizerParamsReg.triangleId) := true.B
        stateReg := 0.U
      }.otherwise {
        // XXX this is where area2Reg gets enqueued in the reciprocal pipeline

        readParameterMem(2.U, 1.U) // Read invW2
        stateReg := 6.U
      }
    }

    is (6.U) {
      rasterizerParamsReg.depthCoeffs.invW2 := Float32(io.readValue)
      fpAdd.io.addend1 := Float32(io.readValue)
      fpAdd.io.addend2 := rasterizerParamsReg.depthCoeffs.invW0.neg
      when (setupParamsReg.numVaryings =/= 0.U) {
        varyingSetupActiveReg := true.B
        varyingVertexReg := 0.U
        varyingIndexReg := 0.U
      }

      stateReg := 7.U
    }

    is (7.U) {
      stateReg := 8.U
    }

    is (8.U) {
      rasterizerParamsReg.depthCoeffs.invdW1 := fpAdd.io.sum
      stateReg := 9.U
    }

    is (9.U) {
      rasterizerParamsReg.depthCoeffs.invdW2 := fpAdd.io.sum
      stateReg := 10.U
    }

    is (10.U) {
      // Pull the result that was enqueued in the reciprocal pipeline in cycle 5
      val multiplier = recip.io.result.asSInt
      for (edge <- 0 until 3) {
        rasterizerParamsReg.edges(edge).xStep := normalizeAndRound(rasterizerParamsReg.edges(edge).xStep, multiplier)
        rasterizerParamsReg.edges(edge).yStep := normalizeAndRound(rasterizerParamsReg.edges(edge).yStep, multiplier)
        rasterizerParamsReg.edges(edge).initialValue := normalizeAndRound(rasterizerParamsReg.edges(edge).initialValue, multiplier)
      }

      stateReg := 11.U
    }

    is (11.U) {
      // Finish
      when (!varyingSetupActiveReg && !varyingStage1ValidReg && !varyingStage2ValidReg && !varyingStage3ValidReg && !varyingStage4ValidReg) {
        io.rasterizerParams.valid := true.B
        when (io.rasterizerParams.ready) {
          stateReg := 0.U
        }
      }
    }
  }

  // Varying setup pipeline
  val c0Reg = Reg(Float32())

  // Pipelined Metadata Registers
  val stage1VertexReg = RegInit(0.U(2.W))
  val writeVaryingIndex0 = WireInit(0.U(cfg.varyingCoeffIndexBits.W))

  // Stage 1: Issue fetch address
  when (varyingSetupActiveReg) {
    varyingStage1ValidReg  := true.B
    stage1VertexReg := varyingVertexReg
    readParameterMem(varyingVertexReg, varyingIndexReg + 2.U)

    when (varyingVertexReg === 2.U) {
      varyingVertexReg := 0.U
      when (varyingIndexReg === setupParamsReg.numVaryings - 1.U) {
        varyingSetupActiveReg := false.B // Disable fetch engine, let pipeline drain
      } .otherwise {
        varyingIndexReg := varyingIndexReg + 1.U
      }
    } .otherwise {
      varyingVertexReg := varyingVertexReg + 1.U
    }
  } .otherwise {
    varyingStage1ValidReg := false.B
  }

  // These are stored in varying coefficient memory in the order dC1, dC2, d0,
  // but we read them out of vertex parameter memory as d0, dC1, dC2, so we
  // need to adjust the destination address here.
  val writeVaryingIndex = varyingIndexReg * 3.U + MuxLookup(
    varyingVertexReg, 0.U)(Seq(
      0.U -> 2.U,
      1.U -> 0.U,
      2.U -> 1.U
    )
  )

  // Stage 2: memory read result
  varyingStage2ValidReg := varyingStage1ValidReg

  val readVal = Float32(io.readValue)

  when (varyingStage1ValidReg) {
    when (stage1VertexReg === 0.U) {
      // Latch base parameter C0 and write out directly
      c0Reg := readVal
      fpAdd.io.addend1 := readVal
      fpAdd.io.addend2 := Float32.Zero
    } .otherwise {
      // Launch dC1 (vertex 1) or dC2 (vertex 2) into FPAdd
      fpAdd.io.addend1 := readVal
      fpAdd.io.addend2 := c0Reg.neg
    }
  }

  varyingStage3ValidReg := varyingStage2ValidReg
  varyingStage4ValidReg := varyingStage3ValidReg

  // Write out completed coefficients
  io.writeVaryingCoeff.valid := varyingStage4ValidReg
  io.writeVaryingCoeff.bits.triangleId := rasterizerParamsReg.triangleId
  io.writeVaryingCoeff.bits.index := ShiftRegister(writeVaryingIndex, 4)
  io.writeVaryingCoeff.bits.value := fpAdd.io.sum
}
