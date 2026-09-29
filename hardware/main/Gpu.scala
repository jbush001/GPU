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
import gpu.shader._

/** Top level GPU module */
class Gpu(implicit val cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val axiBus = new AxiBus

    // Hack: pass throughs for testing
    val coeffs = Flipped(Decoupled(new RasterizerCoeffs))
    val writeVaryingCoeff = Flipped(Valid(new Bundle {
      val triangleId = UInt(cfg.triangleIdBits.W)
      val index = UInt(5.W)
      val value = Float32()
    }))

    val writeDepthCoeffs = Flipped(Valid(new Bundle {
      val triangleId = UInt(cfg.triangleIdBits.W)
      val coeffs = new DepthInterpolatorCoeffs()
    }))

    val startFlush = Input(Bool())
    val flushData = Decoupled(new Bundle {
      val depth = Float32()
      val color = Color()
    })

    val flushBufferSel = Input(RenderBufferId()) // depth or color buffer
    val complete = Output(Bool())
    val batchFinished = Input(Bool())

    val allocateTriangleId = Decoupled(UInt(cfg.triangleIdBits.W))
  })

  val rasterizer = Module(new Rasterizer)
  val depthInterpolator = Module(new DepthInterpolator)
  val tileBuffer = Module(new TileBuffer)
  val pixelShaderConductor = Module(new PixelShaderConductor)
  val shaderCore = Module(new ShaderCore)
  val memoryArbiter = Module(new MemoryArbiter(1, 1))
  val floatArrayToColor = Module(new ShadedQuadConverter)
  val texturePatternGenerator = Module(new TexturePatternGenerator)
  val quadScoreboard = Module(new QuadScoreboard)

  io.complete := quadScoreboard.io.idle

  rasterizer.io.quad <> depthInterpolator.io.rasterizedQuad
  depthInterpolator.io.interpolatedQuad <> pixelShaderConductor.io.sourceQuad

  pixelShaderConductor.io.flush := quadScoreboard.io.flushPixelShader
  pixelShaderConductor.io.startJob <> shaderCore.io.startJob
  shaderCore.io.jobFinished <> pixelShaderConductor.io.jobFinished
  shaderCore.io.regRead <> pixelShaderConductor.io.shaderRegRead
  pixelShaderConductor.io.shaderRegReadData <> shaderCore.io.regReadData
  shaderCore.io.regWrite <> pixelShaderConductor.io.shaderRegWrite
  shaderCore.io.ioWakeJob <> pixelShaderConductor.io.ioWakeJob
  pixelShaderConductor.io.shadedQuad <> floatArrayToColor.io.floatQuad
  floatArrayToColor.io.shadedQuad <> tileBuffer.io.shadedQuad
  shaderCore.io.icacheReadPort <> memoryArbiter.io.readPorts(0)
  memoryArbiter.io.axiBus <> io.axiBus
  quadScoreboard.io.issueQuad.valid := rasterizer.io.quad.fire
  quadScoreboard.io.issueQuad.bits := rasterizer.io.quad.bits.triangleId
  quadScoreboard.io.batchFinished := io.batchFinished
  quadScoreboard.io.submitQuad := depthInterpolator.io.interpolatedQuad.fire
  quadScoreboard.io.retireQuad.valid := (pixelShaderConductor.io.shadedQuad.fire)
  quadScoreboard.io.retireQuad.bits := pixelShaderConductor.io.shadedQuad.bits.triangleId
  quadScoreboard.io.rasterizationFinished <> rasterizer.io.rasterizationFinished
  io.allocateTriangleId <> quadScoreboard.io.allocateTriangleId

  memoryArbiter.io.writePorts(0).burst.valid := false.B
  memoryArbiter.io.writePorts(0).data.valid := false.B
  memoryArbiter.io.writePorts(0).burst.bits.address := 0.U
  memoryArbiter.io.writePorts(0).burst.bits.length := 0.U
  memoryArbiter.io.writePorts(0).data.bits := 0.U

  tileBuffer.io.clearColor.channels(0) := 0.U
  tileBuffer.io.clearColor.channels(1) := 0.U
  tileBuffer.io.clearColor.channels(2) := 0.U
  tileBuffer.io.clearColor.channels(3) := 0.U
  tileBuffer.io.clearDepth := Float32(1.0f)
  tileBuffer.io.startFlush := io.startFlush
  tileBuffer.io.flushBufferSel := io.flushBufferSel
  tileBuffer.io.enableDepthCheck := true.B
  tileBuffer.io.enableDepthWrite := true.B
  tileBuffer.io.enableBlend := false.B
  tileBuffer.io.flushData <> io.flushData
  rasterizer.io.coeffs <> io.coeffs
  depthInterpolator.io.writeCoeffs <> io.writeDepthCoeffs

  io.writeVaryingCoeff <> pixelShaderConductor.io.writeVaryingCoeff

  pixelShaderConductor.io.textureFetchRequest <> texturePatternGenerator.io.textureFetchRequest
  texturePatternGenerator.io.textureFetchResponse <> pixelShaderConductor.io.textureFetchResponse
}
