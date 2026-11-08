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

/** Top level GPU module
  * @see [[docs/block-diagram.md]] for details of how these modules
  * interact.
  */
class Gpu(implicit val cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val axiBus = new AxiBus

    val startTriangle = Flipped(Decoupled(new TriangleSetupParams))
    val writeVertexParam = Flipped(Valid(new Bundle{
      val column = UInt(cfg.vpmColumnIndexBits.W)
      val value = Vec(cfg.shaderVectorLanes, UInt(32.W))
    }))

    val triangleFinished = Valid(UInt(cfg.triangleIdBits.W))
    val lastTriangle = Output(Bool())

    val controlBus = Flipped(new ControlBus)
    val tileWriterDone = Output(Bool())
  })

  val rasterizer              = Module(new Rasterizer)
  val triangleSetup           = Module(new TriangleSetup)
  val vertexParameterMem      = Module(new VertexParameterMem)
  val depthInterpolator       = Module(new DepthInterpolator)
  val tileBuffer              = Module(new TileBuffer)
  val pixelShaderConductor    = Module(new PixelShaderConductor)
  val shaderCore              = Module(new ShaderCore)
  val memoryArbiter           = Module(new MemoryArbiter(1, 1))
  val tileWriter              = Module(new TileWriter)
  val floatArrayToColor       = Module(new ShadedQuadConverter)
  val texturePatternGenerator = Module(new TexturePatternGenerator)

  io.triangleFinished <> pixelShaderConductor.io.triangleFinished
  io.lastTriangle := pixelShaderConductor.io.lastTriangle

  triangleSetup.io.startTriangle <> io.startTriangle
  triangleSetup.io.controlBus <> io.controlBus
  vertexParameterMem.io.writeParam <> io.writeVertexParam
  triangleSetup.io.paramReadAddr <> vertexParameterMem.io.readAddr
  triangleSetup.io.readValue := vertexParameterMem.io.readValue
  triangleSetup.io.triangleFinished <> pixelShaderConductor.io.triangleFinished

  rasterizer.io.quad <> depthInterpolator.io.rasterizedQuad
  depthInterpolator.io.interpolatedQuad <> pixelShaderConductor.io.sourceQuad

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
  shaderCore.io.invalidateICache := false.B

  tileBuffer.io.flushData <> tileWriter.io.flushData
  tileWriter.io.writePort <> memoryArbiter.io.writePorts(0)
  io.tileWriterDone := tileWriter.io.done
  rasterizer.io.params <> triangleSetup.io.rasterizerParams
  tileBuffer.io.controlBus <> io.controlBus
  tileWriter.io.controlBus <> io.controlBus
  pixelShaderConductor.io.writeVaryingCoeff <> triangleSetup.io.writeVaryingCoeff

  pixelShaderConductor.io.textureFetchRequest <> texturePatternGenerator.io.textureFetchRequest
  texturePatternGenerator.io.textureFetchResponse <> pixelShaderConductor.io.textureFetchResponse
}
