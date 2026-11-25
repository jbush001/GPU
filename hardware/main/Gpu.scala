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
    val axiManager = new Axi4IO
    val axiSubordinate = Flipped(new Axi4LiteIO)

    val triangleSetupReady = Output(Bool())
    val writeVertexParam = Flipped(Valid(new Bundle{
      val column = UInt(cfg.vpmColumnIndexBits.W)
      val value = Vec(cfg.shaderVectorLanes, UInt(32.W))
    }))

    val triangleFinished = Valid(UInt(cfg.triangleIdBits.W))
    val fence = Output(Bool())

    val controlBus = Flipped(new ControlBus)
    val tileWriterDone = Output(Bool())
  })

  val controlBusInternal = Wire(new ControlBus)

  val commandProcessor        = Module(new CommandProcessor)
  val rasterizer              = Module(new Rasterizer)
  val triangleSetup           = Module(new TriangleSetup)
  val vertexParameterMem      = Module(new VertexParameterMem)
  val depthInterpolator       = Module(new DepthInterpolator)
  val tileBuffer              = Module(new TileBuffer)
  val fragShaderConductor     = Module(new FragmentShaderConductor)
  val shaderCore              = Module(new ShaderCore)
  val memoryArbiter           = Module(new MemoryArbiter(numReadPorts = 2, numWritePorts = 1))
  val tileWriter              = Module(new TileWriter)
  val floatArrayToColor       = Module(new ShadedQuadConverter)
  val texturePatternGenerator = Module(new TexturePatternGenerator)
  val hostInterface           = Module(new HostInterface)

  commandProcessor.io.memReadPort <> memoryArbiter.io.readPorts(1)
  hostInterface.io.readScratchpadAddr := commandProcessor.io.readScratchpadAddr
  commandProcessor.io.readScratchpadData := hostInterface.io.readScratchpadData
  commandProcessor.io.writeScratchpad <> hostInterface.io.writeScratchpad
  commandProcessor.io.halt := hostInterface.io.haltCommandProcessor
  hostInterface.io.loadProgram <> commandProcessor.io.loadProgram
  commandProcessor.io.tileWriterDone := tileWriter.io.done
  commandProcessor.io.triangleSetupReady := triangleSetup.io.ready
  commandProcessor.io.fenceEvent := fragShaderConductor.io.fence

  // XXX this is a placeholder hack: allow the test harness to directly write to the
  // control bus for now so we can migrate functions to the command processor
  // incrementally.
  controlBusInternal.regWrite.valid := (io.controlBus.regWrite.valid ||
    commandProcessor.io.controlBus.regWrite.valid)
  controlBusInternal.regWrite.bits := Mux(io.controlBus.regWrite.valid,
    io.controlBus.regWrite.bits,
    commandProcessor.io.controlBus.regWrite.bits)

  io.triangleFinished <> fragShaderConductor.io.triangleFinished
  io.fence := fragShaderConductor.io.fence

  triangleSetup.io.controlBus <> controlBusInternal
  vertexParameterMem.io.write0 <> io.writeVertexParam
  triangleSetup.io.paramReadAddr <> vertexParameterMem.io.readAddr1
  triangleSetup.io.readValue := vertexParameterMem.io.readValue1
  triangleSetup.io.triangleFinished <> fragShaderConductor.io.triangleFinished

  // XXX placeholder. This will be used by the vertex shader.
  vertexParameterMem.io.readAddr0.valid := false.B
  vertexParameterMem.io.readAddr0.bits := DontCare
  vertexParameterMem.io.write1.valid := false.B
  vertexParameterMem.io.write1.bits := DontCare

  hostInterface.io.axiSubordinate <> io.axiSubordinate

  rasterizer.io.quad <> depthInterpolator.io.rasterizedQuad
  depthInterpolator.io.interpolatedQuad <> fragShaderConductor.io.sourceQuad

  fragShaderConductor.io.startJob <> shaderCore.io.startJob
  shaderCore.io.jobFinished <> fragShaderConductor.io.jobFinished
  shaderCore.io.regRead <> fragShaderConductor.io.shaderRegRead
  fragShaderConductor.io.shaderRegReadData <> shaderCore.io.regReadData
  shaderCore.io.regWrite <> fragShaderConductor.io.shaderRegWrite
  shaderCore.io.ioWakeJob <> fragShaderConductor.io.ioWakeJob
  fragShaderConductor.io.shadedQuad <> floatArrayToColor.io.shadedQuad
  floatArrayToColor.io.colorQuad <> tileBuffer.io.colorQuad
  shaderCore.io.icacheReadPort <> memoryArbiter.io.readPorts(0)
  memoryArbiter.io.axiBus <> io.axiManager
  shaderCore.io.invalidateICache := false.B

  tileBuffer.io.flushData <> tileWriter.io.flushData
  tileWriter.io.writePort <> memoryArbiter.io.writePorts(0)
  io.tileWriterDone := tileWriter.io.done
  rasterizer.io.params <> triangleSetup.io.rasterizerParams
  io.triangleSetupReady := triangleSetup.io.ready
  tileBuffer.io.controlBus <> controlBusInternal
  tileWriter.io.controlBus <> controlBusInternal
  fragShaderConductor.io.writeVaryingCoeff <> triangleSetup.io.writeVaryingCoeff

  fragShaderConductor.io.textureFetchRequest <> texturePatternGenerator.io.textureFetchRequest
  texturePatternGenerator.io.textureFetchResponse <> fragShaderConductor.io.textureFetchResponse
}
