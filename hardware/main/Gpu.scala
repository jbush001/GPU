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
    val edgeCoeffs = Flipped(Decoupled(new EdgeCoeffs))
    val writeVaryingCoeff = Flipped(Valid(new Bundle {
      val index = UInt(5.W)
      val value = new Float32()
    }))

    val startFlush = Input(Bool())
    val flushData = Decoupled(Bits(32.W))
    val flushBufferSel = Input(RenderBufferId()) // depth or color buffer
    val complete = Output(Bool())
  })

  val rasterizer = Module(new Rasterizer)
  val tileBuffer = Module(new TileBuffer)
  val pixelShaderConductor = Module(new PixelShaderConductor)
  val shaderCore = Module(new ShaderCore)
  val memoryArbiter = Module(new MemoryArbiter(1, 1))
  val floatArrayToColor = Module(new FloatArrayToColor)

  io.complete := pixelShaderConductor.io.idle && rasterizer.io.complete

  rasterizer.io.quad <> pixelShaderConductor.io.rasterizedQuad
  pixelShaderConductor.io.flush := rasterizer.io.complete
  pixelShaderConductor.io.startJob <> shaderCore.io.startJob
  shaderCore.io.jobFinished <> pixelShaderConductor.io.jobFinished
  shaderCore.io.regRead <> pixelShaderConductor.io.shaderRegRead
  pixelShaderConductor.io.shaderRegReadData <> shaderCore.io.regReadData
  shaderCore.io.regWrite <> pixelShaderConductor.io.shaderRegWrite
  pixelShaderConductor.io.shadedQuad <> floatArrayToColor.io.floatQuad
  floatArrayToColor.io.shadedQuad <> tileBuffer.io.shadedQuad
  shaderCore.io.icacheReadPort <> memoryArbiter.io.readPorts(0)
  memoryArbiter.io.axiBus <> io.axiBus
  memoryArbiter.io.writePorts(0).burst.valid := false.B
  memoryArbiter.io.writePorts(0).data.valid := false.B
  memoryArbiter.io.writePorts(0).burst.bits.address := 0.U
  memoryArbiter.io.writePorts(0).burst.bits.length := 0.U
  memoryArbiter.io.writePorts(0).data.bits := 0.U

  tileBuffer.io.clearColor.channels(0) := 0.U
  tileBuffer.io.clearColor.channels(1) := 0.U
  tileBuffer.io.clearColor.channels(2) := 0.U
  tileBuffer.io.clearColor.channels(3) := 0.U
  tileBuffer.io.clearDepth := 0xffffff.U(cfg.depthBufferBits.W)
  tileBuffer.io.startFlush := io.startFlush
  tileBuffer.io.flushBufferSel := io.flushBufferSel
  tileBuffer.io.enableDepthCheck := false.B
  tileBuffer.io.enableDepthWrite := true.B
  tileBuffer.io.enableBlend := false.B
  tileBuffer.io.flushData <> io.flushData
  rasterizer.io.edgeCoeffs <> io.edgeCoeffs

  io.writeVaryingCoeff <> pixelShaderConductor.io.writeVaryingCoeff
}
