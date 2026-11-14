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

import chisel3.util._

object Consts {
  final val triangleEdges = 3
  final val triangleVertices = 3
  final val pixelsPerQuad = 4
}

/** Compile-time parameters that size and configure the GPU hardware.
  *
  * A single instance is passed implicitly to every module, so changing a
  * value here reconfigures the whole design at elaboration time.
  * Several fields are coupled; the constraints are noted per field and
  * checked below.
  */
case class GpuConfig(
  traceEnable:         Boolean = false, // printfs shader instruction execution.

  tileSizePixels:      Int = 64,
  edgeFunctionBits:    Int = 40,
  coordinateBits:      Int = 16,
  busBurstLengthBits:  Int = 8,
  shaderThreads:       Int = 8, // Number of hardware threads per shader core
  shaderVectorLanes:   Int = 16,
  shaderJobIdBits:     Int = 4,
  cacheLineSizeBytes:  Int = 64,
  icacheLines:         Int = 64,
  triangleIdBits:      Int = 2,
  maxVertexAttribs:    Int = 16,
  vpmColumns:          Int = 64,
  numShaderRegisters:  Int = 32,

  // Changing these will break things. Maybe move to Axi.
  busAddressBits:      Int = 64,
  busDataBits:         Int = 64,
) {
  require(isPow2(tileSizePixels), "tileSizePixels must be a power of two")
  require(isPow2(shaderThreads), "shaderThreads must be a power of two")
  require(isPow2(shaderVectorLanes), "shaderVectorLanes must be a power of two")
  require(busDataBits % 8 == 0, "busDataBits must be a multiple of 8")
  require(numShaderRegisters < 64)

  // Derived values
  val tileCoordBits = log2Up(tileSizePixels)
  val totalTilePixels = tileSizePixels * tileSizePixels
  val icacheIndexBits = log2Up(icacheLines)
  val cacheLineOffsetBits = log2Up(cacheLineSizeBytes)
  val icacheTagBits = busAddressBits - icacheIndexBits - cacheLineOffsetBits
  val textureRequestIdBits = shaderJobIdBits + log2Up(shaderVectorLanes / Consts.pixelsPerQuad)
  val maxConcurrentTriangles = 1 << triangleIdBits
  val maxVaryingCoeffs = maxVertexAttribs * 3 // 3 coefficients per varying
  val varyingCoeffIndexBits = log2Up(maxVaryingCoeffs)
  val shaderGprIndexBits = log2Up(numShaderRegisters)
  val vpmColumnIndexBits = log2Up(vpmColumns)
}
