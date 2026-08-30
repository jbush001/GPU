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

//
// Global constants
//
case class GpuConfig(
  // Configurable design parameters
  depthBufferBits: Int = 24,
  tileSizePixels: Int = 64,
  edgeFunctionBits: Int = 32,
  coordinateBits: Int = 16,
  busAddressBits: Int = 36,
  busDataBits: Int = 64,
  busBurstLengthBits: Int = 8,
  shaderThreads: Int = 8, // Number of hardware threads per shader core
  shaderVectorLanes: Int = 16,
  shaderTagBits: Int = 16,
  cacheLineSizeBytes: Int = 64,
  icacheLines: Int = 64,

  traceEnable: Boolean = false
) {
  require(isPow2(tileSizePixels), "tileSizePixels must be a power of two")
  require(isPow2(shaderThreads), "shaderThreads must be a power of two")
  require(isPow2(shaderVectorLanes), "shaderVectorLanes must be a power of two")
  require(busDataBits % 8 == 0, "busDataBits must be a multiple of 8")

  // Derived values
  val tileCoordBits = log2Up(tileSizePixels)
  val totalTilePixels = tileSizePixels * tileSizePixels

  val icacheIndexBits = log2Up(icacheLines)
  val cacheLineOffsetBits = log2Up(cacheLineSizeBytes)
  val icacheTagBits = busAddressBits - icacheIndexBits - cacheLineOffsetBits
}

object Consts {
  final val triangleEdges = 3
  final val pixelsPerQuad = 4
}
