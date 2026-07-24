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

//
// Global constants
//
object GpuConfig {
  // Configurable parameters
  final val depthBits = 24
  final val tileSizePixels = 64;
  final val edgeFunctionBits = 32;
  final val coordinateBits = 16;

  // Derived values
  final val tileCoordBits = log2Up(tileSizePixels)

  assert((GpuConfig.tileSizePixels & (GpuConfig.tileSizePixels - 1)) == 0) // Must be power of two tile
}

object Consts {
  final val triangleEdges = 3
  final val pixelsPerQuad = 4
}
