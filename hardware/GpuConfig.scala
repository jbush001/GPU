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

import spinal.core._

//
// Global constants
//
object GpuConfig {
  // Configurable parameters
  val depthBits: Int = 24
  val tileSizePixels: Int = 64;
  val edgeFunctionBits = 32;

  val tileCoordBits = log2Up(tileSizePixels)

  assert((GpuConfig.tileSizePixels & (GpuConfig.tileSizePixels - 1)) == 0) // Must be power of two tile
}

// A vertical or horizontal screen coordinate in pixels
// TODO:
// - This doesn't provide any type safety, as it is just a wrapper to produce
//   a consistent type.
// - This should probably be a fixed point value (maybe 14.4) to rasterize
//   accurately.
object ScreenCoord {
  def apply(): SInt = SInt(16 bits)
}
