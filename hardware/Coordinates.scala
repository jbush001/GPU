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

class Point2D extends Bundle {
  val x = SInt(GpuConfig.coordinateBits bits)
  val y = SInt(GpuConfig.coordinateBits bits)

  def -(that: Point2D): Point2D = {
    val result = new Point2D()
    result.x := this.x - that.x
    result.y := this.y - that.y
    result
  }
}

object Point2D {
  def apply() = new Point2D()
}

class BoundingBox extends Bundle {
  val top = SInt(GpuConfig.coordinateBits bits)
  val left = SInt(GpuConfig.coordinateBits bits)
  val right = SInt(GpuConfig.coordinateBits bits)
  val bottom = SInt(GpuConfig.coordinateBits bits)

  def topLeft: Point2D = {
    val point = Point2D()
    point.x := this.left
    point.y := this.top
    point
  }
}

object BoundingBox {
  def apply() = new BoundingBox()
}
