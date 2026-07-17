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

/** Represents a point in integer pixel raster coordinates on the screen.
  * The origin (0, 0) is the upper left corner. Coordinate values increase
  * to the right (x-axis) and down (y-axis)
  */
class Point2D extends Bundle {
  val x = SInt(GpuConfig.coordinateBits.W)
  val y = SInt(GpuConfig.coordinateBits.W)

  /** Component-wise subtraction, returning a vector this - that */
  def -(that: Point2D): Point2D = {
    val result = Wire(new Point2D)
    result.x := this.x - that.x
    result.y := this.y - that.y
    result
  }
}

object Point2D {
  def apply() = new Point2D()
}

/** Represents a rectangular region on the screen, using the same coordinate 
  * convention as [[Point2D]]. 
  * All boundary coordinates are inclusive.
  */
class BoundingBox extends Bundle {
  val top = SInt(GpuConfig.coordinateBits.W)
  val left = SInt(GpuConfig.coordinateBits.W)
  val right = SInt(GpuConfig.coordinateBits.W)
  val bottom = SInt(GpuConfig.coordinateBits.W)

  def topLeft: Point2D = {
    val point = Wire(new Point2D)
    point.x := this.left
    point.y := this.top
    point
  }
}

object BoundingBox {
  def apply() = new BoundingBox()
}
