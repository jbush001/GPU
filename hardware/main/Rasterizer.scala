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

class EdgeCoeffs(implicit cfg: GpuConfig) extends Bundle {
  val boundingBox = BoundingBox()
  val initialValue = Vec(Consts.triangleEdges, SInt(cfg.edgeFunctionBits.W))
  val xStep = Vec(Consts.triangleEdges, SInt(cfg.edgeFunctionBits.W))
  val yStep = Vec(Consts.triangleEdges, SInt(cfg.edgeFunctionBits.W))
}

/** Contains coverage and interpolation data for a single 2x2 pixel quad. */
class RasterizedQuad(implicit cfg: GpuConfig) extends Bundle {
  /** Coordinates of the upper left corner, relative to the left/top edges
    * of the current tile bounding box.
    */
  val location = Point2D()

  /** Indicates which pixels are covered, with one bit per pixel using the
    * following layout:
    *
    *     0 1
    *     2 3
    */
  val mask = Bits(Consts.pixelsPerQuad.W)

  /** Normalized barycentric coordindates of the pixels relative to the
    * triangle vertices. The third coordinate is omitted as it can be
    * derived from the first two (all three sum to 1.0)
    * [[https://en.wikipedia.org/wiki/Barycentric_coordinate_system]]
    */
  val lambda = Vec(Consts.pixelsPerQuad, Vec(2, Float32()))
}

/** Determines pixel coverage for a triangle.
  *
  * This sweeps over the triangle using an approach based on Pineda "A parallel
  * algorithm for polygon rasterization" (SIGGRAPH 88). It outputs 2x2 aligned
  * quads with one bit per pixel to indicate coverage.
  *
  * @todo Support MSAA, which would return an N-bit mask for each of the quads.
  * @todo Optimize the sweep algorithm; it currently scans the entire tile.
  */
class Rasterizer(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val edgeCoeffs = Flipped(Decoupled(new EdgeCoeffs))
    val quad = Decoupled(new RasterizedQuad)
    val complete = Output(Bool())
  })

  object StepCommand extends ChiselEnum {
    val Reset, Right, Down, Left, Wait = Value
  }

  val stepCommand = Wire(StepCommand())

  val inCoeffs = RegEnable(io.edgeCoeffs.bits, io.edgeCoeffs.fire)
  val startRasterize = RegNext(io.edgeCoeffs.fire)

  val quadLoc = Reg(Point2D())

  // We compute the visibility of four pixels in the quad in parallel.
  val pixelCheck = Cat((for (pixel <- 0 until Consts.pixelsPerQuad) yield {
    val edgeCheck = for (edge <- 0 until Consts.triangleEdges) yield {
      // The edge value represents the dot product of this point with the edge,
      // which tells us on which side of the edge it is on. If the pixel
      // is on the inside of all three triangle edges, then it is inside the triangle.
      val edgeValue = Reg(SInt(cfg.edgeFunctionBits.W))

      if (edge == 0 || edge == 2) {
        // These are 16_16 fixed point values by convention from the setup stage.
        io.quad.bits.lambda(pixel)(if (edge == 0) 1 else 0) := Float32.fromFixedPoint(edgeValue, 16)
      }

      switch(stepCommand) {
        is(StepCommand.Reset) {
          pixel match {
            case 0 => edgeValue := inCoeffs.initialValue(edge)
            case 1 => edgeValue := inCoeffs.initialValue(edge) + inCoeffs.xStep(edge)
            case 2 => edgeValue := inCoeffs.initialValue(edge) + inCoeffs.yStep(edge)
            case 3 => edgeValue := (inCoeffs.initialValue(edge) + inCoeffs.xStep(edge)
                + inCoeffs.yStep(edge))
          }
        }
        is(StepCommand.Right) {
          edgeValue := edgeValue + (inCoeffs.xStep(edge) << 1.U).tail(1).asSInt
        }
        is(StepCommand.Down) {
          edgeValue := edgeValue + (inCoeffs.yStep(edge) << 1.U).tail(1).asSInt
        }
        is(StepCommand.Left) {
          edgeValue := edgeValue - (inCoeffs.xStep(edge) << 1.U).tail(1).asSInt
        }
      }

      edgeValue >= 0.S
    }

    edgeCheck.reduceLeft(_ & _)
  }).reverse)

  object State extends ChiselEnum {
    val Idle, StepRight, StepLeft = Value
  }

  val stateReg = RegInit(State.Idle)
  io.complete := (stateReg === State.Idle)

  // Stepping state machine. This is fairly simplistic; it sweeps the entire
  // bounding box in a zig-zag pattern.
  io.edgeCoeffs.ready := false.B
  io.quad.valid := false.B
  stepCommand := StepCommand.Wait;
  switch (stateReg) {
    // Waiting to start a new triangle
    is (State.Idle) {
      io.edgeCoeffs.ready := true.B
      when (startRasterize) {
        stepCommand := StepCommand.Reset
        quadLoc := inCoeffs.boundingBox.topLeft
        stateReg := State.StepRight
      } otherwise {
        stepCommand := StepCommand.Wait
      }
    }

    is (State.StepRight) {
      io.quad.valid := pixelCheck =/= 0.U;
      when (io.quad.ready) {
        when (quadLoc.x === inCoeffs.boundingBox.right) {
          when (quadLoc.y === inCoeffs.boundingBox.bottom) {
            stateReg := State.Idle
          }.otherwise {
            stepCommand := StepCommand.Down;
            quadLoc.y := quadLoc.y + 2.S
            stateReg := State.StepLeft
          }
        }.otherwise {
          stepCommand := StepCommand.Right;
          quadLoc.x := quadLoc.x + 2.S
        }
      }
    }

   is (State.StepLeft) {
      io.quad.valid := pixelCheck =/= 0.U
      when (io.quad.ready) {
        when(quadLoc.x === inCoeffs.boundingBox.left) {
          when (quadLoc.y === inCoeffs.boundingBox.bottom) {
            stateReg := State.Idle
          }.otherwise {
            stepCommand := StepCommand.Down
            quadLoc.y := quadLoc.y + 2.S
            stateReg := State.StepRight
          }
        }.otherwise {
          stepCommand := StepCommand.Left
          quadLoc.x := quadLoc.x - 2.S
        }
      }
    }
  }

  // Coordinates need to be relative to bounding box.
  io.quad.bits.location := quadLoc - inCoeffs.boundingBox.topLeft
  io.quad.bits.mask := pixelCheck
}
