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

//
// Given a triangle, the rasterizer determines which pixels it covers.
// This sweeps over the triangle in a fashion similar to that described by
// Pineda "A parallel algorithm for polygon rasterization" (SIGGRAPH 88).
// It outputs 2x2 aligned quads with one bit per pixel to indicate coverage.
//
// TODO
// - Support MSAA, which would return an N Bit mask for each of the quads.
// - This is inefficient in some cases because it sweeps over the entire tile
//   rather than to the edges of the triangle or recursing.
//

package gpu

import scala.util.Random
import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funsuite.AnyFunSuite

object Consts {
  val triangleEdges = 3
  val pixelsPerQuad = 4
}

class RasterizerSetupParams extends Bundle {
  val boundingBox = BoundingBox()
  val initialValue = Vec(Consts.triangleEdges, SInt(GpuConfig.edgeFunctionBits.W))
  val xStep = Vec(Consts.triangleEdges, SInt(GpuConfig.edgeFunctionBits.W))
  val yStep = Vec(Consts.triangleEdges, SInt(GpuConfig.edgeFunctionBits.W))
}

// The output mask has one bit per pixel in the quad that represents covered
// pixels:
//  0 1
//  2 3
// location contains the coordinates of the upper left corner, relative to
// the left/top edges of the current tile bounding box.
//
// The lambda outputs contain unnormalized barycentric coordindates for the
// first two vertices (the third can be inferred from the others, since they
// always add up to the same value). These is used to interpolate varyings
// across the triangle.
//
class QuadOutput extends Bundle {
  val location = Point2D()
  val mask = Bits(Consts.pixelsPerQuad.W)
  val lambda = Vec(Consts.pixelsPerQuad, Vec(Consts.triangleEdges, SInt(32.W)))
}

class Rasterizer extends Module {
  val io = IO(new Bundle {
    val input = Flipped(Decoupled(new RasterizerSetupParams))
    val output = Decoupled(new QuadOutput)
  })

  object StepCommand extends ChiselEnum {
    val Reset, Right, Down, Left, Wait = Value
  }

  val stepCommand = Wire(StepCommand())

  val inParams = RegEnable(io.input.bits, io.input.fire)
  val startRasterize = RegNext(io.input.fire)

  val quadLoc = Reg(Point2D())

  // We compute the visibility of four pixels in the quad in parallel.
  val pixelCheck = Cat((for (pixel <- 0 until Consts.pixelsPerQuad) yield {
    val edgeCheck = for (edge <- 0 until Consts.triangleEdges) yield {
      // The edge value represents the dot product of this point with the edge,
      // which tells us on which side of the edge it is on. If the pixel
      // is on the inside of all three triangle edges, then it is inside the triangle.
      val edgeValue = Reg(SInt(32.W))

      io.output.bits.lambda(pixel)(edge) := edgeValue

      switch(stepCommand) {
        is(StepCommand.Reset) {
          pixel match {
            case 0 => edgeValue := inParams.initialValue(edge)
            case 1 => edgeValue := inParams.initialValue(edge) + inParams.xStep(edge)
            case 2 => edgeValue := inParams.initialValue(edge) + inParams.yStep(edge)
            case 3 => edgeValue := (inParams.initialValue(edge) + inParams.xStep(edge)
                + inParams.yStep(edge))
          }
        }
        is(StepCommand.Right) {
          edgeValue := edgeValue + (inParams.xStep(edge) << 1).pad(32)
        }
        is(StepCommand.Down) {
          edgeValue := edgeValue + (inParams.yStep(edge) << 1).pad(32)
        }
        is(StepCommand.Left) {
          edgeValue := edgeValue - (inParams.xStep(edge) << 1).pad(32)
        }
      }
 
      edgeValue(31)
    }

    edgeCheck.reduceLeft(_ & _)
  }).reverse)

  object State extends ChiselEnum {
    val Idle, StepRight, StepLeft = Value
  }

  val stateReg = RegInit(State.Idle)



  // Stepping state machine. This is fairly simplistic; it sweeps the entire
  // bounding box in a zig-zag pattern.
  io.input.ready := false.B 
  io.output.valid := false.B
  stepCommand := StepCommand.Wait;
  switch (stateReg) {
    // Waiting to start a new triangle
    is (State.Idle) {
      io.input.ready := true.B
      when (startRasterize) {
        stepCommand := StepCommand.Reset
        quadLoc := inParams.boundingBox.topLeft
        stateReg := State.StepRight
      } otherwise {
        stepCommand := StepCommand.Wait
      }
    }

    is (State.StepRight) {
      io.output.valid := pixelCheck =/= 0.U;
      when (io.output.ready) {
        when (quadLoc.x === inParams.boundingBox.right) {
          when (quadLoc.y === inParams.boundingBox.bottom) {
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
      io.output.valid := pixelCheck =/= 0.U
      when (io.output.ready) {
        when(quadLoc.x === inParams.boundingBox.left) {
          when (quadLoc.y === inParams.boundingBox.bottom) {
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
  io.output.bits.location := quadLoc - inParams.boundingBox.topLeft
  io.output.bits.mask := pixelCheck
}

class RasterizerTests extends AnyFunSuite with ChiselSim {
  def rasterizeTriangle(dut: Rasterizer,
    x0: Int,
    y0: Int,
    x1: Int,
    y1: Int,
    x2: Int,
    y2: Int,
    bbLeft: Int,
    bbTop: Int,
    bbRight: Int,
    bbBottom: Int,
    randomizeReady: Boolean) : String = {

    dut.io.input.valid.poke(false)
    dut.io.output.ready.poke(false)
    dut.clock.step() // Wait for reset to complete

    dut.io.output.ready.poke(true)
    dut.clock.step()

    dut.io.input.bits.boundingBox.left.poke(bbLeft)
    dut.io.input.bits.boundingBox.top.poke(bbTop)
    dut.io.input.bits.boundingBox.right.poke(bbRight)
    dut.io.input.bits.boundingBox.bottom.poke(bbBottom)

    // Edge 0->1
    dut.io.input.bits.xStep(0).poke(y1 - y0)
    dut.io.input.bits.yStep(0).poke(x0 - x1)
    dut.io.input.bits.initialValue(0).poke((bbLeft - x0) * (y1 - y0) - (bbTop - y0) * (x1 - x0))

    // Edge 1->2
    dut.io.input.bits.xStep(1).poke(y2 - y1)
    dut.io.input.bits.yStep(1).poke(x1 - x2)
    dut.io.input.bits.initialValue(1).poke((bbLeft - x1) * (y2 - y1) - (bbTop - y1) * (x2 - x1))

    // Edge 2->0
    dut.io.input.bits.xStep(2).poke(y0 - y2)
    dut.io.input.bits.yStep(2).poke(x2 - x0)
    dut.io.input.bits.initialValue(2).poke((bbLeft - x2) * (y0 - y2) - (bbTop - y2) * (x0 - x2))
    dut.io.input.valid.poke(true)

    while (dut.io.input.ready.peek().litValue.toLong == 0) {
      dut.clock.step()
    }

    dut.clock.step()
    dut.io.input.valid.poke(false)

    val outputBuffer = Array.ofDim[Boolean](bbRight - bbLeft + 2, bbBottom - bbTop + 2);

    val rng = new Random(42)
    dut.io.output.ready.poke(true)
    for (_ <- 0 until 2048) {
      if (randomizeReady) {
        dut.io.output.ready.poke(rng.nextBoolean())
      }

      if (dut.io.output.valid.peek().litValue.toLong != 0  
        && dut.io.output.ready.peek().litValue.toLong != 0) {
        dut.io.input.ready.expect(0)
        val x = dut.io.output.bits.location.x.peek().litValue.toInt
        val y = dut.io.output.bits.location.y.peek().litValue.toInt
        assert(x <= (bbRight - bbLeft))
        assert(y <= (bbBottom - bbTop))
        val mask = dut.io.output.bits.mask.peek().litValue.toLong
        if ((mask & 1) != 0) outputBuffer(y)(x) = true
        if ((mask & 2) != 0) outputBuffer(y)(x + 1) = true
        if ((mask & 4) != 0) outputBuffer(y + 1)(x) = true
        if ((mask & 8) != 0) outputBuffer(y + 1)(x + 1) = true
      }

      dut.clock.step()
    }

    dut.io.input.ready.expect(1)
    val sb = new StringBuilder()
    for (y <- 0 until outputBuffer.length) {
      for (x <- 0 until outputBuffer(0).length) {
        sb.append(if (outputBuffer(y)(x)) "X" else ".")
      }
      sb.append("\n") // Newline at the end of each row
    }

    sb.toString()
  }

  test("rasterize") {
    simulate(new Rasterizer()) { dut =>
      val output = rasterizeTriangle(dut, 8, 1, 15, 15, 1, 15, 0, 0, 14, 14, false);
      val expected = """
................
................
........X.......
........X.......
.......XXX......
.......XXX......
......XXXXX.....
......XXXXX.....
.....XXXXXXX....
.....XXXXXXX....
....XXXXXXXXX...
....XXXXXXXXX...
...XXXXXXXXXXX..
...XXXXXXXXXXX..
..XXXXXXXXXXXXX.
................"""

      assert(output.filterNot(_.isWhitespace) == expected.filterNot(_.isWhitespace))
    }
  }

  test("clip right") {
    simulate(new Rasterizer()) { dut =>
      val output = rasterizeTriangle(dut, 8, 1, 15, 15, 1, 15, 0, 0, 6, 6, false);
      val expected = """
........
........
........
........
.......X
.......X
......XX
......XX"""
      assert(output.trim == expected.stripMargin.trim)
    }
  }

  // Test non-zero bounding box offset
  test("clip left") {
    simulate(new Rasterizer()) { dut =>
      val output = rasterizeTriangle(dut, 16, 1, 22, 22, 1, 16, 16, 16, 22, 22, false);
      val expected =  """
XXXXX...
XXXXX...
XXXXX...
XXXXXX..
XXXXXX..
...XXX..
........
........"""

      assert(output.filterNot(_.isWhitespace) == expected.filterNot(_.isWhitespace))
    }
  }

  // Won't display anything
  test("reverse winding") {
    simulate(new Rasterizer()) { dut =>
      val output = rasterizeTriangle(dut, 8, 1, 1, 15, 15, 15, 0, 0, 14, 14, false);
      val expected =  """................
................
................
................
................
................
................
................
................
................
................
................
................
................
................
................"""
      assert(output.trim == expected.stripMargin.trim)
    }
  }

  test("output flow control") {
    simulate(new Rasterizer()) { dut =>
      val output = rasterizeTriangle(dut, 8, 1, 15, 15, 1, 15, 0, 0, 14, 14, true);
      val expected = """................
................
........X.......
........X.......
.......XXX......
.......XXX......
......XXXXX.....
......XXXXX.....
.....XXXXXXX....
.....XXXXXXX....
....XXXXXXXXX...
....XXXXXXXXX...
...XXXXXXXXXXX..
...XXXXXXXXXXX..
..XXXXXXXXXXXXX.
................"""

      assert(output.trim == expected.stripMargin.trim)
    }
  }
}
