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
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite

class RasterizerSetupParams extends Bundle {
  val edgeFunctionBits = 32;
  val triangleEdges = 3;

  val bbLeft = ScreenCoord()
  val bbTop = ScreenCoord()
  val bbRight = ScreenCoord()
  val bbBottom = ScreenCoord()
  val initialValue = Vec(SInt(edgeFunctionBits bits), triangleEdges)
  val xStep = Vec(SInt(edgeFunctionBits bits), triangleEdges)
  val yStep = Vec(SInt(edgeFunctionBits bits), triangleEdges)
}

// The output mask is bits:
//  0 1
//  2 3
// x and y are the coordinates (in quads) of the upper left corner, relative to
// the left/top edges of the current tile bounding box.
class QuadOutput extends Bundle {
  val x = ScreenCoord()
  val y = ScreenCoord()
  val mask = Bits(4 bits)
}

class Rasterizer extends Component {
  val io = new Bundle {
    val input = slave(spinal.lib.Stream(new RasterizerSetupParams))
    val output = master(spinal.lib.Stream(new QuadOutput))
  }

  object StepCommand extends SpinalEnum {
    val Reset, Right, Down, Left, Wait = newElement()
  }

  val stepCommand = StepCommand()

  val inParams = RegNextWhen(io.input.payload, io.input.fire)
  val startRasterize = RegNext(io.input.fire)

  val x = Reg(ScreenCoord())
  val y = Reg(ScreenCoord())

  // We compute the visibility of four pixels in the quad in parallel.
  val pixelCheck = B(for (pixel <- 0 until 4) yield {
    val edgeCheck = for (edge <- 0 until 3) yield {
      // The edge value represents the dot product of this point with the edge,
      // which tells us on which side of the edge it is on. If the pixel
      // is on the inside of all three triangle edges, then it is inside the triangle.
      val edgeValue = Reg(SInt(32 bits))
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
          edgeValue := edgeValue + (inParams.xStep(edge) << 1).resize(32 bits)
        }
        is(StepCommand.Down) {
          edgeValue := edgeValue + (inParams.yStep(edge) << 1).resize(32 bits)
        }
        is(StepCommand.Left) {
          edgeValue := edgeValue - (inParams.xStep(edge) << 1).resize(32 bits)
        }
      }

      edgeValue(31)
    }

    edgeCheck.reduceLeft(_ & _)
  })

  // Stepping state machine. This is fairly simplistic; it sweeps the entire
  // bounding box in a zig-zag pattern.
  val stateMachine = new StateMachine {
    val IDLE = new State with EntryPoint
    val STEP_RIGHT = new State
    val STEP_LEFT = new State

    io.input.ready := False
    io.output.valid := False
    stepCommand := StepCommand.Wait;

    // Waiting to start a new triangle
    IDLE.whenIsActive {
      io.input.ready := True
      when (startRasterize) {
        stepCommand := StepCommand.Reset
        x := io.input.bbLeft
        y := io.input.bbTop
        goto(STEP_RIGHT)
      }
    }

    STEP_RIGHT.whenIsActive {
      io.output.valid := pixelCheck =/= 0;
      when (io.output.ready) {
        when(x === inParams.bbRight) {
          when (y === inParams.bbBottom) {
            goto(IDLE)
          }.otherwise {
            stepCommand := StepCommand.Down;
            y := y + 1
            goto(STEP_LEFT)
          }
        }.otherwise {
          stepCommand := StepCommand.Right;
          x := x + 1
        }
      }
    }

    STEP_LEFT.whenIsActive {
      io.output.valid := pixelCheck =/= 0;
      when (io.output.ready) {
        when(x === inParams.bbLeft) {
          when (y === inParams.bbBottom) {
            goto(IDLE)
          }.otherwise {
            stepCommand := StepCommand.Down;
            y := y + 1
            goto(STEP_RIGHT)
          }
        }.otherwise {
          stepCommand := StepCommand.Left;
          x := x - 1
        }
      }
    }
  }

  io.output.x := x
  io.output.y := y
  io.output.mask := pixelCheck
}

class RasterizerSpec extends AnyFunSuite {
  val compiledModel = TestConfig.testSim.compile(new Rasterizer())

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

    SimTimeout(1000000)

    dut.clockDomain.forkStimulus(period = 10)
    dut.io.input.valid #= false
    dut.io.output.ready #= false
    dut.clockDomain.waitSampling() // Wait for reset to complete

    dut.io.output.ready #= true
    dut.clockDomain.waitSampling()

    dut.io.input.payload.bbLeft #= bbLeft
    dut.io.input.payload.bbTop #= bbTop
    dut.io.input.payload.bbRight #= bbRight
    dut.io.input.payload.bbBottom #= bbBottom

    // Edge 0->1
    dut.io.input.payload.xStep(0) #= y1 - y0
    dut.io.input.payload.yStep(0) #= x0 - x1
    dut.io.input.payload.initialValue(0) #= ((bbLeft * 2) - x0) * (y1 - y0) - ((bbTop * 2) - y0) * (x1 - x0)

    // Edge 1->2
    dut.io.input.payload.xStep(1) #= y2 - y1
    dut.io.input.payload.yStep(1) #= x1 - x2
    dut.io.input.payload.initialValue(1) #= ((bbLeft * 2) - x1) * (y2 - y1) - ((bbTop * 2) - y1) * (x2 - x1)

    // Edge 2->0
    dut.io.input.payload.xStep(2) #= y0 - y2
    dut.io.input.payload.yStep(2) #= x2 - x0
    dut.io.input.payload.initialValue(2) #= ((bbLeft * 2) - x2) * (y0 - y2) - ((bbTop * 2) - y2) * (x0 - x2)
    dut.io.input.valid #= true

    while(!dut.io.input.ready.toBoolean) {
      dut.clockDomain.waitSampling()
    }

    dut.clockDomain.waitSampling()
    dut.io.input.valid #= false

    val outputBuffer = Array.ofDim[Boolean]((bbRight + 1) * 2, (bbBottom + 1) * 2);

    val rng = new Random(42)
    dut.io.output.ready #= true
    for (_ <- 0 until 2048) {
      if (randomizeReady) {
        dut.io.output.ready #= rng.nextBoolean()
      }

      if (dut.io.output.valid.toBoolean && dut.io.output.ready.toBoolean) {
        assert(!dut.io.input.ready.toBoolean)
        val x = dut.io.output.x.toInt
        val y = dut.io.output.y.toInt
        assert(x >= bbLeft)
        assert(y >= bbTop)
        assert(x <= bbRight)
        assert(y <= bbBottom)
        val mask = dut.io.output.mask.toInt
        if ((mask & 1) != 0) outputBuffer(y * 2)(x * 2) = true
        if ((mask & 2) != 0) outputBuffer(y * 2)(x * 2 + 1) = true
        if ((mask & 4) != 0) outputBuffer(y * 2 + 1)(x * 2) = true
        if ((mask & 8) != 0) outputBuffer(y * 2 + 1)(x * 2 + 1) = true
      }

      dut.clockDomain.waitSampling()
    }

    assert(dut.io.input.ready.toBoolean)
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
    compiledModel.doSim { dut =>
      val output = rasterizeTriangle(dut, 8, 1, 15, 15, 1, 15, 0, 0, 7, 7, false);
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

      assert(output.filterNot(_.isWhitespace) == expected.filterNot(_.isWhitespace))
    }
  }

  test("partial left") {
    compiledModel.doSim { dut =>
      val output = rasterizeTriangle(dut, 8, 1, 15, 15, 1, 15, 0, 0, 4, 4, false);
      val expected = """..........
..........
........X.
........X.
.......XXX
.......XXX
......XXXX
......XXXX
.....XXXXX
.....XXXXX"""
      assert(output.trim == expected.stripMargin.trim)
    }
  }

  test("partial right") {
    compiledModel.doSim { dut =>
      val output = rasterizeTriangle(dut, 8, 1, 15, 15, 1, 15, 4, 4, 7, 7, false);
      val expected =  """................
................
................
................
................
................
................
................
........XXXX....
........XXXX....
........XXXXX...
........XXXXX...
........XXXXXX..
........XXXXXX..
........XXXXXXX.
................"""

      assert(output.filterNot(_.isWhitespace) == expected.filterNot(_.isWhitespace))
    }
  }

  // Won't display anything
  test("reverse winding") {
    compiledModel.doSim { dut =>
      val output = rasterizeTriangle(dut, 8, 1, 1, 15, 15, 15, 0, 0, 7, 7, false);
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
    compiledModel.doSim { dut =>
      val output = rasterizeTriangle(dut, 8, 1, 15, 15, 1, 15, 0, 0, 7, 7, true);
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
