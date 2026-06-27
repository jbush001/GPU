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
// This sweeps over the triangle in a fashion similar to that
// described by Pineda "A parallel algorithm for polygon rasterization"
// (SIGGRAPH 88), Figure 4. It outputs 2x2 aligned quads with one bit per pixel
// to indicate coverage.
//

package gpu

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite

class TriangleSetup() extends Bundle {
  val bbLeft = UInt(16 bits)
  val bbTop = UInt(16 bits)
  val bbRight = UInt(16 bits)
  val bbBottom = UInt(16 bits)
  val initialValue = Vec(SInt(32 bits), 3)
  val xStep = Vec(SInt(32 bits), 3)
  val yStep = Vec(SInt(32 bits), 3)
}

// The output mask is bits:
//  0 1
//  2 3
// x and y are the quad coordinates of the upper left corner
class QuadOutput() extends Bundle {
  val x = UInt(16 bits)
  val y = UInt(16 bits)
  val mask = Bits(4 bits)
}

class Rasterizer extends Component {
  val io = new Bundle {
    val input  = slave(spinal.lib.Stream(new TriangleSetup()))
    val output = master(spinal.lib.Stream(new QuadOutput()))
  }

  object StepCommand extends SpinalEnum {
    val stepReset, stepRight, stepDown, stepLeft, stepWait = newElement()
  }

  val stepCommand = StepCommand()

  val x = Reg(UInt(16 bits))
  val y = Reg(UInt(16 bits))

  // We compute the visibility of four pixels in the quad in parallel.
  val pixelCheck = B(for (pixel <- 0 until 4) yield {
    val edgeCheck = for (edge <- 0 until 3) yield {
      // The edge value represents the dot product of this point with the edge,
      // which tells us on which side of the edge it is on. If the pixel
      // is on the inside of all three triangle edges, then it is inside the triangle.
      val edgeValue = Reg(SInt(32 bits))
      switch(stepCommand) {
        is(StepCommand.stepReset) {
          pixel match {
            case 0 => edgeValue := io.input.initialValue(edge)
            case 1 => edgeValue := io.input.initialValue(edge) + io.input.xStep(edge)
            case 2 => edgeValue := io.input.initialValue(edge) + io.input.yStep(edge)
            case 3 => edgeValue := (io.input.initialValue(edge) + io.input.xStep(edge)
                + io.input.yStep(edge))
          }
        }
        is(StepCommand.stepRight) {
          edgeValue := edgeValue + (io.input.xStep(edge) << 1).resize(32 bits)
        }
        is(StepCommand.stepDown) {
          edgeValue := edgeValue + (io.input.yStep(edge) << 1).resize(32 bits)
        }
        is(StepCommand.stepLeft) {
          edgeValue := edgeValue - (io.input.xStep(edge) << 1).resize(32 bits)
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
    stepCommand := StepCommand.stepWait;

    // Waiting to start a new triangle
    IDLE.whenIsActive {
      io.input.ready := True
      when(io.input.valid) {
        stepCommand := StepCommand.stepReset
        x := io.input.bbLeft
        y := io.input.bbTop
        goto(STEP_RIGHT)
      }
    }

    STEP_RIGHT.whenIsActive {
      io.output.valid := pixelCheck =/= 0;
      when (io.output.ready) {
        when(x === io.input.bbRight) {
          when (y === io.input.bbBottom) {
            goto(IDLE)
          }.otherwise {
            stepCommand := StepCommand.stepDown;
            y := y + 1
            goto(STEP_LEFT)
          }
        }.otherwise {
          stepCommand := StepCommand.stepRight;
          x := x + 1
        }
      }
    }

    STEP_LEFT.whenIsActive {
      io.output.valid := pixelCheck =/= 0;
      when (io.output.ready) {
        when(x === io.input.bbLeft) {
          when (y === io.input.bbBottom) {
            goto(IDLE)
          }.otherwise {
            stepCommand := StepCommand.stepDown;
            y := y + 1
            goto(STEP_RIGHT)
          }
        }.otherwise {
          stepCommand := StepCommand.stepLeft;
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
    readyInterval: Int) : String = {

    val cd = dut.clockDomain.get
    cd.forkStimulus(period = 10)
    dut.io.input.valid #= false
    dut.io.output.ready #= false
    cd.waitSampling()

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
    for(cycle <- 0 until 2048) {

      // This allows us to simulate downstream hardware not being ready.
      val ready = cycle % readyInterval == 0;
      dut.io.output.ready #= ready;
      if (dut.io.output.valid.toBoolean && ready) {
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

    val sb = new StringBuilder()
    for (y <- 0 until outputBuffer.length) {
      for (x <- 0 until outputBuffer(0).length) {
        sb.append(if (outputBuffer(y)(x)) "X" else ".")
      }
      sb.append("\n") // Newline at the end of each row
    }

    sb.toString()
  }

  test("Should rasterize") {
    TestConfig.testSim.compile(new Rasterizer()).doSim { dut =>
      val output = rasterizeTriangle(dut, 8, 1, 15, 15, 1, 15, 0, 0, 7, 7, 1);
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

  test("partial left") {
    TestConfig.testSim.compile(new Rasterizer()).doSim { dut =>
      val output = rasterizeTriangle(dut, 8, 1, 15, 15, 1, 15, 0, 0, 4, 4, 1);
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
    TestConfig.testSim.compile(new Rasterizer()).doSim { dut =>
      val output = rasterizeTriangle(dut, 8, 1, 15, 15, 1, 15, 4, 4, 7, 7, 1);
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

      assert(output.trim == expected.stripMargin.trim)
    }
  }

  // Won't display anything
  test("reverse winding") {
    TestConfig.testSim.compile(new Rasterizer()).doSim { dut =>
      val output = rasterizeTriangle(dut, 8, 1, 1, 15, 15, 15, 0, 0, 7, 7, 1);
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
    TestConfig.testSim.compile(new Rasterizer()).doSim { dut =>
      val output = rasterizeTriangle(dut, 8, 1, 15, 15, 1, 15, 0, 0, 7, 7, 2);
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
