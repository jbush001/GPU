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
// This computes the edge equation coefficients used by the rasterizer.
// It takes 28 cycles to set up the next triangle. This has an output buffer
// so the computation for the next triangle overlaps rasterization
// of the current one.
// To optimize area, this runs a small microsequencer that performs the
// calculations (rather than just doing it all in parallel).
//

package gpu

import spinal.core._
import spinal.lib._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import scala.collection.mutable.ListBuffer

class TriangleSetupParams extends Bundle {
    val bbLeft = ScreenCoord()
    val bbTop = ScreenCoord()
    val bbRight = ScreenCoord()
    val bbBottom = ScreenCoord()
    val x0 = ScreenCoord()
    val y0 = ScreenCoord()
    val x1 = ScreenCoord()
    val y1 = ScreenCoord()
    val x2 = ScreenCoord()
    val y2 = ScreenCoord()
}

// TODO some registers can be used as sources, some as destinations, some both.
// I don't check that here for simplicity, but there are a number of ways to do
// this at either compile or runtime.
object MicrocodeCompiler {
  class Program {
    val uops = ListBuffer[Int]()
  }

  sealed trait Expression
  case class SubExpr(op1: Operand, op2: Operand) extends Expression
  case class MulExpr(op1: Operand, op2: Operand) extends Expression

  sealed abstract class Operand(val index: Int) {
    def -(op2: Operand): Expression = SubExpr(this, op2)
    def *(op2: Operand): Expression = MulExpr(this, op2)
    def :=(expr: Expression)(implicit program: Program) = {
      expr match {
        case SubExpr(op1, op2) => program.uops += packInstruction(0, this.index, op1.index, op2.index)
        case MulExpr(op1, op2) => program.uops += packInstruction(1, this.index, op1.index, op2.index)
      }
    }
  }

  case object temp0 extends Operand(1)
  case object temp1 extends Operand(2)
  case object temp2 extends Operand(3)
  case object x0 extends Operand(4)
  case object x1 extends Operand(5)
  case object x2 extends Operand(6)
  case object y0 extends Operand(7)
  case object y1 extends Operand(8)
  case object y2 extends Operand(9)
  case object bbleft extends Operand(10)
  case object bbtop extends Operand(11)
  case object xs0 extends Operand(4)
  case object ys0 extends Operand(5)
  case object iv0 extends Operand(6)
  case object xs1 extends Operand(7)
  case object ys1 extends Operand(8)
  case object iv1 extends Operand(9)
  case object xs2 extends Operand(10)
  case object ys2 extends Operand(11)
  case object iv2 extends Operand(12)

  def packInstruction(op: Int, dest: Int, op1: Int, op2: Int): Int = {
    return ((op << 12) | (dest << 8) | (op1 << 4) | op2)
  }

  def assemble(function: Program => Unit): List[Int] = {
    val program = new Program()
    function(program)
    program.uops += (1 << 13) // Set halt bit
    program.uops.toList
  }
}

class TriangleSetup extends Component {
  val io = new Bundle {
    val input = slave(spinal.lib.Stream(new TriangleSetupParams))
    val output = master(spinal.lib.Stream(new RasterizerSetupParams))
  }

  // Register incoming parameters
  val inParams = RegNextWhen(io.input.payload, io.input.fire)

  // Here is where we accumulate values as we compute them
  val setupResult = Reg(new RasterizerSetupParams())
  val outputResultPending = Reg(Bool()) init(False)

  val internalStream = Stream(new RasterizerSetupParams())
  internalStream.payload := setupResult
  internalStream.valid := outputResultPending

  // This adds an additional buffer, which lets us get one triangle ahead
  // of the rasterizer, overlapping execution.
  io.output << internalStream.stage()

  val halt = Bool()
  val computing = Reg(Bool()) init(False)
  io.input.ready := !computing && !outputResultPending

  // This state machine handles start and finishing computations and
  // handshaking with upstream and downstream components.
  when (computing) {
    when (halt) {
      outputResultPending := True
      computing := False
    }
  } otherwise {
    when (io.input.fire) {
      setupResult.bbLeft := io.input.bbLeft
      setupResult.bbRight := io.input.bbRight
      setupResult.bbTop := io.input.bbTop
      setupResult.bbBottom := io.input.bbBottom
      computing := True
    }
  }

  when (internalStream.fire) {
    outputResultPending := False
  }


  val microcode = MicrocodeCompiler.assemble { implicit program =>
    import MicrocodeCompiler._

    temp0 := bbleft - x0
    temp1 := y1 - y0
    temp2 := temp0 * temp1
    temp0 := bbtop - y0
    temp1 := x1 - x0
    temp0 := temp0 * temp1
    iv0 := temp2 - temp0
    xs0 := y1 - y0
    ys0 := x0 - x1

    temp0 := bbleft - x1
    temp1 := y2 - y1
    temp2 := temp0 * temp1
    temp0 := bbtop - y1
    temp1 := x2 - x1
    temp0 := temp0 * temp1
    iv1 := temp2 - temp0
    xs1 := y2 - y1
    ys1 := x1 - x2

    temp0 := bbleft - x2
    temp1 := y0 - y2
    temp2 := temp0 * temp1
    temp0 := bbtop - y2
    temp1 := x0 - x2
    temp0 := temp0 * temp1
    iv2 := temp2 - temp0
    xs2 := y0 - y2
    ys2 := x2 - x0
  }

  // Control path
  val upc = Reg(UInt(log2Up(microcode.length) bits)) init(0)
  val microcodeRom = Mem(UInt(18 bits), initialContent = microcode.map(x => U(x, 18 bits)))
  val uInst = microcodeRom(upc)
  halt := uInst(13)
  val operation = uInst(12)
  val destSelect = uInst(11 downto 8)
  val aSelect = uInst(7 downto 4)
  val bSelect = uInst(3 downto 0)

  when (computing) {
    upc := upc + 1
  } otherwise {
    upc := 0
  }

  // Scratchpad registers for interim results.
  val acc0 = Reg(SInt(32 bits))
  val acc1 = Reg(SInt(32 bits))
  val acc2 = Reg(SInt(32 bits))

  // Source operand multiplexers
  val operands = Vec(
    S(0, 16 bits),
    acc0,
    acc1,
    acc2,
    inParams.x0.resize(32),
    inParams.x1.resize(32),
    inParams.x2.resize(32),
    inParams.y0.resize(32),
    inParams.y1.resize(32),
    inParams.y2.resize(32),
    (inParams.bbLeft << 1).resize(32),
    (inParams.bbTop << 1).resize(32)
  )

  val operand1 = operands(aSelect)
  val operand2 = operands(bSelect)

  val result = Mux(operation, (operand1(15 downto 0) * operand2(15 downto 0)), operand1 - operand2)

  when (destSelect === 1) { acc0 := result }
  when (destSelect === 2) { acc1 := result }
  when (destSelect === 3) { acc2 := result }
  when (destSelect === 4) { setupResult.xStep(0) := result }
  when (destSelect === 5) { setupResult.yStep(0) := result }
  when (destSelect === 6) { setupResult.initialValue(0) := result }
  when (destSelect === 7) { setupResult.xStep(1) := result }
  when (destSelect === 8) { setupResult.yStep(1) := result }
  when (destSelect === 9) { setupResult.initialValue(1) := result }
  when (destSelect === 10) { setupResult.xStep(2) := result }
  when (destSelect === 11) { setupResult.yStep(2) := result }
  when (destSelect === 12) { setupResult.initialValue(2) := result }
}

class TriangleSetupSpec extends AnyFunSuite {
  val compiledModel = TestConfig.testSim.compile(new TriangleSetup())

  def computeExpectedValues(bbLeft: Int, bbTop: Int,
                            x0: Int, y0: Int, x1: Int, y1: Int, x2: Int, y2: Int) = {
    val xStep0 = y1 - y0
    val yStep0 = x0 - x1
    val initialValue0 = ((bbLeft * 2) - x0) * (y1 - y0) - ((bbTop * 2) - y0) * (x1 - x0)

    val xStep1 = y2 - y1
    val yStep1 = x1 - x2
    val initialValue1 = ((bbLeft * 2) - x1) * (y2 - y1) - ((bbTop * 2) - y1) * (x2 - x1)

    val xStep2 = y0 - y2
    val yStep2 = x2 - x0
    val initialValue2 = ((bbLeft * 2) - x2) * (y0 - y2) - ((bbTop * 2) - y2) * (x0 - x2)

    (xStep0, yStep0, initialValue0,
      xStep1, yStep1, initialValue1,
      xStep2, yStep2, initialValue2)
  }

  // TODO test input/output handshaking, queuing up multiple requests.

  test("triangle setup") {
    compiledModel.doSim { dut =>
        SimTimeout(1000000)
        dut.clockDomain.forkStimulus(period = 10)

        dut.io.input.valid #= false

        dut.clockDomain.waitSampling()

        val bbLeft = 32
        val bbTop = 96
        val bbRight = 64
        val bbBottom = 128
        val x0 = 47
        val y0 = 88
        val x1 = 59
        val y1 = 110
        val x2 = 33
        val y2 = 107

        dut.io.input.valid #= true
        dut.io.input.bbLeft #= bbLeft
        dut.io.input.bbTop #= bbTop
        dut.io.input.bbRight #= bbRight
        dut.io.input.bbBottom #= bbBottom
        dut.io.input.x0 #= x0
        dut.io.input.y0 #= y0
        dut.io.input.x1 #= x1
        dut.io.input.y1 #= y1
        dut.io.input.x2 #= x2
        dut.io.input.y2 #= y2

        while (!dut.io.input.ready.toBoolean) {
          dut.clockDomain.waitSampling()
        }

        // Wait for a clock edge to latch it.
        dut.clockDomain.waitSampling()

        // Clear the inputs to ensure it has latched them properly.
        dut.io.input.valid #= false
        dut.io.input.bbLeft #= 0
        dut.io.input.bbTop #= 0
        dut.io.input.bbRight #= 0
        dut.io.input.bbBottom #= 0
        dut.io.input.x0 #= 0
        dut.io.input.y0 #= 0
        dut.io.input.x1 #= 0
        dut.io.input.y1 #= 0
        dut.io.input.x2 #= 0
        dut.io.input.y2 #= 0

        while (!dut.io.output.valid.toBoolean) {
          dut.clockDomain.waitSampling()
        }

        assert(dut.io.output.valid.toBoolean)

        val (xStep0, yStep0, initialValue0,
          xStep1, yStep1, initialValue1,
          xStep2, yStep2, initialValue2) = computeExpectedValues(bbLeft, bbTop,
            x0, y0, x1, y1, x2, y2)
        assert(dut.io.output.xStep(0).toInt == xStep0)
        assert(dut.io.output.yStep(0).toInt == yStep0)
        assert(dut.io.output.initialValue(0).toInt == initialValue0)
        assert(dut.io.output.xStep(1).toInt == xStep1)
        assert(dut.io.output.yStep(1).toInt == yStep1)
        assert(dut.io.output.initialValue(1).toInt == initialValue1)
        assert(dut.io.output.xStep(2).toInt == xStep2)
        assert(dut.io.output.yStep(2).toInt == yStep2)
        assert(dut.io.output.initialValue(2).toInt == initialValue2)
        assert(dut.io.output.bbLeft.toInt == bbLeft)
        assert(dut.io.output.bbTop.toInt == bbTop)
        assert(dut.io.output.bbRight.toInt == bbRight)
        assert(dut.io.output.bbBottom.toInt == bbBottom)
    }
  }
}
