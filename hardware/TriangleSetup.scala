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
// This has an output buffer so the computation for the next triangle
// overlaps rasterization of the current one.
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
}

class UInst extends Bundle {
  val opcode = UInt(1 bits)
  val dest = UInt(4 bits)
  val src1 = UInt(4 bits)
  val src2 = UInt(4 bits)
  val readEn = Bool()
  val readAddress = UInt(4 bits)
  val halt = Bool()
}

object UInst {
  def apply(opcode: Int, dest: Int, src1: Int, src2: Int, readEn: Boolean = false, readAddress: Int = 0, halt: Boolean = false): UInst = {
    val result = new UInst()
    result.opcode := opcode
    result.dest := dest
    result.src1 := src1
    result.src2 := src2
    result.readEn := Bool(readEn)
    result.readAddress := readAddress
    result.halt := Bool(halt)
    result
  }

  def apply() = new UInst()
}

// TODO some registers can be used as sources, some as destinations, some both.
// I don't check that here for simplicity, but there are a number of ways to do
// this at either compile or runtime.
object MicrocodeCompiler {
  type Inst = (Int, Int, Int, Int, Boolean, Int, Boolean)

  class Program {
    val uops = ListBuffer[Inst]()
    uops += ((0, 0, 0, 0, false, 0, false)) // no-op

    def addLoadOp(address: Int) = {
      // We need to patch the previous instruction to issue the load, since
      // there is one cycle of latency.
      if (uops.length == 0) {
        throw new Exception("cannot issue load as first instruction")
      }

      uops(uops.length - 1) = uops.last.copy(_5 = true, _6 = address)
    }
  }

  sealed trait Expression
  case class SubExpr(op1: Operand, op2: Operand) extends Expression
  case class MulExpr(op1: Operand, op2: Operand) extends Expression
  case class LoadExpr(op1: MemOperand) extends Expression

  sealed trait Operand {
    def -(op2: Operand): Expression = SubExpr(this, op2)
    def *(op2: Operand): Expression = MulExpr(this, op2)
  }

  sealed abstract class RegOperand(val index: Int) extends Operand {
    def :=(expr: Expression)(implicit program: Program) = {
      val (opcode, op1, op2) = expr match {
        case SubExpr(o1, o2) => (0, o1, o2)
        case MulExpr(o1, o2) => (1, o1, o2)
        case LoadExpr(o1) => (0, o1, zero)
      }

      (op1, op2) match {
        case (RegOperand(a), RegOperand(b)) => {
          program.uops += ((opcode, this.index, a, b, false, 0, false))
        }

        case (RegOperand(a), MemOperand(b)) => {
          program.addLoadOp(b)
          program.uops += ((opcode, this.index, a, 15, false, 0, false))
        }

        case (MemOperand(a), RegOperand(b)) => {
          program.addLoadOp(a)
          program.uops += ((opcode, this.index, 15, b, false, 0, false))
        }

        case _ => throw new IllegalArgumentException("Unsupported configuration")
      }
    }

    def :=(mem: MemOperand)(implicit program: Program): Unit = this := LoadExpr(mem)
  }

  object RegOperand {
    def unapply(op: RegOperand): Option[Int] = Some(op.index)
  }

  sealed case class MemOperand(addr: Int) extends Operand

  def mem(addr: Int) = MemOperand(addr)

  case object zero extends RegOperand(0)
  case object temp0 extends RegOperand(1)
  case object temp1 extends RegOperand(2)
  case object temp2 extends RegOperand(3)
  case object bbleft extends RegOperand(4)
  case object bbtop extends RegOperand(5)
  case object xs0 extends RegOperand(4)
  case object ys0 extends RegOperand(5)
  case object iv0 extends RegOperand(6)
  case object xs1 extends RegOperand(7)
  case object ys1 extends RegOperand(8)
  case object iv1 extends RegOperand(9)
  case object xs2 extends RegOperand(10)
  case object ys2 extends RegOperand(11)
  case object iv2 extends RegOperand(12)

  def assemble(function: Program => Unit): List[Inst] = {
    val program = new Program()
    function(program)
    program.uops += ((0, 0, 0, 0, false, 0, true)) // Halt
    program.uops.toList
  }
}

class VertexParameterInterface extends Bundle with IMasterSlave {
  val readEn = Bool()
  val readAddress = UInt(4 bits)
  val readData = UInt(32 bits)

  override def asMaster(): Unit = {
    out(readEn, readAddress)
    in(readData)
  }
}

class TriangleSetup extends Component {
  val io = new Bundle {
    val input = slave(spinal.lib.Stream(new TriangleSetupParams))
    val output = master(spinal.lib.Stream(new RasterizerSetupParams))
    val vpi = master(new VertexParameterInterface())
  }

  // RegOperand incoming parameters
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

  // Vertex parameter offsets
  val PARAM_X0 = 0
  val PARAM_Y0 = 1
  val PARAM_X1 = 2
  val PARAM_Y1 = 3
  val PARAM_X2 = 4
  val PARAM_Y2 = 5

  val microcode = MicrocodeCompiler.assemble { implicit program =>
    import MicrocodeCompiler._

    // Edge 1
    temp0 := bbleft - mem(PARAM_X0)
    temp1 := mem(PARAM_Y1)
    temp1 := temp1 - mem(PARAM_Y0)
    temp2 := temp0 * temp1
    temp0 := bbtop - mem(PARAM_Y0)
    temp1 := mem(PARAM_X1)
    temp1 := temp1 - mem(PARAM_X0)
    temp0 := temp0 * temp1
    iv0 := temp2 - temp0
    temp0 := mem(PARAM_Y1)
    xs0 := temp0 - mem(PARAM_Y0)
    temp0 := mem(PARAM_X0)
    ys0 := temp0 - mem(PARAM_X1)

    // Edge 2
    temp0 := bbleft - mem(PARAM_X1)
    temp1 := mem(PARAM_Y2)
    temp1 := temp1 - mem(PARAM_Y1)
    temp2 := temp0 * temp1
    temp0 := bbtop - mem(PARAM_Y1)
    temp1 := mem(PARAM_X2)
    temp1 := temp1 - mem(PARAM_X1)
    temp0 := temp0 * temp1
    iv1 := temp2 - temp0
    temp0 := mem(PARAM_Y2)
    xs1 := temp0 - mem(PARAM_Y1)
    temp0 := mem(PARAM_X1)
    ys1 := temp0 - mem(PARAM_X2)


    // Edge 3
    temp0 := bbleft - mem(PARAM_X2)
    temp1 := mem(PARAM_Y0)
    temp1 := temp1 - mem(PARAM_Y2)
    temp2 := temp0 * temp1
    temp0 := bbtop - mem(PARAM_Y2)
    temp1 := mem(PARAM_X0)
    temp1 := temp1 - mem(PARAM_X2)
    temp0 := temp0 * temp1
    iv2 := temp2 - temp0
    temp0 := mem(PARAM_Y0)
    xs2 := temp0 - mem(PARAM_Y2)
    temp0 := mem(PARAM_X2)
    ys2 := temp0 - mem(PARAM_X0)
  }

  // Control path
  val upc = Reg(UInt(log2Up(microcode.length) bits)) init(0)
  val microcodeRom = Mem(UInst(), initialContent = microcode.map(x => UInst(x._1, x._2, x._3, x._4, x._5, x._6, x._7)))
  val uInst = microcodeRom(upc)
  halt := uInst.halt
  io.vpi.readEn := uInst.readEn
  io.vpi.readAddress := uInst.readAddress

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
  // The order of this list must match the indices defined in case objects
  // defined in MicrocodeAssembler
  val operands = Vec(
    S(0, 32 bits),
    acc0,
    acc1,
    acc2,
    (inParams.bbLeft << 1).resize(32),
    (inParams.bbTop << 1).resize(32),
    S(0),
    S(0),
    S(0),
    S(0),
    S(0),
    S(0),
    S(0),
    S(0),
    S(0),
    io.vpi.readData.intoSInt.resize(32)
  )

  val operand1 = operands(uInst.src1)
  val operand2 = operands(uInst.src2)

  val result = Mux(uInst.opcode.asBool, (operand1(15 downto 0) * operand2(15 downto 0)), operand1 - operand2)

  when (uInst.dest === 1) { acc0 := result }
  when (uInst.dest === 2) { acc1 := result }
  when (uInst.dest === 3) { acc2 := result }
  when (uInst.dest === 4) { setupResult.xStep(0) := result }
  when (uInst.dest === 5) { setupResult.yStep(0) := result }
  when (uInst.dest === 6) { setupResult.initialValue(0) := result }
  when (uInst.dest === 7) { setupResult.xStep(1) := result }
  when (uInst.dest === 8) { setupResult.yStep(1) := result }
  when (uInst.dest === 9) { setupResult.initialValue(1) := result }
  when (uInst.dest === 10) { setupResult.xStep(2) := result }
  when (uInst.dest === 11) { setupResult.yStep(2) := result }
  when (uInst.dest === 12) { setupResult.initialValue(2) := result }
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
      dut.io.vpi.readData #= 0
      dut.clockDomain.waitSampling() // wait for reset

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


      val vpData = Array(
        x0,
        y0,
        x1,
        y1,
        x2,
        y2
      )

      fork {
        while (true) {
          dut.clockDomain.waitSampling()
          if (dut.io.vpi.readEn.toBoolean) {
            dut.io.vpi.readData #= vpData(dut.io.vpi.readAddress.toInt)
          }
        }
      }

      dut.io.input.valid #= true
      dut.io.input.bbLeft #= bbLeft
      dut.io.input.bbTop #= bbTop
      dut.io.input.bbRight #= bbRight
      dut.io.input.bbBottom #= bbBottom

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
