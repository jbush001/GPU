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
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ListBuffer
import chisel3.util.Queue

class UInst extends Bundle {
  val opcode = UInt(1.W)
  val dest = UInt(4.W)
  val src1 = UInt(4.W)
  val src2 = UInt(4.W)
  val readEn = Bool()
  val readAddress = UInt(4.W)
  val halt = Bool()
}

object UInst {
  def apply(opcode: Int, dest: Int, src1: Int, src2: Int, readEn: Boolean = false, readAddress: Int = 0, halt: Boolean = false): UInst = {
    val result = Wire(new UInst)
    result.opcode := opcode.U
    result.dest := dest.U
    result.src1 := src1.U
    result.src2 := src2.U
    result.readEn := readEn.B
    result.readAddress := readAddress.U
    result.halt := halt.B
    result
  }

  def apply() = new UInst()
}

/** Implements a domain specific language for defining the microcode
  * program to set up the triangle parameters.
  *
  * @todo some registers can be used as sources, some as destinations, some both.
  * I don't check that here for simplicity, but there are a number of ways to do
  * this at either compile or runtime.
  */
object MicrocodeAssembler {
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

  // This register ID gets the result of the memory read from the
  // last cycle.
  val REG_MEM_RESULT = 7

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
          program.uops += ((opcode, this.index, a, REG_MEM_RESULT, false, 0, false))
        }

        case (MemOperand(a), RegOperand(b)) => {
          program.addLoadOp(a)
          program.uops += ((opcode, this.index, REG_MEM_RESULT, b, false, 0, false))
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

class VertexParameterInterface extends Bundle {
  val readEn = Output(Bool())
  val readAddress = Output(UInt(4.W))
  val readData = Input(UInt(32.W))
}

/** Computes the edge equation coefficients used by the rasterizer.
  *
  * This has an output buffer so the computation for the next triangle
  * overlaps rasterization of the current one. To optimize area, this uses
  * a small microsequencer that performs the calculations (rather than just
  * doing it all in parallel).
  *
  * @todo Rework to compute in floating point.
  * @todo Implment top-left fill convention to properly handle shared edge 
  *   overlap. For a top of left edge (y1 > y2 || (y1 == y2 && x2 > x1)), 
  *   increment the edge values by one.
  */
class TriangleSetup extends Module {
  val io = IO(new Bundle {
    val input = Flipped(Decoupled(BoundingBox()))
    val output = Decoupled(new RasterizerSetupParams)
    val vpi = new VertexParameterInterface()
  })

  // RegOperand incoming parameters
  val inParams = RegEnable(io.input.bits, io.input.fire)

  // Here is where we accumulate values as we compute them
  val setupResult = Reg(new RasterizerSetupParams())
  val outputResultPending = RegInit(Bool(), false.B)

  val internalStream = Wire(Decoupled(new RasterizerSetupParams))
  internalStream.bits := setupResult
  internalStream.valid := outputResultPending

  // This adds an additional buffer, which lets us get one triangle ahead
  // of the rasterizer, overlapping execution.
  io.output <> Queue(internalStream, entries = 1)

  val halt = Wire(Bool())
  val computing = RegInit(Bool(), false.B)
  io.input.ready := !computing && !outputResultPending

  // This state machine handles start and finishing computations and
  // handshaking with upstream and downstream components.
  when (computing) {
    when (halt) {
      outputResultPending := true.B
      computing := false.B
    }
  }.otherwise {
    when (io.input.fire) {
      setupResult.boundingBox := io.input.bits
      computing := true.B
    }
  }

  when (internalStream.fire) {
    outputResultPending := false.B
  }

  // Vertex parameter offsets
  val PARAM_X0 = 0
  val PARAM_Y0 = 1
  val PARAM_X1 = 2
  val PARAM_Y1 = 3
  val PARAM_X2 = 4
  val PARAM_Y2 = 5

  val microcode = MicrocodeAssembler.assemble { implicit program =>
    import MicrocodeAssembler._

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
  val upc = RegInit(UInt(log2Up(microcode.length).W), 0.U)
  val microcodeRom = VecInit(microcode.map(x => UInst(x._1, x._2, x._3, x._4, x._5, x._6, x._7)))
  val uInst = microcodeRom(upc)
  halt := uInst.halt
  io.vpi.readEn := uInst.readEn && computing
  io.vpi.readAddress := uInst.readAddress

  when (computing) {
    upc := upc + 1.U
  }.otherwise {
    upc := 0.U
  }

  // Scratchpad registers for interim results.
  val acc0 = Reg(SInt(32.W))
  val acc1 = Reg(SInt(32.W))
  val acc2 = Reg(SInt(32.W))

  // Source operand multiplexers
  // The order of this list must match the indices defined in case objects
  // defined in MicrocodeAssembler
  val operands = VecInit(
    0.S(32.W),
    acc0,
    acc1,
    acc2,
    inParams.left.pad(32),
    inParams.top.pad(32),
    0.S(32.W),
    io.vpi.readData.asSInt.pad(32)
  )

  val operand1 = operands(uInst.src1(2, 0))
  val operand2 = operands(uInst.src2(2, 0))

  val result = Mux(uInst.opcode.asBool, 
    (operand1(15, 0).asSInt * operand2(15, 0).asSInt), 
    operand1 - operand2)

  when (uInst.dest === 1.U) { acc0 := result }
  when (uInst.dest === 2.U) { acc1 := result }
  when (uInst.dest === 3.U) { acc2 := result }
  when (uInst.dest === 4.U) { setupResult.xStep(0) := result }
  when (uInst.dest === 5.U) { setupResult.yStep(0) := result }
  when (uInst.dest === 6.U) { setupResult.initialValue(0) := result }
  when (uInst.dest === 7.U) { setupResult.xStep(1) := result }
  when (uInst.dest === 8.U) { setupResult.yStep(1) := result }
  when (uInst.dest === 9.U) { setupResult.initialValue(1) := result }
  when (uInst.dest === 10.U) { setupResult.xStep(2) := result }
  when (uInst.dest === 11.U) { setupResult.yStep(2) := result }
  when (uInst.dest === 12.U) { setupResult.initialValue(2) := result }
}

class TriangleSetupTests extends AnyFunSuite with ChiselSim {
  def computeExpectedValues(bbLeft: Int, bbTop: Int,
                            x0: Int, y0: Int, x1: Int, y1: Int, x2: Int, y2: Int) = {
    val xStep0 = y1 - y0
    val yStep0 = x0 - x1
    val initialValue0 = (bbLeft - x0) * (y1 - y0) - (bbTop - y0) * (x1 - x0)

    val xStep1 = y2 - y1
    val yStep1 = x1 - x2
    val initialValue1 = (bbLeft - x1) * (y2 - y1) - (bbTop - y1) * (x2 - x1)

    val xStep2 = y0 - y2
    val yStep2 = x2 - x0
    val initialValue2 = (bbLeft - x2) * (y0 - y2) - (bbTop - y2) * (x0 - x2)

    (xStep0, yStep0, initialValue0,
      xStep1, yStep1, initialValue1,
      xStep2, yStep2, initialValue2)
  }

  // TODO test input/output handshaking, queuing up multiple requests.

  test("triangle setup") {
   simulate(new TriangleSetup()) { dut =>
      dut.io.input.valid.poke(false)
      dut.io.vpi.readData.poke(0)
      dut.clock.step() // wait for reset

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

      dut.io.input.valid.poke(true)
      dut.io.input.bits.left.poke(bbLeft)
      dut.io.input.bits.top.poke(bbTop)
      dut.io.input.bits.right.poke(bbRight)
      dut.io.input.bits.bottom.poke(bbBottom)

      while (dut.io.input.ready.peek().litValue.toLong == 0) {
        dut.clock.step()
      }

      dut.clock.step()

      // Clear the inputs to ensure it has latched them properly.
      dut.io.input.valid.poke(false)
      dut.io.input.bits.left.poke(0)
      dut.io.input.bits.top.poke(0)
      dut.io.input.bits.right.poke(0)
      dut.io.input.bits.bottom.poke(0)

      while (dut.io.output.valid.peek().litValue.toLong == 0) {
        if (dut.io.vpi.readEn.peek().litValue.toInt != 0) {
          val readData = vpData(dut.io.vpi.readAddress.peek().litValue.toInt)
          dut.clock.step()
          dut.io.vpi.readData.poke(readData)
        } else {
          dut.clock.step()
        }
      }

      val (xStep0, yStep0, initialValue0,
        xStep1, yStep1, initialValue1,
        xStep2, yStep2, initialValue2) = computeExpectedValues(bbLeft, bbTop,
          x0, y0, x1, y1, x2, y2)
      dut.io.output.bits.xStep(0).expect(xStep0)
      dut.io.output.bits.yStep(0).expect(yStep0)
      dut.io.output.bits.initialValue(0).expect(initialValue0)
      dut.io.output.bits.xStep(1).expect(xStep1)
      dut.io.output.bits.yStep(1).expect(yStep1)
      dut.io.output.bits.initialValue(1).expect(initialValue1)
      dut.io.output.bits.xStep(2).expect(xStep2)
      dut.io.output.bits.yStep(2).expect(yStep2)
      dut.io.output.bits.initialValue(2).expect(initialValue2)
      dut.io.output.bits.boundingBox.left.expect(bbLeft)
      dut.io.output.bits.boundingBox.top.expect(bbTop)
      dut.io.output.bits.boundingBox.right.expect(bbRight)
      dut.io.output.bits.boundingBox.bottom.expect(bbBottom)
    }
  }
}
