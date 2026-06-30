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
import spinal.lib._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

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

class TriangleSetup extends Component {
  val io = new Bundle {
    val input = slave(spinal.lib.Stream(new TriangleSetupParams()))
    val output = master(spinal.lib.Stream(new RasterizerSetupParams()))
  }

  // Register incoming parameters
  val inParams = RegNextWhen(io.input.payload, io.input.fire)

  val inputReady = Reg(Bool()) init(True)
  io.input.ready := inputReady

  // Here is where we accumulate values as we compute them
  val setupResult = Reg(new RasterizerSetupParams())
  val setupResultValid = Reg(Bool()) init(False)

  val internalStream = Stream(new RasterizerSetupParams())
  internalStream.payload := setupResult
  internalStream.valid := setupResultValid

  // This adds an additional buffer, which lets us get one triangle ahead
  // of the rasterizer, overlapping execution.
  io.output << internalStream.stage()

  val halt = Bool()
  val halted = Reg(Bool()) init(True)

  // This state machine handles start and finishing computations and
  // handshaking with upstream and downstream components.
  when (io.input.fire) {
    inputReady := False
    setupResult.bbLeft := io.input.bbLeft
    setupResult.bbRight := io.input.bbRight
    setupResult.bbTop := io.input.bbTop
    setupResult.bbBottom := io.input.bbBottom
  } elsewhen (!inputReady) {
    when (halt) { // XXX check for completion
      setupResultValid := True
      halted := True
    } otherwise {
      halted := False
    }


    when (internalStream.fire) {
      // Client consumed, reset and prepare to set up another.
      inputReady := True
      setupResultValid := False
    }
  }

  val A0 = 1
  val A1 = 2
  val A2 = 3
  val S_X0 = 4
  val S_X1 = 5
  val S_X2 = 6
  val S_Y0 = 7
  val S_Y1 = 8
  val S_Y2 = 9
  val S_BL = 10
  val S_BT = 11

  val D_XS0 = 4
  val D_YS0 = 5
  val D_IV0 = 6
  val D_XS1 = 7
  val D_YS1 = 8
  val D_IV1 = 9
  val D_XS2 = 10
  val D_YS2 = 11
  val D_IV2 = 12

  val SUB = 0
  val MUL = 1

  val microcode = Array(
    // Edge 0->1
    (0, SUB, A0, S_BL, S_X0),
    (0, SUB, A1, S_Y1, S_Y0),
    (0, MUL, A2, A0, A1),
    (0, SUB, A0, S_BT, S_Y0),
    (0, SUB, A1, S_X1, S_X0),
    (0, MUL, A0, A0, A1),
    (0, SUB, D_IV0, A2, A0),
    (0, SUB, D_XS0, S_Y1, S_Y0),
    (0, SUB, D_YS0, S_X0, S_X1),

    // Edge 1->2
    (0, SUB, A0, S_BL, S_X1),
    (0, SUB, A1, S_Y2, S_Y1),
    (0, MUL, A2, A0, A1),
    (0, SUB, A0, S_BT, S_Y1),
    (0, SUB, A1, S_X2, S_X1),
    (0, MUL, A0, A0, A1),
    (0, SUB, D_IV1, A2, A0),
    (0, SUB, D_XS1, S_Y2, S_Y1),
    (0, SUB, D_YS1, S_X1, S_X2),

    // Edge 2->0
    (0, SUB, A0, S_BL, S_X2),
    (0, SUB, A1, S_Y0, S_Y2),
    (0, MUL, A2, A0, A1),
    (0, SUB, A0, S_BT, S_Y2),
    (0, SUB, A1, S_X0, S_X2),
    (0, MUL, A0, A0, A1),
    (0, SUB, D_IV2, A2, A0),
    (0, SUB, D_XS2, S_Y0, S_Y2),
    (0, SUB, D_YS2, S_X2, S_X0),

    (1, 0, 0, 0, 0) // Halt
  )

  // Control path
  val upc = Reg(UInt(log2Up(microcode.length) bits)) init(0)
  val microcodeRom = Mem(UInt(18 bits),
    initialContent = microcode.map(x => U(((x._1 << 13) | (x._2 << 12) | (x._3 << 8) | (x._4 << 4) | x._5), 18 bits)))
  val uInst = microcodeRom(upc)
  halt := uInst(13)
  val operation = uInst(12)
  val destSelect = uInst(11 downto 8)
  val aSelect = uInst(7 downto 4)
  val bSelect = uInst(3 downto 0)

  when (halted) {
    upc := 0
  } otherwise {
    upc := upc + 1
  }

  // Scratchpad registers for interim results.
  val acc0 = Reg(SInt(32 bits))
  val acc1 = Reg(SInt(32 bits))
  val acc2 = Reg(SInt(32 bits))

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
  test("triangle setup") {
    TestConfig.testSim.compile(new TriangleSetup()).doSim { dut =>
        val cd = dut.clockDomain.get
        cd.forkStimulus(period = 10)

        dut.io.input.valid #= false

        cd.waitSampling()

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
          cd.waitSampling()
        }

        // Wait for a clock edge to latch it.
        cd.waitSampling()

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

        var iterationCount = 0;
        while (!dut.io.output.valid.toBoolean) {
          assert(iterationCount < 50)
          iterationCount += 1
          cd.waitSampling()
        }

        assert(dut.io.output.valid.toBoolean)

        assert(dut.io.output.xStep(0).toInt == y1 - y0)
        assert(dut.io.output.yStep(0).toInt == x0 - x1)
        assert(dut.io.output.initialValue(0).toInt
          == ((bbLeft * 2) - x0) * (y1 - y0) - ((bbTop * 2) - y0) * (x1 - x0))
        assert(dut.io.output.xStep(1).toInt == y2 - y1)
        assert(dut.io.output.yStep(1).toInt == x1 - x2)
        assert(dut.io.output.initialValue(1).toInt
          == ((bbLeft * 2) - x1) * (y2 - y1) - ((bbTop * 2) - y1) * (x2 - x1))
        assert(dut.io.output.xStep(2).toInt == y0 - y2)
        assert(dut.io.output.yStep(2).toInt == x2 - x0)
        assert(dut.io.output.initialValue(2).toInt
          == ((bbLeft * 2) - x2) * (y0 - y2) - ((bbTop * 2) - y2) * (x0 - x2))
        assert(dut.io.output.bbLeft.toInt == bbLeft)
        assert(dut.io.output.bbTop.toInt == bbTop)
        assert(dut.io.output.bbRight.toInt == bbRight)
        assert(dut.io.output.bbBottom.toInt == bbBottom)
    }
  }
}
