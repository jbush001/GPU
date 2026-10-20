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
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funsuite.AnyFunSuite

class TriangleSetupTests extends AnyFunSuite with ChiselSim with TestHelpers {
  implicit val cfg: GpuConfig = GpuConfig()

  def floatCloseEnough(actual: Float, expected: Float, epsilon: Float = 1e-6f): Boolean = {
    math.abs(actual - expected) < epsilon
  }

  def floatToRaw(value: Float): Int = java.lang.Float.floatToRawIntBits(value)
  def runSetup(dut: TriangleSetup, vertices: Seq[(Int, Int, Float)],
    tileLeft: Int, tileTop: Int, varyings: Seq[Seq[Float]]) = {

    require(vertices.length == 3, "There must be exactly 3 vertices")
    require(varyings.length == 3, "There must be exactly 3 sets of varyings")
    require(varyings.forall(_.length == varyings(0).length),
      "All varying sets must have the same length")

    val vertexParamMem = Array.fill(1024)(0)
    val baseAddresses = Array(100, 210, 308)
    dut.io.startTriangle.bits.vertexBaseAddr(0).poke(baseAddresses(0))
    dut.io.startTriangle.bits.vertexBaseAddr(1).poke(baseAddresses(1))
    dut.io.startTriangle.bits.vertexBaseAddr(2).poke(baseAddresses(2))

    // Vertex memory layout
    //  0 - x, y (12.4 fixed point each)
    //  1 - 1.0 / w (float)
    //  2 - varying 0...
    for (vertexI <- 0 until 3) {
      val (x, y, invW) = vertices(vertexI)
      val baseAddr = baseAddresses(vertexI)
      vertexParamMem(baseAddr + 0) = ((x << 16) | (y & 0xffff)).toInt
      vertexParamMem(baseAddr + 1) = floatToRaw(invW)
      for ((v, i) <- varyings(vertexI).zipWithIndex) {
        vertexParamMem(baseAddr + 2 + i) = floatToRaw(v)
      }
    }

    dut.io.startTriangle.bits.numVaryings.poke(varyings(0).length)
    dut.io.startTriangle.bits.bbLeft.poke(tileLeft)
    dut.io.startTriangle.bits.bbTop.poke(tileTop)
    dut.io.startTriangle.valid.poke(true.B)

    var triangleId = Option.empty[Int]
    val varyingCoeffs = Array.fill(varyings(0).length * 3)(Float.NaN)

    var cycleCount = 0
    dut.io.rasterizerParams.ready.poke(true.B)
    while (!dut.io.rasterizerParams.valid.peek().litToBoolean) {
      // Process varying writes.
      if (dut.io.writeVaryingCoeff.valid.peek().litToBoolean) {
        // Ensure the triangle ID is consistent
        triangleId match {
          case Some(id) => dut.io.writeVaryingCoeff.bits.triangleId.expect(id.U)
          case None => triangleId = Some(dut.io.writeVaryingCoeff.bits.triangleId.peek().litValue.toInt)
        }

        val index = dut.io.writeVaryingCoeff.bits.index.peek().litValue.toInt
        assert(index < varyingCoeffs.length)
        varyingCoeffs(index) = dut.io.writeVaryingCoeff.bits.value.peek()
      }

      // Simulate parameter memory read
      if (dut.io.paramReadAddr.valid.peek().litToBoolean) {
        val readVal = vertexParamMem(dut.io.paramReadAddr.bits.peek().litValue.toInt)
        dut.clock.step()
        dut.io.readValue.poke(readVal)
      } else {
        dut.clock.step()
        dut.io.readValue.poke(0xffffffff)
      }

      if (cycleCount == 0) {
        dut.io.startTriangle.valid.poke(false.B)
      }

      cycleCount += 1
      if (cycleCount > 100) {
        fail("Triangle setup did not finish")
      }
    }

    // Now check all values
    val result = dut.io.rasterizerParams.bits
    triangleId match {
      case Some(id) => result.triangleId.expect(id.U) // Ensure the value is consistent
      case None => // If there are no params, this won't be set.
    }

    result.depthCoeffs.invW0.expect(vertices(0)._3)
    result.depthCoeffs.invW1.expect(vertices(1)._3)
    result.depthCoeffs.invW2.expect(vertices(2)._3)
    result.depthCoeffs.invdW1.expect(vertices(1)._3 - vertices(0)._3)
    result.depthCoeffs.invdW2.expect(vertices(2)._3 - vertices(0)._3)

    val rawParams = new Array[(Int, Int, Int)](3)
    var area2 = 0.0f
    for (edge <- 0 until 3) {
      val (startX, startY, _) = vertices(edge)
      val (endX, endY, _) = vertices((edge + 1) % 3)
      val dx = startY - endY
      val dy = startX - endX
      val iv = ((tileLeft - startX) * dx) - ((tileTop - startY) * dy)
      area2 += iv
      rawParams(edge) = (dx, dy, iv)
    }

//    val normFactor = 1.0 / area2
    val normFactor = 1.0 / 0xffff // XXX Unnormalized for now

    for (edge <- rawParams.indices) {
      val (xs, ys, iv) = rawParams(edge)
      val normXs = (xs * normFactor * 0xffffL).toInt
      val normYs = (ys * normFactor * 0xffffL).toInt
      val normIv = (iv * normFactor * 0xffffL).toInt

      dut.io.rasterizerParams.bits.edges(edge).xStep.expect(normXs)
      dut.io.rasterizerParams.bits.edges(edge).yStep.expect(normYs)
      dut.io.rasterizerParams.bits.edges(edge).initialValue.expect(normIv)
    }

    for (i <- 0 until varyings(0).length) {
      val c0 = varyingCoeffs(i * 3 + 0)
      val c1 = varyingCoeffs(i * 3 + 1)
      val c2 = varyingCoeffs(i * 3 + 2)
      assert(floatCloseEnough(varyings(0)(i), c0))
      assert(floatCloseEnough(varyings(1)(i), c1 - c0))
      assert(floatCloseEnough(varyings(2)(i), c2 - c0))
    }
  }

  test("TriangleSetup basic operation") {
    simulate(new TriangleSetup) { dut =>
      runSetup(dut,
        Seq((27, 13, 0.5f), (65, 110, 0.6f), (123, 25, 0.7f)),
        0, 0,
        Seq(Seq(), Seq(), Seq()) // No parameters
      )
    }
  }
}