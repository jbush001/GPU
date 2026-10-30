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

  case class InputParams(
    vertices: Seq[(Int, Int, Float)],
    tileLeft: Int,
    tileTop: Int,
    varyings: Seq[Seq[Float]]
  )

  def runSetup(dut: TriangleSetup, params: InputParams) = {
    val varyingCoeffs = Array.fill(params.varyings(0).length * 3)(Float.NaN)

    val triangleId = submitParams(dut, params, varyingCoeffs)
    if (triangleId.isEmpty) {
      fail("Triangle setup did not finish")
    }

    checkOutput(dut, params, varyingCoeffs)
  }

  def submitParams(dut: TriangleSetup, params: InputParams, outVaryingCoeffs: Array[Float]): Option[Int] = {
    val vertexParamMem = Array.fill(1024)(0)
    val baseAddresses = Array(100, 210, 308)

    var waitCount = 0
    while (!dut.io.startTriangle.ready.peek().litToBoolean) {
      dut.clock.step()
      waitCount += 1
      if (waitCount > 1000) {
        fail("Timeout waiting for startTriangle to be ready")
      }
    }

    dut.io.startTriangle.bits.vertexBaseAddr(0).poke(baseAddresses(0))
    dut.io.startTriangle.bits.vertexBaseAddr(1).poke(baseAddresses(1))
    dut.io.startTriangle.bits.vertexBaseAddr(2).poke(baseAddresses(2))
    dut.io.startTriangle.bits.lastTriangle.poke(true)

    // Vertex memory layout
    //  0 - x, y (12.4 fixed point each)
    //  1 - 1.0 / w (float)
    //  2 - varying 0...
    for (vertexI <- 0 until 3) {
      val (x, y, invW) = params.vertices(vertexI)
      val baseAddr = baseAddresses(vertexI)
      vertexParamMem(baseAddr + 0) = ((x << 16) | (y & 0xffff)).toInt
      vertexParamMem(baseAddr + 1) = floatToRaw(invW)
      for ((v, i) <- params.varyings(vertexI).zipWithIndex) {
        vertexParamMem(baseAddr + 2 + i) = floatToRaw(v)
      }
    }

    dut.io.startTriangle.bits.numVaryings.poke(params.varyings(0).length)
    dut.io.startTriangle.bits.tileOffset.x.poke(params.tileLeft)
    dut.io.startTriangle.bits.tileOffset.y.poke(params.tileTop)
    dut.io.startTriangle.valid.poke(true.B)

    var triangleId = Option.empty[Int]

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
        assert(index < outVaryingCoeffs.length * 3)
        outVaryingCoeffs(index) = dut.io.writeVaryingCoeff.bits.value.peek()
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
        assert(triangleId.isEmpty)
        return None
      }
    }

    triangleId match {
      case Some(id) => dut.io.rasterizerParams.bits.triangleId.expect(id.U)
      case None => triangleId = Some(dut.io.rasterizerParams.bits.triangleId.peek().litValue.toInt)
    }

    return triangleId
  }

  def checkOutput(dut: TriangleSetup, params: InputParams, varyingCoeffs: Array[Float]) = {
    val result = dut.io.rasterizerParams.bits

    result.depthCoeffs.invW0.expect(params.vertices(0)._3)
    result.depthCoeffs.invW1.expect(params.vertices(1)._3)
    result.depthCoeffs.invW2.expect(params.vertices(2)._3)
    result.depthCoeffs.invdW1.expect(params.vertices(1)._3 - params.vertices(0)._3)
    result.depthCoeffs.invdW2.expect(params.vertices(2)._3 - params.vertices(0)._3)
    result.offset.x.expect(params.tileLeft)
    result.offset.y.expect(params.tileTop)
    result.boundingBox.left.expect(params.tileLeft)
    result.boundingBox.top.expect(params.tileTop)
    result.boundingBox.right.expect(params.tileLeft + (cfg.tileSizePixels - 2))
    result.boundingBox.bottom.expect(params.tileTop + (cfg.tileSizePixels - 2))
    result.lastTriangle.expect(true.B)

    val rawParams = new Array[(Long, Long, Long)](3)
    var area2 = 0.0f
    for (edge <- 0 until 3) {
      val (startX, startY, _) = params.vertices(edge)
      val (endX, endY, _) = params.vertices((edge + 1) % 3)
      val dx = endY - startY
      val dy = startX - endX
      val iv = (params.tileTop - startY) * dy + ((params.tileLeft - startX) * dx)
      area2 += iv
      rawParams(edge) = (dx.toLong, dy, iv)
    }

    val normFactor = 1.0 / area2

    for (edge <- rawParams.indices) {
      val (xs, ys, iv) = rawParams(edge)
      val normXs = (xs * normFactor * 0xffffffL).toLong
      val normYs = (ys * normFactor * 0xffffffL).toLong
      val normIv = (iv * normFactor * 0xffffffL).toLong

      val edgeVals = dut.io.rasterizerParams.bits.edges(edge)

      // Note: due to finite precision differences, our reference values
      // may be slightly off.
      assert(scala.math.abs(edgeVals.xStep.peek().litValue.toLong - normXs) <= 2)
      assert(scala.math.abs(edgeVals.yStep.peek().litValue.toLong - normYs) <= 2)
      assert(scala.math.abs(edgeVals.initialValue.peek().litValue.toLong - normIv) <= 2)
    }

    for (i <- 0 until params.varyings(0).length) {
      val c1 = varyingCoeffs(i * 3 + 0)
      val c2 = varyingCoeffs(i * 3 + 1)
      val c0 = varyingCoeffs(i * 3 + 2)
      assert(floatCloseEnough(params.varyings(0)(i), c0))
      assert(floatCloseEnough(params.varyings(1)(i) - params.varyings(0)(i), c1))
      assert(floatCloseEnough(params.varyings(2)(i) - params.varyings(0)(i), c2))
    }
  }

  def isFrontFace(params: InputParams): Boolean = {
    // Counter-clockwise winding order indicates a front-facing triangle.
    val (x0, y0, _) = params.vertices(0)
    val (x1, y1, _) = params.vertices(1)
    val (x2, y2, _) = params.vertices(2)
    (x1 - x0) * (y2 - y0) - (y1 - y0) * (x2 - x0) < 0
  }

  def generateRandomTriangle(rng: scala.util.Random): InputParams = {
    val vertices = Seq(
      (rng.nextInt(128), rng.nextInt(128), rng.nextFloat()),
      (rng.nextInt(128), rng.nextInt(128), rng.nextFloat()),
      (rng.nextInt(128), rng.nextInt(128), rng.nextFloat())
    )

    val tileLeft = rng.nextInt(4) * cfg.tileSizePixels
    val tileTop = rng.nextInt(4) * cfg.tileSizePixels
    val numVaryings = rng.nextInt(8)
    val varyings = Seq.fill(3)(Seq.fill(numVaryings)(rng.nextFloat()))

    InputParams(vertices, tileLeft, tileTop, varyings)
  }

  test("TriangleSetup basic operation") {
    simulate(new TriangleSetup) { dut =>
      runSetup(dut,
        InputParams(
          Seq((27, 13, 0.5f), (65, 110, 0.6f), (123, 25, 0.7f)),
          0, 0,
          Seq(Seq(), Seq(), Seq()) // No parameters
        )
      )
    }
  }

  test("TriangleSetup with varying parameters") {
    simulate(new TriangleSetup) { dut =>
      runSetup(dut,
        InputParams(
          Seq((27, 13, 0.5f), (65, 110, 0.6f), (123, 25, 0.7f)),
          64, 64,
          Seq(Seq(1.0f, 2.0f, 3.0f), Seq(4.0f, 5.0f, 6.0f), Seq(7.0f, 8.0f, 9.0f))
        )
      )
    }
  }

  test("TriangleSetup backface culling") {
    simulate(new TriangleSetup) { dut =>
      val varyingCoeffs = Array.fill(3)(Float.NaN)

      // Run this multiple times to ensure the triangle ID is freed when
      // the triangle is culled.
      for (_ <- 0 until cfg.maxConcurrentTriangles + 1) {
        dut.io.startTriangle.ready.expect(true)
        assert(!submitParams(dut,
          InputParams(Seq((27, 13, 0.5f), (123, 25, 0.7f), (65, 110, 0.6f)),
          0, 0,
          Seq(Seq(), Seq(), Seq())), // No parameters
          varyingCoeffs
        ).isDefined)
      }
    }
  }

  test("TriangleSetup stress") {
    simulate(new TriangleSetup) { dut =>
      val varyingCoeffs = Array.fill(64)(Float.NaN)
      val activeTriangleIds = scala.collection.mutable.Set[Int]()
      for (_ <- 0 until 500) {
        // Free a random triangle ID if there are more than four
        if (activeTriangleIds.size == cfg.maxConcurrentTriangles) {
          val triangleId = activeTriangleIds.toSeq(scala.util.Random.nextInt(activeTriangleIds.size))
          dut.io.triangleFinished.bits.poke(triangleId)
          dut.io.triangleFinished.valid.poke(true)
          dut.clock.step()
          dut.io.triangleFinished.valid.poke(false)
          activeTriangleIds -= triangleId
        }

        val params = generateRandomTriangle(scala.util.Random)

        val frontFace = isFrontFace(params)
        val triangleId = submitParams(dut, params, varyingCoeffs)
        if (triangleId.isDefined != frontFace) {
          fail(s"Triangle emitted status $triangleId did not match expected front face $frontFace")
        }

        if (triangleId.isDefined) {
          activeTriangleIds += triangleId.get
          checkOutput(dut, params, varyingCoeffs)
        }
      }
    }
  }
}