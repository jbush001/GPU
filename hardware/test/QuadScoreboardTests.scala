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

import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funsuite.AnyFunSuite

class QuadScoreboardTests extends AnyFunSuite with ChiselSim {
  implicit val cfg: GpuConfig = GpuConfig()

  def allocateTriangle(dut: QuadScoreboard): Int = {
    val triangleId = dut.io.allocateTriangleId.bits.peek().litValue.toInt
    dut.io.allocateTriangleId.valid.expect(true)
    dut.io.allocateTriangleId.bits.expect(triangleId)
    dut.io.allocateTriangleId.ready.poke(true)
    dut.clock.step()
    dut.io.allocateTriangleId.ready.poke(false)
    triangleId
  }

  def finishRasterization(dut: QuadScoreboard, triangleId: Int): Unit = {
    dut.io.rasterizationFinished.bits.poke(triangleId)
    dut.io.rasterizationFinished.valid.poke(true)
    dut.clock.step()
    dut.io.rasterizationFinished.valid.poke(false)
  }

  def issueQuad(dut: QuadScoreboard, triangleId: Int): Unit = {
    dut.io.issueQuad.bits.poke(triangleId)
    dut.io.issueQuad.valid.poke(true)
    dut.clock.step()
    dut.io.issueQuad.valid.poke(false)
  }

  def submitQuad(dut: QuadScoreboard): Unit = {
    dut.io.submitQuad.poke(true)
    dut.clock.step()
    dut.io.submitQuad.poke(false)
  }

  def retireQuad(dut: QuadScoreboard, triangleId: Int): Unit = {
    dut.io.retireQuad.bits.poke(triangleId)
    dut.io.retireQuad.valid.poke(true)
    dut.clock.step()
    dut.io.retireQuad.valid.poke(false)
  }

  test("QuadScoreboard allocate all") {
    simulate(new QuadScoreboard()) { dut =>
      val allocatedTriangles = scala.collection.mutable.ArrayBuffer[Int]()
      for (_ <- 0 until cfg.maxConcurrentTriangles) {
        val next = allocateTriangle(dut)
        assert(!allocatedTriangles.contains(next), s"Triangle ID $next was allocated twice")
        allocatedTriangles += next
      }

      dut.io.allocateTriangleId.valid.expect(false)
      dut.io.idle.expect(false)

      // Now free a triangle, ensure it can be reallocated
      finishRasterization(dut, allocatedTriangles(0))
      val reallocated = allocateTriangle(dut)
      assert(reallocated == allocatedTriangles(0), "Reallocated invalid triangle")
    }
  }

  // Finish rasterization, then ensure outstanding quads are handled correctly.
  test("QuadScoreboard outstanding quads") {
    simulate(new QuadScoreboard()) { dut =>
      val triangleId = allocateTriangle(dut)
      for (_ <- 0 until 10) {
        issueQuad(dut, triangleId)
        dut.io.idle.expect(false)
      }

      finishRasterization(dut, triangleId)
      dut.io.idle.expect(false)
      dut.clock.step(3) // Some clocks with no activity

      for (_ <- 0 until 10) {
        dut.io.allocateTriangleId.valid.expect(true)
        assert(dut.io.allocateTriangleId.bits.peek().litValue.toInt != triangleId,
          "Reallocated triangle ID that still has outstanding quads")

        dut.io.idle.expect(false)
        retireQuad(dut, triangleId)
      }

      dut.io.idle.expect(true)
      dut.io.allocateTriangleId.valid.expect(true)
      dut.io.allocateTriangleId.bits.expect(triangleId)
    }
  }

  // Retire all quads then finish rasterization.
  test("QuadScoreboard finish after last retire") {
    simulate(new QuadScoreboard()) { dut =>
      val triangleId = allocateTriangle(dut)
      issueQuad(dut, triangleId)
      dut.io.idle.expect(false)
      dut.clock.step(3) // Some clocks with no activity
      retireQuad(dut, triangleId)
      dut.io.idle.expect(false)
      finishRasterization(dut, triangleId)

      dut.io.idle.expect(true)
      dut.io.allocateTriangleId.valid.expect(true)
      dut.io.allocateTriangleId.bits.expect(triangleId)
    }
  }

  test("QuadScoreboard multiple triangles") {
    simulate(new QuadScoreboard()) { dut =>
      val tri1 = allocateTriangle(dut)
      val tri2 = allocateTriangle(dut)
      issueQuad(dut, tri1)
      dut.io.idle.expect(false)
      issueQuad(dut, tri2)
      dut.io.idle.expect(false)
      finishRasterization(dut, tri1)
      dut.io.idle.expect(false)
      finishRasterization(dut, tri2)
      dut.io.idle.expect(false)
      dut.clock.step(3) // Some clocks with no activity

      dut.io.allocateTriangleId.valid.expect(true)
      val nextTri = dut.io.allocateTriangleId.bits.peek().litValue.toInt
      assert(nextTri != tri1 && nextTri != tri2,
        "Reallocated triangle ID that still has outstanding quads")

      // XXX this makes an assumption about order of triangle allocation.
      retireQuad(dut, tri2)
      dut.io.allocateTriangleId.valid.expect(true)
      dut.io.allocateTriangleId.bits.expect(tri2)
      dut.io.idle.expect(false)

      retireQuad(dut, tri1)
      dut.io.allocateTriangleId.valid.expect(true)
      dut.io.allocateTriangleId.bits.expect(tri1)
      dut.io.idle.expect(true)
    }
  }

  // The count should remain the same in this case.
  test("QuadScoreboard simultaneous issue/retire") {
    simulate(new QuadScoreboard()) { dut =>
      val triangleId = allocateTriangle(dut)

      issueQuad(dut, triangleId)

      dut.io.issueQuad.bits.poke(triangleId)
      dut.io.issueQuad.valid.poke(true)
      dut.io.retireQuad.bits.poke(triangleId)
      dut.io.retireQuad.valid.poke(true)
      dut.clock.step()
      dut.io.issueQuad.valid.poke(false)
      dut.io.retireQuad.valid.poke(false)

      dut.clock.step(3) // Some clocks with no activity

      retireQuad(dut, triangleId) // Count should now be zero.
      dut.io.idle.expect(false)

      finishRasterization(dut, triangleId)
      dut.io.idle.expect(true)
      dut.io.allocateTriangleId.valid.expect(true)
      dut.io.allocateTriangleId.bits.expect(triangleId)
    }
  }

  test("QuadScoreboard flushPixelShader") {
    simulate(new QuadScoreboard()) { dut =>
      val triangleId = allocateTriangle(dut)
      dut.io.flushPixelShader.expect(false)

      issueQuad(dut, triangleId)
      finishRasterization(dut, triangleId)
      dut.io.flushPixelShader.expect(false)

      submitQuad(dut)
      dut.io.flushPixelShader.expect(false)
      dut.io.batchFinished.poke(true)
      dut.io.flushPixelShader.expect(true)
    }
  }

  test("QuadScoreboard simultaneous issue and submission") {
    simulate(new QuadScoreboard()) { dut =>
      val triangleId = allocateTriangle(dut)
      dut.io.issueQuad.bits.poke(triangleId)
      dut.io.issueQuad.valid.poke(true)
      dut.io.submitQuad.poke(true)
      dut.clock.step()
      dut.io.issueQuad.valid.poke(false)
      dut.io.submitQuad.poke(false)
      dut.io.batchFinished.poke(true)

      finishRasterization(dut, triangleId)
      dut.io.flushPixelShader.expect(true)
    }
  }
}
