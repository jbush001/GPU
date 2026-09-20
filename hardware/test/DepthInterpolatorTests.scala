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

import scala.collection.mutable.Queue
import scala.util.Random

class DepthInterpolatorTests extends AnyFunSuite with ChiselSim {
  implicit val cfg: GpuConfig = GpuConfig()

  case class ExpectedQuad(
    primitiveId: Int,
    location: (Int, Int),
    mask: Int,
    lambdas: Seq[(Float, Float)],
    vertexW: (Float, Float, Float))

  def floatToRawBits(value: Float) = java.lang.Float.floatToIntBits(value)

  def rawToFloat(raw: BigInt) = java.lang.Float.intBitsToFloat(raw.toInt)

  def referenceDepth(
    vertexW: (Float, Float, Float),
    lambda: (Float, Float)
  ) = {
    val lambda2 = 1.0 - lambda._1 - lambda._2
    val weightedInverseW = lambda2 / vertexW._1 +
      lambda._1 / vertexW._2 + lambda._2 / vertexW._3
    1.0 / weightedInverseW
  }

  def pokeCoefficients(
    dut: DepthInterpolator,
    primitiveId: Int,
    coefficients: (Float, Float, Float)
  ) = {
    dut.io.writeCoeffs.bits.primitiveId.poke(primitiveId)
    val invW0 = 1.0f / coefficients._1
    val invW1 = 1.0f / coefficients._2
    val invW2 = 1.0f / coefficients._3
    dut.io.writeCoeffs.bits.coeffs.invW0.raw.poke(floatToRawBits(invW0))
    dut.io.writeCoeffs.bits.coeffs.invW1.raw.poke(floatToRawBits(invW1))
    dut.io.writeCoeffs.bits.coeffs.invW2.raw.poke(floatToRawBits(invW2))
    dut.io.writeCoeffs.bits.coeffs.invdW1.raw.poke(floatToRawBits(invW1 - invW0))
    dut.io.writeCoeffs.bits.coeffs.invdW2.raw.poke(floatToRawBits(invW2 - invW0))
    dut.io.writeCoeffs.valid.poke(true)
    dut.clock.step()
    dut.io.writeCoeffs.valid.poke(false)
  }

  def pokeQuadBits(
    dut: DepthInterpolator,
    primitiveId: Int,
    lambdas: Seq[(Float, Float)],
    location: (Int, Int) = (0, 0),
    mask: Int = 0xf
  ) = {
    dut.io.rasterizedQuad.bits.primitiveId.poke(primitiveId)
    dut.io.rasterizedQuad.bits.location.x.poke(location._1)
    dut.io.rasterizedQuad.bits.location.y.poke(location._2)
    dut.io.rasterizedQuad.bits.mask.poke(mask)
    for (i <- 0 until Consts.pixelsPerQuad) {
      dut.io.rasterizedQuad.bits.lambda(i)(0).raw.poke(floatToRawBits(lambdas(i)._1))
      dut.io.rasterizedQuad.bits.lambda(i)(1).raw.poke(floatToRawBits(lambdas(i)._2))
    }
  }

  def pokeQuad(
    dut: DepthInterpolator,
    primitiveId: Int,
    lambdas: Seq[(Float, Float)],
    location: (Int, Int) = (0, 0),
    mask: Int = 0xf
  ) = {
    pokeQuadBits(dut, primitiveId, lambdas, location, mask)
    dut.io.rasterizedQuad.valid.poke(true)
    assert(dut.io.rasterizedQuad.ready.peek().litToBoolean)
    dut.clock.step()
    dut.io.rasterizedQuad.valid.poke(false)

    for (i <- 0 until Consts.pixelsPerQuad) {
      dut.io.rasterizedQuad.bits.lambda(i)(0).raw.poke(0)
      dut.io.rasterizedQuad.bits.lambda(i)(1).raw.poke(0)
    }
  }

  def expectDepths(dut: DepthInterpolator, expected: Seq[Double]): Unit = {
    for (i <- 0 until Consts.pixelsPerQuad) {
      val actual = rawToFloat(dut.io.interpolatedQuad.bits.depths(i).raw.peek().litValue)
      assert(
        math.abs(actual - expected(i)) < 0.00001f,
        s"pixel $i: expected depth ${expected(i)}, got $actual")
    }
  }

  def expectCorrectedLambdas(
    dut: DepthInterpolator,
    vertexW: (Float, Float, Float),
    lambdas: Seq[(Float, Float)]
  ): Unit = {
    for (i <- 0 until Consts.pixelsPerQuad) {
      val lambda2 = 1.0 - lambdas(i)._1 - lambdas(i)._2
      val perspectiveDenominator = lambda2 / vertexW._1 +
        lambdas(i)._1 / vertexW._2 + lambdas(i)._2 / vertexW._3
      val expected = Seq(
        lambdas(i)._1 / vertexW._2 / perspectiveDenominator,
        lambdas(i)._2 / vertexW._3 / perspectiveDenominator)
      for (component <- 0 until 2) {
        val actual = rawToFloat(
          dut.io.interpolatedQuad.bits.quad.lambda(i)(component).raw.peek().litValue)
        assert(
          math.abs(actual - expected(component)) < 0.00001f,
          s"pixel $i lambda $component: expected ${expected(component)}, got $actual")
      }
    }
  }

  def expectQuadMetadata(
    dut: DepthInterpolator,
    primitiveId: Int,
    location: (Int, Int),
    mask: Int
  ): Unit = {
    dut.io.interpolatedQuad.bits.quad.primitiveId.expect(primitiveId)
    dut.io.interpolatedQuad.bits.quad.location.x.expect(location._1)
    dut.io.interpolatedQuad.bits.quad.location.y.expect(location._2)
    dut.io.interpolatedQuad.bits.quad.mask.expect(mask)
  }

  def expectReference(dut: DepthInterpolator, expected: ExpectedQuad): Unit = {
    expectQuadMetadata(
      dut,
      expected.primitiveId,
      expected.location,
      expected.mask)
    expectDepths(dut, expected.lambdas.map(referenceDepth(expected.vertexW, _)))
    expectCorrectedLambdas(dut, expected.vertexW, expected.lambdas)
  }

  test("DepthInterpolator boundary values") {
    simulate(new DepthInterpolator) { dut =>
      dut.io.interpolatedQuad.ready.poke(true)
      val coefficients = (0.25f, 0.5f, 1.0f)
      val lambdas = Seq(
        (0.0f, 0.0f),
        (1.0f, 0.0f),
        (0.0f, 1.0f),
        (0.2f, 0.3f))

      pokeCoefficients(dut, 2, coefficients)
      pokeQuad(dut, 2, lambdas, location = (7, 9), mask = 0x5)
      dut.clock.step(16)

      dut.io.interpolatedQuad.valid.expect(true.B)
      expectQuadMetadata(dut, 2, (7, 9), 0x5)
      expectDepths(dut, lambdas.map(referenceDepth(coefficients, _)))
      expectCorrectedLambdas(dut, coefficients, lambdas)
    }
  }

  test("DepthInterpolator stall") {
    simulate(new DepthInterpolator) { dut =>
      dut.io.interpolatedQuad.ready.poke(true)
      dut.io.idle.expect(true.B)
      pokeCoefficients(dut, 0, (0.25f, 0.5f, 1.0f))
      pokeQuad(
        dut,
        0,
        Seq.fill(Consts.pixelsPerQuad)((0.0f, 0.0f)),
        location = (3, 4),
        mask = 0x9)

      dut.clock.step(16)
      dut.io.interpolatedQuad.valid.expect(true.B)
      dut.io.interpolatedQuad.ready.poke(false)
      dut.io.rasterizedQuad.ready.expect(false.B)

      val depthBeforeStall = dut.io.interpolatedQuad.bits.depths(0).raw.peek().litValue
      val primitiveBeforeStall = dut.io.interpolatedQuad.bits.quad.primitiveId.peek().litValue
      val locationBeforeStall = dut.io.interpolatedQuad.bits.quad.location.x.peek().litValue

      dut.clock.step(3)
      dut.io.interpolatedQuad.valid.expect(true.B)
      dut.io.interpolatedQuad.bits.depths(0).raw.expect(depthBeforeStall)
      dut.io.interpolatedQuad.bits.quad.primitiveId.expect(primitiveBeforeStall)
      dut.io.interpolatedQuad.bits.quad.location.x.expect(locationBeforeStall)

      dut.io.interpolatedQuad.ready.poke(true)
      dut.clock.step()
      dut.io.interpolatedQuad.valid.expect(false.B)
      dut.io.idle.expect(true.B)
    }
  }

  test("DepthInterpolator stress") {
    simulate(new DepthInterpolator) { dut =>
      val rng = new Random(42)
      val vertexW = Seq(
        (0.2f, 0.65f, 1.3f),
        (0.35f, 0.8f, 1.6f),
        (0.5f, 0.9f, 1.8f),
        (0.7f, 1.1f, 2.0f))
      val expected = Queue[ExpectedQuad]()

      dut.io.interpolatedQuad.ready.poke(true.B)
      for (primitiveId <- vertexW.indices) {
        pokeCoefficients(dut, primitiveId, vertexW(primitiveId))
      }

      def randomLambdas(): Seq[(Float, Float)] = {
        Seq.fill(Consts.pixelsPerQuad) {
          val lambda0 = rng.nextFloat()
          val lambda1 = rng.nextFloat() * (1.0f - lambda0)
          (lambda0, lambda1)
        }
      }

      def checkOutput(): Unit = {
        val outputFire = dut.io.interpolatedQuad.valid.peek().litToBoolean &&
          dut.io.interpolatedQuad.ready.peek().litToBoolean
        if (outputFire) {
          assert(expected.nonEmpty, "Output appeared without an accepted input")
          expectReference(dut, expected.dequeue())
        }
      }

      for (cycle <- 0 until 1000) {
        dut.io.interpolatedQuad.ready.poke(rng.nextBoolean().B)
        val primitiveId = rng.nextInt(vertexW.length)
        val transaction = ExpectedQuad(
          primitiveId = primitiveId,
          location = (cycle % 32, (cycle * 3) % 32),
          mask = rng.nextInt(16),
          lambdas = randomLambdas(),
          vertexW = vertexW(primitiveId))
        pokeQuadBits(
          dut,
          transaction.primitiveId,
          transaction.lambdas,
          transaction.location,
          transaction.mask)
        val inputValid = rng.nextBoolean()
        dut.io.rasterizedQuad.valid.poke(inputValid.B)

        checkOutput()
        if (inputValid && dut.io.rasterizedQuad.ready.peek().litToBoolean) {
          expected.enqueue(transaction)
        }
        dut.clock.step()
      }

      dut.io.rasterizedQuad.valid.poke(false.B)
      dut.io.interpolatedQuad.ready.poke(true.B)
      var drainCycles = 0
      while (expected.nonEmpty || dut.io.interpolatedQuad.valid.peek().litToBoolean) {
        checkOutput()
        dut.clock.step()
        drainCycles += 1
        assert(drainCycles < 100, "Pipeline did not drain within 100 cycles")
      }
      assert(expected.isEmpty, "Not all accepted inputs produced outputs")
      dut.io.idle.expect(true.B)
    }
  }
}
