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

class DepthInterpolatorTests extends AnyFunSuite with ChiselSim {
  implicit val cfg: GpuConfig = GpuConfig()

  val oneRawBits = java.lang.Float.floatToRawIntBits(1.0f)

  def floatToRawBits(value: Float) = java.lang.Float.floatToIntBits(value)

  def writeCoefficients(
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

  def pokeQuad(
    dut: DepthInterpolator,
    primitiveId: Int,
    lambdas: Seq[(Float, Float)]
  ) = {
    dut.io.rasterizedQuad.bits.primitiveId.poke(primitiveId)
    for (i <- 0 until Consts.pixelsPerQuad) {
      dut.io.rasterizedQuad.bits.lambda(i)(0).raw.poke(floatToRawBits(lambdas(i)._1))
      dut.io.rasterizedQuad.bits.lambda(i)(1).raw.poke(floatToRawBits(lambdas(i)._2))
    }
    dut.io.rasterizedQuad.valid.poke(true)
    dut.clock.step()
    dut.io.rasterizedQuad.valid.poke(false)

    for (i <- 0 until Consts.pixelsPerQuad) {
      dut.io.rasterizedQuad.bits.lambda(i)(0).raw.poke(0)
      dut.io.rasterizedQuad.bits.lambda(i)(1).raw.poke(0)
    }
  }

  def expectDepths(dut: DepthInterpolator, expected: Seq[Float]): Unit = {
    for (i <- 0 until Consts.pixelsPerQuad) {
      val actual = java.lang.Float.intBitsToFloat(
        dut.io.interpolatedQuad.bits.depths(i).raw.peek().litValue.toInt)
      assert(
        math.abs(actual - expected(i)) < 0.00001f,
        s"pixel $i: expected depth ${expected(i)}, got $actual")
    }
  }

  test("DepthInterpolator basic operation") {
    simulate(new DepthInterpolator) { dut =>
      dut.io.interpolatedQuad.ready.poke(true)
      writeCoefficients(dut, 0, (0.2f, 0.6f, 0.8f))
      writeCoefficients(dut, 1, (0.3f, 0.4f, 0.5f))

      pokeQuad(dut, 0, Seq(
        (0.0f, 0.0f),
        (1.0f, 0.0f),
        (0.0f, 1.0f),
        (0.333333f, 0.333333f)))
      pokeQuad(dut, 1, Seq(
        (1.0f, 0.0f),
        (0.0f, 1.0f),
        (0.333333f, 0.333333f),
        (0.0f, 0.0f)))
      dut.clock.step(15) // Two cycles of latency was consumed by pokeQuad

      expectDepths(dut, Seq(0.2f, 0.6f, 0.8f, 0.37894696f))
      dut.clock.step()
      expectDepths(dut, Seq(0.4f, 0.5f, 0.38297862f, 0.3f))
    }
  }
}
