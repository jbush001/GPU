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

class FloatingPointTests extends AnyFunSuite with ChiselSim {
  // Helper for reciprocal tests
  def fpTruncate(value: Float): Float =
    java.lang.Float.intBitsToFloat(java.lang.Float.floatToIntBits(value) & 0xfffe0000)

  def floatToRawBits(fval: Float) = java.lang.Float.floatToIntBits(fval) & 0xffffffffL

def floatBitsMatch(expected: Float, actualBits: Long, maxUlps: Long = 1): Boolean = {
  val actualFloat = java.lang.Float.intBitsToFloat(actualBits.toInt)

  if (expected.isNaN) {
    actualFloat.isNaN
  } else if (actualFloat.isNaN) {
    false
  } else {
    def toLexicographical(bits: Int): Long =
      if (bits < 0) (~bits) & 0xffffffffL else (bits ^ 0x80000000) & 0xffffffffL

    val expOrd = toLexicographical(java.lang.Float.floatToRawIntBits(expected))
    val actOrd = toLexicographical(actualBits.toInt)

    math.abs(expOrd - actOrd) <= maxUlps
  }
}
  /** This manages pipelining test operations, inserting the appropriate delay
    * between issueing requests and checking their results.
    */
  def runFpPipelineTest[T <: Module, V](
    dut: T,
    pipelineDelay: Int,
    testVectors: Seq[V],
    driveInput: (T, V) => Unit,
    verifyOutput: (T, V, Int) => Unit
  ) = {
    for (cycle <- 0 until testVectors.length + pipelineDelay) {
      if (cycle < testVectors.length) {
        driveInput(dut, testVectors(cycle))
      }

      if (cycle >= pipelineDelay) {
        verifyOutput(dut, testVectors(cycle - pipelineDelay), cycle - pipelineDelay)
      }

      dut.clock.step()
    }
  }

  def reportTestFailure(index: Int, a: Float, b: Float, expected: Float, actual: Float) = {
    println(f"mismatch at test entry $index: $a%.3f $b%.3f")
    println(f"  expected = $expected%.6f  (0x${this.floatToRawBits(expected)}%08x)")
    println(f"  actual   = $actual%.6f  (0x${this.floatToRawBits(actual)}%08x)")
  }

  test("FpAdd") {
   simulate(new FpAdd()) { dut =>
      type TestVector = (Float, Float, Float)
      val testVectors: Seq[TestVector] = Seq(
        (3.0f, 2.0f, 5.0f), // pos + pos
        (-4.0f, -5.0f, -9.0f), // neg + neg
        (7.7f, -3.5f, 4.2f), // pos + smaller neg
        (7.0f, -13.0f, -6.0f), // pos + larger neg
        (-40.0f, 37.0f, -3.0f), // neg + smaller pos
        (-27.0f, 35.0f, 8.0f), // neg + larger pos
        (5.0f, -5.0f, 0.0f), // Exact cancellation
        (-5.0f, 5.0f, 0.0f),
        (17.79f, 19.32f, 37.11f), // Exponents equal. Will carry into next significand bit
        (0.34f, 44.23f, 44.57f), // Exponent 2 larger
        (44.23f, 0.034f, 44.264f), // Exponent 1 larger
        (5.0f, 0.0f, 5.0f), // Zero identity
        (0.0f, 5.0f, 5.0f), // " "
        (0.0f, 0.0f, 0.0f), // " "
        (1000000.0f, 0.0000001f, 1000000.0f), //  Second op is lost because of precision
        (0.0000001f, 0.00000001f, 0.00000011f), // Very small number
        (1000000.0f, 10000000.0f, 11000000.0f), // Large number
        (-0.0f, 2.323f, 2.323f), // negative zero
        (2.323f, -0.0f, 2.323f), // negative zero
        (Float.PositiveInfinity, Float.PositiveInfinity, Float.PositiveInfinity), // Infinity and NaN cases...
        (Float.PositiveInfinity, 1.0f, Float.PositiveInfinity),
        (Float.NegativeInfinity, 1.0f, Float.NegativeInfinity),
        (0.0f, Float.NegativeInfinity, Float.NegativeInfinity),
        (1.0f, Float.PositiveInfinity, Float.PositiveInfinity),
        (1.0f, Float.NegativeInfinity, Float.NegativeInfinity),
        (Float.PositiveInfinity, Float.NegativeInfinity, Float.NaN),
        (Float.NaN, 1.0f, Float.NaN),
        (1.0f, Float.NaN, Float.NaN),
        (Float.NaN, Float.NaN, Float.NaN),
        (java.lang.Float.MIN_NORMAL, java.lang.Float.MIN_NORMAL,
          java.lang.Math.scalb(1.0f, -125)), // Smallest normal values add without underflow
        (java.lang.Math.nextAfter(1.0f, 0.0f), java.lang.Math.nextAfter(1.0f, 0.0f),
          java.lang.Math.nextAfter(java.lang.Math.nextAfter(2.0f, 0.0f), 0.0f)), // Addition just below the 2.0 boundary
        (java.lang.Math.nextAfter(1.0f, 2.0f), -1.0f,
          java.lang.Math.scalb(1.0f, -23)), // Cancellation leaves one representable residue
        (java.lang.Float.MAX_VALUE, java.lang.Float.MAX_VALUE,
          Float.PositiveInfinity), // Largest finite values overflow
        (Float.NegativeInfinity, Float.NegativeInfinity,
          Float.NegativeInfinity), // Same-sign negative infinities remain infinite
        (0.0f, -0.0f, 0.0f), // Opposite signed zeroes produce positive zero

        // Random real-world numbers
        (224.78541564941406f, 971.5863647460938f, 1196.371826171875f),
        (-74.77526092529297f, 24.81484603881836f, -49.96041488647461f),
        (-740.80712890625f, 128.89651489257812f, -611.91064453125f),
        (-274.5276794433594f, -373.82733154296875f, -648.35498046875f),
        (314.9764099121094f, -504.24749755859375f, -189.27108764648438f),
        (-547.7982788085938f, 239.33090209960938f, -308.4673767089844f),
        (-978.55322265625f, -693.2662963867188f, -1671.819580078125f),
        (-955.0369873046875f, 729.0521850585938f, -225.98480224609375f),
        (966.34765625f, -385.4020690917969f, 580.945556640625f),
        (-884.7400512695312f, -629.8983764648438f, -1514.638427734375f),
        (-95.3381576538086f, 118.1715316772461f, 22.8333740234375f),
        (-355.2882995605469f, -608.7131958007812f, -964.00146484375f),
        (-822.39208984375f, -846.8786010742188f, -1669.270751953125f),
        (3.520339012145996f, -379.3164978027344f, -375.7961730957031f),
        (-801.3521118164062f, 298.9920349121094f, -502.3600769042969f),
        (-144.6096649169922f, -568.098388671875f, -712.7080688476562f),
        (268.8532409667969f, -768.6394653320312f, -499.7862243652344f),
        (-334.4381103515625f, -107.95967102050781f, -442.39776611328125f),
        (1.7076239585876465f, -603.3707275390625f, -601.6630859375f),
        (-433.1484680175781f, 88.93685913085938f, -344.21160888671875f),
      )

      dut.io.addend1.raw.poke(0)
      dut.io.addend2.raw.poke(0)
      dut.io.en.poke(1)

      runFpPipelineTest(
        dut,
        3,
        testVectors,
        (dut:FpAdd, test: TestVector) => {
          dut.io.addend1.raw.poke(this.floatToRawBits(test._1))
          dut.io.addend2.raw.poke(this.floatToRawBits(test._2))
        },
        (dut: FpAdd, test: TestVector, index: Int) => {
          val actualBits: Long = dut.io.result.raw.peek().litValue.toLong & 0xffffffffL
          if (!floatBitsMatch(test._3, actualBits)) {
            reportTestFailure(index, test._1, test._2, test._3,
              java.lang.Float.intBitsToFloat(actualBits.toInt))
            fail()
          }
        }
      )
    }
  }

  test("FpMul") {
    simulate(new FpMul()) { dut =>
      type TestVector = (Float, Float, Float)
      val testVectors: Seq[TestVector] = Seq(
        ( 100.0f, 25.0f, 2500.0f), // positive * positive
        ( -10.0f, 32.0f, -320.0f), // negative * positive
        ( 0.5f, -90.0f, -45.0f), // positive * negative
        ( -15.0f, -4.0f, 60.0f), // negative * negative
        ( -15.0f, 0.0f, -0.0f), // zero identity, negative
        ( 0.0f, 15.0f, 0.0f), // zero identity, positive
        ( 0.00001f, 12345.0f, 0.12345f),
        ( 200000.5f, 123.0f, 24600061.5f),
        ( 1.0E25f, 1.0E25f, Float.PositiveInfinity), // Overflow
        ( 1.0E-20f, 1.0E-20f, 0.0f), // Underflow
        ( Float.PositiveInfinity, Float.PositiveInfinity, Float.PositiveInfinity), // Infinity and NaN cases...
        ( Float.PositiveInfinity, 1.0f, Float.PositiveInfinity),
        ( Float.NegativeInfinity, 1.0f, Float.NegativeInfinity),
        ( Float.NegativeInfinity, -1.0f, Float.PositiveInfinity),
        ( Float.PositiveInfinity, -1.0f, Float.NegativeInfinity),
        ( 0.0f, Float.NegativeInfinity, Float.NaN),
        ( 0.0f, Float.PositiveInfinity, Float.NaN),
        ( Float.NegativeInfinity, 0.0f, Float.NaN),
        ( Float.PositiveInfinity, 0.0f, Float.NaN),
        ( Float.PositiveInfinity, Float.NegativeInfinity, Float.NegativeInfinity),
        ( Float.NaN, 1.0f, Float.NaN),
        ( 1.0f, Float.NaN, Float.NaN),
        ( Float.NaN, Float.NaN, Float.NaN),
        ( Float.NaN, Float.PositiveInfinity, Float.NaN),
        ( Float.PositiveInfinity, Float.NaN, Float.NaN),
        ( Float.NegativeInfinity, Float.NaN, Float.NaN),
        ( Float.NaN, Float.NegativeInfinity, Float.NaN),
        (java.lang.Float.MAX_VALUE, 1.0f,
          java.lang.Float.MAX_VALUE), // Largest finite value times one is unchanged
        (java.lang.Float.MIN_NORMAL, 0.5f,
          0.0f), // Smallest normal value underflows when halved
        (java.lang.Float.MIN_VALUE, 2.0f,
          0.0f), // Subnormal input is treated as zero
        (-0.0f, -1.0f, 0.0f), // Signed zero result follows operand sign
        (-0.0f, 1.0f, -0.0f), // Negative zero times positive one stays negative
        (Float.NegativeInfinity, -1.0f,
          Float.PositiveInfinity), // Negative infinity times negative one is positive
        (Float.PositiveInfinity, -0.0f, Float.NaN), // Infinity times zero is invalid
        (Float.NegativeInfinity, Float.NegativeInfinity,
          Float.PositiveInfinity), // Two negative infinities multiply positively

        // FIXME
        //(java.lang.Float.MAX_VALUE, 2.0f,
        //  Float.PositiveInfinity), // Largest finite value times two overflows

        // Random real-world numbers
        (-167.08126831054688f, -773.0420532226562f, 129160.84375f),
        (253.1892852783203f, -548.824462890625f, -138956.46875f),
        (-502.2134704589844f, 598.0180053710938f, -300332.6875f),
        (-987.9479370117188f, -639.246826171875f, 631542.5625f),
        (584.0684204101562f, 80.51570892333984f, 47026.68359375f),
        (947.8267822265625f, -639.8771362304688f, -606492.6875f),
        (202.74545288085938f, -334.1349182128906f, -67744.3359375f),
        (886.7651977539062f, 514.4378051757812f, 456185.53125f),
        (-937.6377563476562f, -635.3958740234375f, 595771.1875f),
        (888.47607421875f, 254.92257690429688f, 226492.609375f),
        (565.4705810546875f, 273.5024719238281f, 154657.609375f),
        (712.580078125f, -717.5458374023438f, -511308.875f),
        (-235.69676208496094f, 691.8612060546875f, -163069.453125f),
        (-14.650430679321289f, 50.74040222167969f, -743.3687744140625f),
        (161.2047576904297f, -887.080810546875f, -143001.640625f),
        (-481.0034484863281f, 621.6049194335938f, -298994.125f),
        (-169.6221923828125f, -162.37887573242188f, 27543.060546875f),
        (-705.5831909179688f, 914.3872680664062f, -645176.3125f),
        (-745.9539184570312f, -418.518310546875f, 312195.375f),
        (-179.07528686523438f, -352.3625183105469f, 63099.41796875f),
      )

      dut.io.en.poke(1)

      runFpPipelineTest(
        dut,
        2,
        testVectors,
        (dut: FpMul, test: TestVector) => {
          dut.io.multiplier.raw.poke(this.floatToRawBits(test._1))
          dut.io.multiplicand.raw.poke(this.floatToRawBits(test._2))
        },
        (dut: FpMul, test: TestVector, index: Int) => {
          val actualBits: Long = dut.io.product.raw.peek().litValue.toLong & 0xffffffffL
          if (!floatBitsMatch(test._3, actualBits)) {
            reportTestFailure(index, test._1, test._2, test._3,
              java.lang.Float.intBitsToFloat(actualBits.toInt))
            fail()
          }
        }
      )
    }
  }

  test("Float32 reciprocalEstimate") {
    simulate(new Module {
      val io = IO(new Bundle {
        val operand = Input(Float32())
        val result = Output(Float32())
      })

      io.result := Float32(io.operand.raw).reciprocalEstimate()
    }) { dut =>
      type TestVector = (Float, Float)
      val testVectors: Seq[TestVector] = Seq(
        (1.0f, 0.9921875f), // A bit of a hack, see ReciprocalLut
        (2.0f, 0.49609375f),
        (0.5f, 1.984375f),
        (3.0f, this.fpTruncate(0.333333f)),
        (1.5f, this.fpTruncate(0.666666f)),
        (0.333333f, this.fpTruncate(3.0f)),
        (1000.0f, this.fpTruncate(0.001f)),
        (0.99999f, 1.0f), // Last table entry
        (java.lang.Float.MIN_VALUE, Float.PositiveInfinity), // Smallest subnormal is treated as zero
        (0.0f, Float.PositiveInfinity), // Division by zero
        (-0.0f, Float.NegativeInfinity),
        (Float.NaN, Float.NaN), // Divison by NaN
        (Float.NegativeInfinity, -0.0f), // Division by inf
        (Float.PositiveInfinity, 0.0f), // Division by inf
      )
      dut.io.operand.raw.poke(0)
      dut.clock.step() // Wait for reset to complete

      for ((a, expected) <- testVectors) {
        dut.io.operand.raw.poke(floatToRawBits(a).U)
        dut.clock.step()
        val actualBits: Long = dut.io.result.raw.peek().litValue.toLong & 0xffffffffL
        if (!floatBitsMatch(expected, actualBits)) {
          println(f"mismatch: $a%.3f reciprocal, expected $expected%.3f actual ${java.lang.Float.intBitsToFloat(dut.io.result.raw.peek().litValue.toInt)} (0x${dut.io.result.raw.peek().litValue.toInt}%08x)")
          fail()
        }
      }
    }
  }

  test("Float32 toFixedPoint") {
    simulate(new Module {
      val io = IO(new Bundle {
        val a = Input(UInt(32.W))
        val result0 = Output(SInt(32.W))
        val result16_16 = Output(SInt(32.W))
      })

      io.result0 := io.a.asTypeOf(Float32()).toFixedPoint()
      io.result16_16 := io.a.asTypeOf(Float32()).toFixedPoint(16)
    }) { dut =>
      val testVectors = Seq(
        (1.0f, 1, (1 << 16)),
        (-1.0f, -1, -(1 << 16)),
        (0.0f, 0, 0),
        (1234.56f, 1234, (1234.56f * 0x10000).toInt),
        (0.1234f, 0, (0.1234f * 0x10000).toInt),
        (-5543.1f, -5543, (-5543.1f * 0x10000).toInt),
        (Float.NaN, 0, 0),
        (Float.PositiveInfinity, Int.MaxValue, Int.MaxValue),
        (Float.NegativeInfinity, Int.MinValue, Int.MinValue),
        (2000000000.0f, 2000000000, Int.MaxValue),
        (-2000000000.0f, -2000000000, Int.MinValue),
        (3000000000.0f, Int.MaxValue, Int.MaxValue),
        (-3000000000.0f, Int.MinValue, Int.MinValue),
        (1E+30f, Int.MaxValue, Int.MaxValue),
        (-1E+30f, Int.MinValue, Int.MinValue),
        (1E-30f, 0, 0),
        (-1E-30f, 0, 0),
        (1.99999f, 1, 131071), // Conversion truncates toward zero for positive fractions
        (-1.99999f, -1, -131071), // Conversion truncates toward zero for negative fractions
        (32767.996f, 32767, 2147483392), // Value just below 16.16 positive saturation
        (32768.0f, 32768, Int.MaxValue), // 16.16 positive overflow saturates
        (-32768.0f, -32768, Int.MinValue), // Exact negative 16.16 endpoint is representable
        (1073741824.0f, 1073741824, Int.MaxValue), // Largest useful integer-range fixed-point value
        (2147483648.0f, Int.MaxValue, Int.MaxValue), // Integer conversion overflow saturates
      )

      for ((a, expected0, expected16_16) <- testVectors) {
        dut.io.a.poke(floatToRawBits(a).U)
        dut.clock.step()
        if (dut.io.result0.peek().litValue.toInt != expected0) {
          println(f"mismatch: $a%.3f to int, expected $expected0 actual ${dut.io.result0.peek().litValue.toInt}")
          fail()
        }
        if (dut.io.result16_16.peek().litValue.toInt != expected16_16) {
          println(f"mismatch: $a%.3f to 16.16 fixed point, expected $expected16_16 actual ${dut.io.result16_16.peek().litValue.toInt}")
          fail()
        }
      }
    }
  }

  test("Float32 toUnorm") {
    simulate(new Module {
      val io = IO(new Bundle {
        val a = Input(UInt(32.W))
        val result = Output(UInt(32.W))
      })

      io.result := io.a.asTypeOf(Float32()).toUnorm(12)
    }) { dut =>
      val testVectors = Seq(
        (0.0f, 0x000),
        (0.25f, 0x3ff),
        (0.5f, 0x7ff),
        (0.75f, 0xbff),
        (1.0f, 0xfff),
        (2.0f, 0xfff),
        (-1.0f, 0x000),
        (Float.NaN, 0xfff),
        (Float.PositiveInfinity, 0xfff),
        (Float.NegativeInfinity, 0x000)
      )

      for ((a, expected) <- testVectors) {
        dut.io.a.poke(floatToRawBits(a).U)
        dut.clock.step()
        if (dut.io.result.peek().litValue.toInt != expected) {
          println(f"mismatch: $a%.3f to unorm, expected $expected actual ${dut.io.result.peek().litValue.toInt}")
          fail()
        }
      }
    }
  }

  // Whole number
  test("Float32 fromFixedPoint") {
    simulate(new Module {
      val io = IO(new Bundle {
        val a = Input(SInt(32.W))
        val result = Output(UInt(32.W))
      })

      io.result := Float32.fromFixedPoint(io.a).raw
    }) { dut =>
      val testVectors = Seq(
        (1, 1.0f),
        (2, 2.0f),
        (-1, -1.0f),
        (-2, -2.0f),
        (1000, 1000.0f),
        (-1000, -1000.0f),
        (0, 0.0f),
        (Int.MaxValue, Int.MaxValue.toFloat),
        (Int.MinValue, Int.MinValue.toFloat),
        (0x7fffff, 8388607.0f), // Maximum positive input preserves its integer magnitude
        (-0x7fffff, -8388607.0f), // Maximum negative input preserves its integer magnitude
      )

      for ((a, expected) <- testVectors) {
        dut.io.a.poke(a.S)
        dut.clock.step()
        val actual = dut.io.result.peek().litValue.toLong & 0xffffffffL
        if (!floatBitsMatch(expected, actual)) {
          println(f"mismatch: int to fp, expected $expected actual ${java.lang.Float.intBitsToFloat(dut.io.result.peek().litValue.toInt)} (0x${dut.io.result.peek().litValue.toInt}%08x)")
          fail()
        }
      }
    }
  }

  // 16.16
  test("Float32 fromFixedPoint2") {
    simulate(new Module {
      val io = IO(new Bundle {
        val a = Input(SInt(32.W))
        val result = Output(UInt(32.W))
      })

      io.result := Float32.fromFixedPoint(io.a, 16).raw
    }) { dut =>
      val testVectors = Seq(
        (0x10000, 1.0f),
        (0x20000, 2.0f),
        (-0x10000, -1.0f),
        (-0x20000, -2.0f),
        (0x500, 0.01953125f),
        (-0x500, -0.01953125f),
        (0, 0.0f),
        (Int.MaxValue, 32767.998f),
        (Int.MinValue, -32768.0f),
        (1, 0.0000152587890625f), // One fixed-point unit becomes one 2^-16 step
        (-1, -0.0000152587890625f), // Negative fixed-point unit keeps its sign
        (0x7fffff, 127.9999847f), // Positive value near the 16.16 upper range
        (-0x7fffff, -127.9999847f), // Negative value near the 16.16 lower range
      )

      for ((a, expected) <- testVectors) {
        dut.io.a.poke(a.S)
        dut.clock.step()
        val actual = dut.io.result.peek().litValue.toLong & 0xffffffffL
        if (!floatBitsMatch(expected, actual)) {
          println(f"mismatch: int to fp, expected $expected actual ${java.lang.Float.intBitsToFloat(dut.io.result.peek().litValue.toInt)} (0x${dut.io.result.peek().litValue.toInt}%08x)")
          fail()
        }
      }
    }
  }

  test("Float32 greaterThan") {
    simulate(new Module {
      val io = IO(new Bundle {
        val a = Input(UInt(32.W))
        val b = Input(UInt(32.W))
        val result = Output(Bool())
      })

      io.result := io.a.asTypeOf(Float32()) > io.b.asTypeOf(Float32())
    }) { dut =>
      val testVectors = Seq(
        (1.0f, 2.0f, false),
        (2.0f, 1.0f, true),
        (1.0f, 1.0f, false),
        (-1.0f, 1.0f, false),
        (1.0f, -1.0f, true),
        (-1.0f, -2.0f, true),
        (-2.0f, -1.0f, false),
        (690.0439453125f, 690.0439453125f, false),
        (690.0439453126f, 689.0439453125f, true),
        (+0.0f, -0.0f, false), // These are effectively equal
        (-0.0f, +0.0f, false),
        (Float.PositiveInfinity, 1.0f, true),
        (1.0f, Float.PositiveInfinity, false),
        (Float.NegativeInfinity, 1.0f, false),
        (1.0f, Float.NegativeInfinity, true),
        (Float.PositiveInfinity, Float.NegativeInfinity, true),
        (Float.NegativeInfinity, Float.PositiveInfinity, false),
        (Float.NaN, 1.0f, false), // NaN should always return false for comparisons
        (1.0f, Float.NaN, false),
        (Float.NaN, Float.NaN, false),
        (java.lang.Math.nextAfter(1.0f, 2.0f), 1.0f,
          true), // The next representable value above one compares greater
        (1.0f, java.lang.Math.nextAfter(1.0f, 2.0f),
          false), // One compares less than its next representable value
        (java.lang.Float.MAX_VALUE, Float.PositiveInfinity,
          false), // Largest finite value is below positive infinity
        (Float.NegativeInfinity, -java.lang.Float.MAX_VALUE,
          false), // Negative infinity is below every finite value
        (-java.lang.Float.MAX_VALUE, Float.NegativeInfinity,
          true), // Largest finite negative value is above negative infinity
      )

      for ((a, b, expected) <- testVectors) {
        dut.io.a.poke(floatToRawBits(a).U)
        dut.io.b.poke(floatToRawBits(b).U)
        dut.clock.step()
        if (dut.io.result.peek().litToBoolean != expected) {
          println(f"mismatch: $a%.3f > $b%.3f, expected $expected actual ${dut.io.result.peek().litToBoolean}")
          fail()
        }
      }
    }
  }

  test("Float32 abs") {
    simulate(new Module {
      val io = IO(new Bundle {
        val a = Input(UInt(32.W))
        val result = Output(UInt(32.W))
      })

      io.result := io.a.asTypeOf(Float32()).abs.raw
    }) { dut =>
      val testVectors = Seq(
        (-1.0f, 1.0f),
        (1.0f, 1.0f),
        (-1234.56f, 1234.56f),
        (1234.56f, 1234.56f),
        (-0.0f, 0.0f),
        (0.0f, 0.0f),
        (Float.NaN, Float.NaN),
        (Float.PositiveInfinity, Float.PositiveInfinity),
        (Float.NegativeInfinity, Float.PositiveInfinity)
      )

      for ((a, expected) <- testVectors) {
        dut.io.a.poke(floatToRawBits(a).U)
        dut.clock.step()
        val actual = dut.io.result.peek().litValue.toLong & 0xffffffffL
        if (!floatBitsMatch(expected, actual)) {
          println(f"mismatch: abs($a%.3f), expected $expected actual ${java.lang.Float.intBitsToFloat(dut.io.result.peek().litValue.toInt)} (0x${dut.io.result.peek().litValue.toInt}%08x)")
          fail()
        }
      }
    }
  }

  test("FpReciprocal") {
    simulate(new FpReciprocal) { dut =>
      type TestVector = (Float, Float)
      val testVectors: Seq[TestVector] = Seq(
        (1.0f, 1.0f),
        (-1.0f, -1.0f),
        (0.5f, 2.0f),
        (2.0f, 0.5f),
        (10.0f, 0.1f),
        (0.1f, 10.0f),
        (0.33f, 3.030303f),
        (0.65f, 1.5384618f),
        (0.001f, 1000.0f),
        (1.234f, 0.8103729f),
        (java.lang.Math.nextAfter(1.0f, 2.0f), 0.9999999f), // Reciprocal just above one
        (java.lang.Math.nextAfter(2.0f, 0.0f), 0.50000006f), // Reciprocal just below one half
        (java.lang.Float.MIN_NORMAL, 1.0f / java.lang.Float.MIN_NORMAL), // Reciprocal of the smallest normal
        (-0.5f, -2.0f), // Negative reciprocal preserves sign
        (-0.0f, Float.NegativeInfinity),
        (0.0f, Float.PositiveInfinity),
        (Float.PositiveInfinity, 0.0f),
        (Float.NegativeInfinity, -0.0f),
        (Float.NaN, Float.NaN)

        // FIXME
        //(java.lang.Float.MAX_VALUE, 1.0f / java.lang.Float.MAX_VALUE), // Reciprocal of the largest finite value
      )

      dut.io.en.poke(1)

      runFpPipelineTest(
        dut,
        5,
        testVectors,
        (dut: FpReciprocal, test: TestVector) => {
          dut.io.divisor.raw.poke(this.floatToRawBits(test._1))
        },
        (dut: FpReciprocal, test: TestVector, _) => {
          val actualBits: Long = dut.io.reciprocal.raw.peek().litValue.toLong & 0xffffffffL
          if (!floatBitsMatch(test._2, actualBits, 2)) {
            println(f"mismatch: ${test._1}%.3f reciprocal, expected ${test._2} actual ${java.lang.Float.intBitsToFloat(dut.io.reciprocal.raw.peek().litValue.toInt)} (0x${dut.io.reciprocal.raw.peek().litValue.toInt}%08x)")
            fail()
          }
        }
      )
    }
  }
}
