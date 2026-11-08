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
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ArrayBuffer
import scala.sys.process.{Process, ProcessLogger}

class CommandProcessorTests extends AnyFunSuite with ChiselSim with TestHelpers {
  implicit val cfg: GpuConfig = GpuConfig()

  val CompletionAddress = 255

  case class ControlWrite(address: Int, data: BigInt)

  def assembleProgram(source: String): Seq[BigInt] = {
    val testDir = implementation.getDirectory
    val sourcePath = testDir.resolve("test.s")
    val hexPath = testDir.resolve("test.hex")
    val output = new StringBuilder
    val logger = ProcessLogger(
      line => output.append(line).append('\n'),
      line => output.append(line).append('\n')
    )

    Files.write(sourcePath, source.getBytes(StandardCharsets.UTF_8))
    val exitCode = Process(Seq("python3", "tools/assemble-command.py", sourcePath.toString)).!(logger)
    if (exitCode != 0) {
        fail(s"Assembler failed with exit code $exitCode:\n$output")
    }

    Files.readAllLines(hexPath).toArray.toSeq.map(_.toString).map(BigInt(_, 16))
  }

  def loadProgram(dut: CommandProcessor, program: Seq[BigInt]): Unit = {
    dut.io.halt.poke(true)
    dut.io.loadProgram.valid.poke(true)
    for (instr <- program) {
      dut.io.loadProgram.bits.poke(instr)
      dut.clock.step()
    }
    dut.io.loadProgram.valid.poke(false)
    dut.io.halt.poke(false)
  }

  def runProgram(dut: CommandProcessor): Seq[ControlWrite] = {
    dut.io.halt.poke(false)
    val writes = ArrayBuffer[ControlWrite]()
    var cycles = 0
    val maxCycles = 5000

    while (cycles < maxCycles) {
      if (dut.io.controlBus.regWrite.valid.peek().litToBoolean) {
        val writeAddress = dut.io.controlBus.regWrite.bits.address.peek().litValue.toInt
        if (writeAddress == CompletionAddress) {
          return writes.toSeq
        }

        writes += ControlWrite(
          writeAddress,
          dut.io.controlBus.regWrite.bits.data.peek().litValue
        )
      }

      dut.clock.step()
      cycles += 1
    }

    fail(s"Command processor timed out after $maxCycles cycles; writes observed: $writes")
  }

  // Exercises conditional branches, local memory loads/stores, forwarding, compares.
  test("CommandProcessor bubble sort") {
    simulate(new CommandProcessor) { dut =>
      loadProgram(dut, assembleProgram(
        """
          |; Initialize list
          |           clear r0 ; this is always zero
          |           movei r1, 17
          |           sw r1, (r0)
          |           movei r1, 5
          |           sw r1, 1(r0)
          |           movei r1, 13
          |           sw r1, 2(r0)
          |           movei r1, 8
          |           sw r1, 3(r0)
          |           movei r1, 22
          |           sw r1, 4(r0)
          |
          |; Sort loop
          |           movei r2, 4 ; End address
          |outer:     clear r1    ; r1 is the current index
          |inner:     lw r3, 0(r1)
          |           lw r4, 1(r1)
          |           nop
          |           sltu r5, r4, r3
          |           beq r5, r0, noswap
          |           ; swap
          |           sw r3, 1(r1)
          |           sw r4, 0(r1)
          |noswap:    addi r1, r1, 1
          |           bne r1, r2, inner
          |           addi r2, r2, -1
          |           bne r2, r0, outer
          |
          |; Copy out
          |           clear r1      ; source pointer
          |           movei r2, 5   ; end pointer
          |out_loop:  lw r3, (r1)
          |           nop
          |           sw r3, -256(r0) ; write out
          |           addi r1, r1, 1
          |           bne r1, r2, out_loop
          |           sw r4, -1(r0) ; Write end reigster
          |""".stripMargin
      ))

      assert(runProgram(dut) == Seq(
        ControlWrite(0, 5),
        ControlWrite(0, 8),
        ControlWrite(0, 13),
        ControlWrite(0, 17),
        ControlWrite(0, 22),
      ))
    }
  }

  // Exercises subroutine calls, memory access, arithmetic
  test("CommandProcessor fibonacci") {
    simulate(new CommandProcessor) { dut =>
      loadProgram(dut, assembleProgram(
        """
          |           clear r0     ; r0 is always zero
          |           loadi r6, 64 ; Stack pointer
          |           loadi r1, 10
          |           jal r7, fib  ; Call fib(10)
          |           sw r1, -256(r0) ; return result
          |           sw r1, -1(r0) ; signal completion
          |
          |; r1 is argument and result. r6 is the pointer to the base of the
          |; current stack frame. r7 holds the return address.
          |fib:       sltui r2, r1, 2
          |           bne r2, r0, return
          |           addi r6, r6, -3 ; Allocate stack frame
          |           sw r7, 0(r6)    ; Save return address
          |           sw r1, 1(r6)    ; Save original argument
          |
          |           addi r1, r1, -1
          |           jal r7, fib     ; Call fib(n - 1)
          |           sw r1, 2(r6)    ; Save this return value
          |
          |           lw r1, 1(r6)    ; Restore original argument
          |           nop
          |           addi r1, r1, -2
          |           jal r7, fib     ; Call fib(n - 2)
          |
          |           lw r2, 2(r6)    ; Load first result
          |           nop
          |           add r1, r1, r2  ; Add two results together
          |           lw r7, (r6)     ; restore return address
          |           addi r6, r6, 3  ; Deallocate stack frame
          |return:    jalr r7, r7
          |""".stripMargin
      ))

      assert(runProgram(dut) == Seq(ControlWrite(0, 55)))
    }
  }

  test("CommandProcessor bit reverse") {
    simulate(new CommandProcessor) { dut =>
      loadProgram(dut, assembleProgram(
        """
          |           clear r0 ; this is always 0
          |           loadi r1, 0xf00d1234
          |           movei r2, 0 ; result
          |           movei r3, 32 ; loop counter
          |           movei r4, 1 ; mask
          |loop:      slli r2, r2, 1
          |           and r5, r1, r4
          |           or r2, r2, r5
          |           srli r1, r1, 1
          |           addi r3, r3, -1
          |           bne r3, r0, loop
          |; Done, report result
          |           sw r2, -256(r0)
          |           sw r2, -1(r0)
          |""".stripMargin
      ))

      assert(runProgram(dut) == Seq(ControlWrite(0, 0x2c48b00f)))
    }
  }
}

