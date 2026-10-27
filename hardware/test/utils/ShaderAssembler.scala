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

package gpu.shader

/**
  * Simple embedded assembler for shader used to generate test programs.
  */
class ShaderAssembler {
  private val instructions = scala.collection.mutable.ArrayBuffer[Int]()
  private val labels = scala.collection.mutable.Map[String, Int]()
  private val fixups = scala.collection.mutable.ArrayBuffer[(String, Int)]()

  private def addInstruction(instruction: Int): Unit = {
    instructions += instruction
  }

  def rInst(opcode: OpCode.Type, rd: Int, rs1: Int, rs2: Int): this.type = {
    val instruction = opcode.litValue.toInt | (rd << 7) | (rs1 << 15) | (rs2 << 23)
    addInstruction(instruction)
    this
  }

  def bInst(opcode: OpCode.Type, rs1: Int, label: String): this.type = {
    fixups += ((label, instructions.length))
    val instruction = opcode.litValue.toInt | (rs1 << 15)
    addInstruction(instruction)
    this
  }

  def kInst(opcode: OpCode.Type, rd: Int, imm: Int): this.type = {
    val instruction = opcode.litValue.toInt | (rd << 7) | (imm << 16)
    addInstruction(instruction)
    this
  }

  def move(rd: Int, rs: Int): this.type = {
    rInst(OpCode.Or, rd, rs, SpecialReg.Const0)
    this
  }

  def halt(): this.type = {
    rInst(OpCode.Halt, 0, 0, 0)
    this
  }

  def emitLabel(label: String): this.type = {
    if (labels.contains(label)) {
      throw new Exception(s"Label $label already defined")
    }
    labels(label) = instructions.length
    this
  }

  def finish(): Seq[Long] = {
    for ((label, index) <- fixups) {
      val targetIndex = labels.getOrElse(label, throw new Exception(s"Label $label not found"))
      val offset = targetIndex - (index + 1)
      val instruction = instructions(index)
      val newInstruction = instruction | (((offset & 0xff) << 7)
                                          | ((offset >> 8) << 20))
      instructions(index) = newInstruction
    }

    // Need to pack these into 64 bit words
    for (i <- 0 until (instructions.length + 1) / 2) yield {
      val low = instructions(i * 2)
      val high = if (i * 2 + 1 < instructions.length) instructions(i * 2 + 1) else 0
      ((high.toLong << 32) | (low.toLong & 0xffffffffL))
    }
  }
}
