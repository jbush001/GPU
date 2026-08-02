#
#   Copyright 2026 Jeff Bush
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#

import struct

VECTOR_WIDTH = 8
RECIP_BITS = 16
NUM_SCALAR_REGS = 64
NUM_VECTOR_REGS = 64
EXEC_MASK_REG = 32

# The default register type is stored as u32, these functions cast to other types

def reinterp_u2i(value: int) -> int:
    """Interpret a 32-bit unsigned integer as a signed integer."""
    return (value ^ 0x80000000) - 0x80000000

def reinterp_u2f(value: float) -> float:
    """Interpret a 32-bit unsigned integer as a float."""
    return struct.unpack('f', struct.pack('I', value))[0]

def reinterp_f2u(value: float) -> int:
    return struct.unpack('I', struct.pack('f', value))[0] & 0xffffffff

def recip_estimate(value: float) -> int:
    if value == 0.0:
        return reinterp_f2u(float('inf'))

    return reinterp_f2u(1.0 / value) & (0xffffffff << (23 - RECIP_BITS))

def is_scalar_reg(reg_num: int) -> bool:
    return reg_num < NUM_SCALAR_REGS

FMT_RRR = 0
FMT_RR = 1
FMT_CMP = 2
FMT_BRANCH = 3
FMT_K = 4
FMT_X = 5

# (format, operation)
INSTR_TABLE = {
    0b0000000: (FMT_RRR, lambda a, b: a & b), # and
    0b0000001: (FMT_RRR, lambda a, b: a | b), # or
    0b0000010: (FMT_RRR, lambda a, b: a ^ b), # xor
    0b0000011: (FMT_RRR, lambda a, b: a + b), # addi
    0b0000100: (FMT_RRR, lambda a, b: a - b), # subi
    0b0000101: (FMT_RRR, lambda a, b: a * b), # muli
    0b0000110: (FMT_RRR, lambda a, b: a << b), # lsl
    0b0000111: (FMT_RRR, lambda a, b: reinterp_u2i(a) >> b), # asr
    0b0001000: (FMT_RRR, lambda a, b: a >> b), # lsr
    0b0001001: (FMT_RRR, lambda a, b: reinterp_f2u(reinterp_u2f(a) + reinterp_u2f(b))), # addf
    0b0001010: (FMT_RRR, lambda a, b: reinterp_f2u(reinterp_u2f(a) - reinterp_u2f(b))), # subf
    0b0001011: (FMT_RRR, lambda a, b: reinterp_f2u(reinterp_u2f(a) * reinterp_u2f(b))), # mulf
    0b0001100: (FMT_RR, lambda a: recip_estimate(reinterp_u2f(a))), # recip
    0b0001101: (FMT_RR, lambda a: int(reinterp_u2f(a)) & 0xffffffff), # ftoi
    0b0001110: (FMT_RR, lambda a: reinterp_f2u(float(reinterp_u2i(a)))), # itof
    0b0001111: (FMT_CMP, lambda a, b: reinterp_u2f(a) > reinterp_u2f(b)), # setgtf
    0b0010000: (FMT_CMP, lambda a, b: reinterp_u2f(a) < reinterp_u2f(b)), # setltf
    0b0010001: (FMT_CMP, lambda a, b: reinterp_u2i(a) >= reinterp_u2i(b)), # setgei
    0b0010010: (FMT_CMP, lambda a, b: reinterp_u2i(a) < reinterp_u2i(b)), # setlti
    0b0010011: (FMT_CMP, lambda a, b: a >= b), # setgeu
    0b0010100: (FMT_CMP, lambda a, b: a < b), # setltu
    0b0010101: (FMT_CMP, lambda a, b: a == b), # seteq
    0b0010110: (FMT_CMP, lambda a, b: a != b), # setne
    0b1000000: (FMT_BRANCH, lambda a: a != 0), # bnz
    0b1000001: (FMT_BRANCH, lambda a: a == 0), # bz
    0b1000010: (FMT_BRANCH, lambda _: True), # j
    0b1000011: (FMT_K, lambda a, b: (a & 0xffff0000) | b), # loadlo
    0b1000100: (FMT_K, lambda a, b: (a & 0x0000ffff) | (b << 16)), # loadhi
    0b1111111: (FMT_X, None), # halt
}

class Emulator:
    def __init__(self):
        self.registers = [0] * NUM_SCALAR_REGS
        self.registers += [[0] * VECTOR_WIDTH for _ in range(NUM_VECTOR_REGS)]
        self.pc = 0
        self.instructions = []
        self.registers[EXEC_MASK_REG] = 0xff
        self.halted = False

    def load_hex_file(self, filename):
        with open(filename, "r") as f:
            for line in f:
                line = line.strip()
                if not line or line.startswith("#"):
                    continue
                self.instructions.append(int(line, 16))

    def run(self):
        while not self.halted:
            self.execute_instr()

    def execute_instr(self):
        if self.pc >= len(self.instructions):
            raise Exception("PC out of range")

        instr = self.instructions[self.pc]
        self.pc += 1

        opcode = instr & 0x7f
        if opcode not in INSTR_TABLE:
            raise Exception(f"Bad instruction: {opcode:#x}")

        format, operation = INSTR_TABLE[opcode]
        if format == FMT_RRR:
            rd = (instr >> 7) & 0x7f
            rs1 = (instr >> 14) & 0x7f
            rs2 = (instr >> 21) & 0x7f

            if is_scalar_reg(rd):
                if not is_scalar_reg(rs1) or not is_scalar_reg(rs2):
                    raise Exception(f'Illegal source register')

                self.registers[rd] = operation(self.registers[rs1],
                                               self.registers[rs2]) & 0xffffffff
            else:
                if is_scalar_reg(rs1):
                    rs1_value = [self.registers[rs1]] * VECTOR_WIDTH
                else:
                    rs1_value = self.registers[rs1]

                if is_scalar_reg(rs2):
                    rs2_value = [self.registers[rs2]] * VECTOR_WIDTH
                else:
                    rs2_value = self.registers[rs2]

                exec_mask = self.registers[EXEC_MASK_REG]
                for i in range(VECTOR_WIDTH):
                    if (exec_mask >> i) & 1:
                        self.registers[rd][i] = operation(rs1_value[i], rs2_value[i]) & 0xffffffff
        elif format == FMT_RR:
            rd = (instr >> 7) & 0x7f
            rs = (instr >> 14) & 0x7f

            if is_scalar_reg(rd):
                assert(is_scalar_reg(rs))
                self.registers[rd] = operation(self.registers[rs])
            else:
                if is_scalar_reg(rs):
                    rs1_value = [self.registers[rs]] * VECTOR_WIDTH
                else:
                    rs1_value = self.registers[rs]

                exec_mask = self.registers[EXEC_MASK_REG]
                for i in range(VECTOR_WIDTH):
                    if (exec_mask >> i) & 1:
                        self.registers[rd][i] = operation(rs1_value[i]) & 0xffffffff
        elif format == FMT_CMP:
            rd = (instr >> 7) & 0x7f
            rs1 = (instr >> 14) & 0x7f
            rs2 = (instr >> 21) & 0x7f

            assert(is_scalar_reg(rd))
            if is_scalar_reg(rs1):
                rs1_value = [self.registers[rs1]] * VECTOR_WIDTH
            else:
                rs1_value = self.registers[rs1]

            if is_scalar_reg(rs2):
                rs2_value = [self.registers[rs2]] * VECTOR_WIDTH
            else:
                rs2_value = self.registers[rs2]

            result = 0
            for i in range(VECTOR_WIDTH):
                if operation(rs1_value[i], rs2_value[i]):
                    result |= (1 << i)

            self.registers[rd] = result
        elif format == FMT_BRANCH:
            rs = (instr >> 14) & 0x3f
            take_branch = operation(self.registers[rs])
            print(f"take branch = {take_branch}")
            if take_branch:
                raw_offset = ((instr >> 7) & 0x3f) | ((instr >> 20) << 7)
                offset = (raw_offset ^ (1 << 19)) - (1 << 19)  # Sign extend
                self.pc += offset
        elif format == FMT_K:
            # Load constant
            rd = (instr >> 7) & 0x7f
            imm_val = (instr >> 16) & 0xffff
            if is_scalar_reg(rd):
                self.registers[rd] = operation(self.registers[rd], imm_val)
            else:
                exec_mask = self.registers[EXEC_MASK_REG]
                for i in range(VECTOR_WIDTH):
                    if (exec_mask >> i) & 1:
                        self.registers[rd][i] = operation(self.registers[rd][i], imm_val)
        else:
            # Halt
            self.halted = True
