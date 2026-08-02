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

LPM_READ_ADDR = 33
LPM_READ_DATA = 109
LPM_WRITE_ADDR = 34
LPM_WRITE_DATA = 110
UNIFORM_ADDR = 35
UNIFORM_DATA = 36
LANE_ID = 111
CONST_ZERO = 53

# The default register type is stored as u32, these functions cast to other types

def reinterp_u2i(value: int) -> int:
    """Interpret a 32-bit unsigned integer as a signed integer."""
    return (value ^ 0x80000000) - 0x80000000

def reinterp_u2f(value: float) -> float:
    """Interpret a 32-bit unsigned integer as a float."""
    return struct.unpack('f', struct.pack('I', value))[0]

def reinterp_f2u(value: float) -> int:
    """Interpret a float as a 32-bit unsigned integer."""
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

# Each table entry is (format, operation)
# The signature of the passed function is different depending on the format.
# * For RRR instructions, the function takes two scalar arguments (rs1, rs2)
#   and returns the result of the computation.
# * For RR instructions, the function takes one argument (rs) and returns the
#   result.
# * For branch istructions, the function takes one argument (rs) and returns a
#   boolean indicating whether to take the branch.
# * For comparison instructions, the function takes two scalar arguments (rs1, rs2)
#   and returns a boolean indicating whether the comparison is true.
# * For K instructions, the function takes two arguments: the prior value of the
#   destination register and the immediate value, and returns the new value of the
#   destination register.
INSTR_TABLE = {
    0:  (FMT_X,      None), # halt
    1:  (FMT_RRR,    lambda a, b: a & b), # and
    2:  (FMT_RRR,    lambda a, b: a | b), # or
    3:  (FMT_RRR,    lambda a, b: a ^ b), # xor
    4:  (FMT_RRR,    lambda a, b: a + b), # addi
    5:  (FMT_RRR,    lambda a, b: a - b), # subi
    6:  (FMT_RRR,    lambda a, b: a * b), # muli
    7:  (FMT_RRR,    lambda a, b: (a * b) >> 32), # mulih
    8:  (FMT_RRR,    lambda a, b: a << b), # lsl
    9:  (FMT_RRR,    lambda a, b: reinterp_u2i(a) >> b), # asr
    10: (FMT_RRR,    lambda a, b: a >> b), # lsr
    11: (FMT_RRR,    lambda a, b: reinterp_f2u(reinterp_u2f(a) + reinterp_u2f(b))), # addf
    12: (FMT_RRR,    lambda a, b: reinterp_f2u(reinterp_u2f(a) - reinterp_u2f(b))), # subf
    13: (FMT_RRR,    lambda a, b: reinterp_f2u(reinterp_u2f(a) * reinterp_u2f(b))), # mulf
    14: (FMT_RR,     lambda a: recip_estimate(reinterp_u2f(a))), # recip
    15: (FMT_RR,     lambda a: int(reinterp_u2f(a)) & 0xffffffff), # ftoi
    16: (FMT_RR,     lambda a: reinterp_f2u(float(reinterp_u2i(a)))), # itof
    17: (FMT_CMP,    lambda a, b: reinterp_u2f(a) > reinterp_u2f(b)), # setgtf
    18: (FMT_CMP,    lambda a, b: reinterp_u2f(a) < reinterp_u2f(b)), # setltf
    19: (FMT_CMP,    lambda a, b: reinterp_u2i(a) >= reinterp_u2i(b)), # setgei
    20: (FMT_CMP,    lambda a, b: reinterp_u2i(a) < reinterp_u2i(b)), # setlti
    21: (FMT_CMP,    lambda a, b: a >= b), # setgeu
    22: (FMT_CMP,    lambda a, b: a < b), # setltu
    23: (FMT_CMP,    lambda a, b: a == b), # seteq
    24: (FMT_CMP,    lambda a, b: a != b), # setne
    25: (FMT_BRANCH, lambda a: a != 0), # bnz
    26: (FMT_BRANCH, lambda a: a == 0), # bz
    27: (FMT_BRANCH, lambda _: True), # j
    28: (FMT_K,      lambda a, b: (a & 0xffff0000) | b), # loadlo
    29: (FMT_K,      lambda a, b: (a & 0x0000ffff) | (b << 16)), # loadhi
}

class RuntimeFault(Exception):
    def __init__(self, pc, message):
        super().__init__(f'Runtime fault at {pc:#x}: {message}')

def print_reg_name(reg_num):
    if is_scalar_reg(reg_num):
        return f'r{reg_num}'
    else:
        return f'v{reg_num - NUM_SCALAR_REGS}'

class Emulator:
    def __init__(self):
        self.registers = [0] * NUM_SCALAR_REGS
        self.registers += [[0] * VECTOR_WIDTH for _ in range(NUM_VECTOR_REGS)]
        self.pc = 0
        self.instructions = []
        self.registers[EXEC_MASK_REG] = 0xff
        self.halted = False
        self.uniforms = [0] * 1024
        self.uniform_addr = 0
        self.local_parameter_memory = [0] * 1024
        self.lpm_read_addr = 0
        self.lpm_write_addr = 0

        # Set this flag to see all register writes.
        self.trace = False

    def load_hex_file(self, filename):
        with open(filename, "r") as f:
            for line in f:
                line = line.strip()
                if not line or line.startswith("#"):
                    continue
                self.instructions.append(int(line, 16))

    def set_register(self, rd, value):
        if is_scalar_reg(rd):
            if self.trace:
                print(f'r{rd} <= {value:08x}')

            if rd == LPM_READ_ADDR:
                self.lpm_read_addr = value
            elif rd == LPM_WRITE_ADDR:
                self.lpm_write_addr = value
            elif rd == UNIFORM_ADDR:
                self.uniform_addr = value
            else:
                self.registers[rd] = value
        else:
            if self.trace:
                print(f'v{rd - NUM_SCALAR_REGS} <= {" ".join(f"{x:08x}" for x in value)} mask={self.registers[EXEC_MASK_REG]:08b}')

            if rd == LPM_WRITE_DATA:
                for i in range(VECTOR_WIDTH):
                    if (self.registers[EXEC_MASK_REG] >> i) & 1:
                        self.local_parameter_memory[self.lpm_write_addr] = value[i]

                    self.lpm_write_addr += 1
            else:
                exec_mask = self.registers[EXEC_MASK_REG]
                for i in range(VECTOR_WIDTH):
                    if (exec_mask >> i) & 1:
                        self.registers[rd][i] = value[i]

    def get_register(self, reg_num):
        if reg_num == LPM_READ_DATA:
            result = []
            for i in range(VECTOR_WIDTH):
                result.append(self.local_parameter_memory[self.lpm_read_addr + i])

            self.lpm_read_addr = self.lpm_read_addr + VECTOR_WIDTH
            return result

        if reg_num == UNIFORM_DATA:
            result = self.uniforms[self.uniform_addr]
            self.uniform_addr += 1
            return result

        if reg_num == CONST_ZERO:
            return 0

        if reg_num == LANE_ID:
            return [i for i in range(VECTOR_WIDTH)]

        if reg_num == UNIFORM_ADDR:
            return self.uniform_addr

        if reg_num == LPM_READ_ADDR:
            return self.lpm_read_addr

        if reg_num == LPM_WRITE_ADDR:
            return self.lpm_write_addr

        return self.registers[reg_num]

    def run(self):
        while not self.halted:
            self.execute_instr()

    def execute_instr(self):
        if self.pc >= len(self.instructions):
            raise RuntimeFault(self.pc, "PC out of range")

        instr = self.instructions[self.pc]
        self.pc += 1

        opcode = instr & 0x7f
        if opcode not in INSTR_TABLE:
            raise RuntimeFault(self.pc, f"Illegal instruction {opcode:#x}")

        format, operation = INSTR_TABLE[opcode]
        if format == FMT_RRR:
            rd = (instr >> 7) & 0x7f
            rs1 = (instr >> 14) & 0x7f
            rs2 = (instr >> 21) & 0x7f

            if is_scalar_reg(rd):
                if not is_scalar_reg(rs1):
                    raise RuntimeFault(self.pc,
                        f'Illegal source register {print_reg_name(rs1)} for scalar destination {print_reg_name(rd)}')

                if not is_scalar_reg(rs2):
                    raise RuntimeFault(self.pc,
                        f'Illegal source register {print_reg_name(rs2)} for scalar destination {print_reg_name(rd)}')

                self.set_register(rd, operation(self.get_register(rs1),
                    self.get_register(rs2)) & 0xffffffff)
            else:
                # Vector destination
                if is_scalar_reg(rs1):
                    rs1_value = [self.get_register(rs1)] * VECTOR_WIDTH
                else:
                    rs1_value = self.get_register(rs1)

                if is_scalar_reg(rs2):
                    rs2_value = [self.get_register(rs2)] * VECTOR_WIDTH
                else:
                    rs2_value = self.get_register(rs2)

                result = [operation(a, b) & 0xffffffff for a, b in zip(rs1_value, rs2_value)]
                self.set_register(rd, result)
        elif format == FMT_RR:
            rd = (instr >> 7) & 0x7f
            rs = (instr >> 14) & 0x7f

            if is_scalar_reg(rd):
                if not is_scalar_reg(rs):
                    raise RuntimeFault(self.pc,
                        f'Illegal source register {print_reg_name(rs)} for scalar destination {print_reg_name(rd)}')

                self.set_register(rd, operation(self.get_register(rs)) & 0xffffffff)
            else:
                if is_scalar_reg(rs):
                    rs1_value = [self.get_register(rs)] * VECTOR_WIDTH
                else:
                    rs1_value = self.get_register(rs)

                result = [operation(a) & 0xffffffff for a in rs1_value]
                self.set_register(rd, result)
        elif format == FMT_CMP:
            rd = (instr >> 7) & 0x7f
            rs1 = (instr >> 14) & 0x7f
            rs2 = (instr >> 21) & 0x7f

            if not is_scalar_reg(rd):
                raise RuntimeFault(self.pc,
                    f'Illegal destination register {print_reg_name(rd)} for comparison instruction')

            if is_scalar_reg(rs1):
                rs1_value = [self.get_register(rs1)] * VECTOR_WIDTH
            else:
                rs1_value = self.get_register(rs1)

            if is_scalar_reg(rs2):
                rs2_value = [self.get_register(rs2)] * VECTOR_WIDTH
            else:
                rs2_value = self.get_register(rs2)

            result = 0
            for i in range(VECTOR_WIDTH):
                if operation(rs1_value[i], rs2_value[i]):
                    result |= (1 << i)

            self.set_register(rd, result)
        elif format == FMT_BRANCH:
            rs = (instr >> 14) & 0x3f
            take_branch = operation(self.get_register(rs))
            if take_branch:
                raw_offset = ((instr >> 7) & 0x3f) | ((instr >> 20) << 7)
                offset = (raw_offset ^ (1 << 19)) - (1 << 19)  # Sign extend
                self.pc += offset
        elif format == FMT_K:
            # Load constant
            rd = (instr >> 7) & 0x7f
            imm_val = (instr >> 16) & 0xffff
            if is_scalar_reg(rd):
                self.set_register(rd, operation(self.get_register(rd), imm_val))
            else:
                result = [operation(self.get_register(rd)[i], imm_val) for i in range(VECTOR_WIDTH)]
                self.set_register(rd, result)
        else:
            # Halt
            self.halted = True
