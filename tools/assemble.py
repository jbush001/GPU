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
import sys
from pathlib import Path

FMT_RRR = 0
FMT_RR = 1
FMT_COMPARE = 2
FMT_COND_BRANCH = 3
FMT_UNCOND_BRANCH = 4
FMT_CONST = 5
FMT_INHERENT = 6

# Each entry is: opcode, format, swap_operands
INSTRS = {
    'halt':   (0,  FMT_INHERENT,      False),
    'and':    (1,  FMT_RRR,           False),
    'or':     (2,  FMT_RRR,           False),
    'xor':    (3,  FMT_RRR,           False),
    'addi':   (4,  FMT_RRR,           False),
    'subi':   (5,  FMT_RRR,           False),
    'muli':   (6,  FMT_RRR,           False),
    'mulih':  (7,  FMT_RRR,           False),
    'lsl':    (8,  FMT_RRR,           False),
    'asr':    (9,  FMT_RRR,           False),
    'lsr':    (10, FMT_RRR,           False),
    'addf':   (11, FMT_RRR,           False),
    'subf':   (12, FMT_RRR,           False),
    'mulf':   (13, FMT_RRR,           False),
    'recip':  (14, FMT_RR,            False),
    'ftoi':   (15, FMT_RR,            False),
    'itof':   (16, FMT_RR,            False),
    'setgtf': (17, FMT_COMPARE,       False),
    'setgei': (19, FMT_COMPARE,       False),
    'setlei': (19, FMT_COMPARE,       True),
    'setlti': (20, FMT_COMPARE,       False),
    'setgti': (20, FMT_COMPARE,       True),
    'setgeu': (21, FMT_COMPARE,       False),
    'setleu': (21, FMT_COMPARE,       True),
    'setltu': (22, FMT_COMPARE,       False),
    'setgtu': (22, FMT_COMPARE,       True),
    'seteq':  (23, FMT_COMPARE,       False),
    'setne':  (24, FMT_COMPARE,       False),
    'bnz':    (25, FMT_COND_BRANCH,   False),
    'bz':     (26, FMT_COND_BRANCH,   False),
    'j':      (27, FMT_UNCOND_BRANCH, False),
    'fmin':   (30, FMT_RRR,           False),
    'fmax':   (31, FMT_RRR,           False),
    'fabs':   (32, FMT_RR,            False),
}

BUILTIN_REGISTERS = {
    'exec': 32,
    'LPM_READ_ADDR': 33,
    'LPM_WRITE_ADDR': 34,
    'UNIFORM_ADDR': 35,
    'UNIFORM_READ_DATA': 36,
    'CONST_0': 53,
    'CONST_1': 54,
    'CONST_MINUS_ONE': 55,
    'CONST_2': 56,
    'CONST_4': 57,
    'CONST_0_5': 58,
    'CONST_MINUS_0_5': 59,
    'CONST_1_0': 60,
    'CONST_MINUS_1_0': 61,
    'CONST_2_0': 62,
    'CONST_MINUS_2_0': 63,
    'LPM_READ_VALUE': 109,
    'LPM_WRITE_VALUE': 110,
    'LANE_ID': 111
}

BRANCH_OFFSET_WIDTH = 19


def is_vector_reg(val):
    return val >= 64


def is_scalar_reg(val):
    return val < 64


# returns 0-63 for scalar registers
# 64-127 for vector registers
def parse_reg_operand(lineno, token):
    if token in BUILTIN_REGISTERS:
        return BUILTIN_REGISTERS[token]
    elif token.startswith('r'):
        index = int(token[1:])
        if index > 63:
            raise AssembleError(lineno, f'Invalid register index {index}')

        return index
    elif token.startswith('v'):
        index = int(token[1:])
        if index > 63:
            raise AssembleError(lineno, f'Invalid register index {index}')

        return index + 64
    else:
        raise AssembleError(lineno, f'Invalid register {token}')


class AssembleError(Exception):
    def __init__(self, lineno, message):
        super().__init__(lineno, message)
        self.lineno = lineno
        self.message = message

    def __str__(self):
        return f'Line {self.lineno}: {self.message}'


class Assembler:
    def __init__(self):
        self.labels = {}
        self.code = []
        self.fixups = []
        self.line_map = {}

    def assemble(self, source):
        for lineno, line in enumerate(source.split('\n'), 1):
            self.assemble_line(lineno, line)

        for from_addr, symbol, lineno in self.fixups:
            if symbol not in self.labels:
                raise AssembleError(lineno, f'Unknown label {symbol}')

            # Note, this assumes the instruction at the source already has its
            # branch offset zeroed out
            # XXX does not do range checking, although it's unlikely you'd
            # ever hit that.
            to_addr = self.labels[symbol]
            offset = (((to_addr - (from_addr + 4)) // 4) &
                      ((1 << BRANCH_OFFSET_WIDTH) - 1))
            self.code[from_addr // 4] |= (((offset & 0x7f) << 7)
                                          | ((offset >> 7) << 20))

    def write_list_file(self, source, output_filename):
        """Create an annotated listing file with source code and generated machine code."""
        with open(output_filename, 'w') as list_file:
            for lineno, source_line in enumerate(source.split('\n'), 1):
                source_line = source_line.rstrip()
                instructions = self.line_map.get(lineno)
                if instructions is not None:
                    for i, addr in enumerate(instructions):
                        if i == 0:
                            list_file.write(f'{lineno:>4}   {addr * 4:04x}   {self.code[addr]:08x}   {source_line}\n')
                        else:
                            list_file.write(f'{"":4}   {addr * 4:04x}   {self.code[addr]:08x}\n')
                else:
                    list_file.write(f'{lineno:>4}   {"":15}   {source_line}\n')

    def write_hex_file(self, output_filename):
        """Write the assembled code to a hex file, one instruction per line."""
        with open(output_filename, 'w') as hex_file:
            for instruction in self.code:
                hex_file.write(f'{instruction:08x}\n')

    def assemble_line(self, lineno, line):
        # Strip comments
        line = line.split(';', 1)[0].strip()
        if not line:
            return

        index = 0
        tokens = line.replace(',', ' , ').split() # ensure commas are separate tokens

        def next_token():
            nonlocal index
            if index == len(tokens):
                return None

            retval = tokens[index]
            index += 1
            return retval

        def match(expected):
            got = next_token()
            if got != expected:
                raise AssembleError(lineno, f'Invalid token, got {got} expected {expected}')

        lookahead = next_token()
        if lookahead.endswith(':'):
            self.emit_label(lookahead[:-1], lineno)
            lookahead = next_token()

        if lookahead is None: # End of line
            return

        if lookahead == 'nop':
            self.emit_raw(lineno, 1)  # addi r0, r0, 0
        elif lookahead == 'loadf' or lookahead == 'loadi':
            # Load constant. This is a pseudo-instruction that expands to two
            # instructions: loadhi and loadlo.
            rd = parse_reg_operand(lineno, next_token())
            match(',')
            value = next_token()
            if lookahead == 'loadf':
                raw_int = struct.unpack('<I', struct.pack('<f', float(value)))[0]
            else:
                raw_int = int(value, 0)

            self.emit_k(lineno, 29, rd, (raw_int >> 16) & 0xffff)  # loadhi
            self.emit_k(lineno, 28, rd, raw_int & 0xffff)  # loadlo
        elif lookahead == 'move':
            # Pseudo-instruction that expands to addi rd, rs, 0
            rd = parse_reg_operand(lineno, next_token())
            match(',')
            rs = parse_reg_operand(lineno, next_token())
            self.emit_r(lineno, 4, rd, rs, 53)
        elif lookahead == 'clear':
            # Pseudo-instruction that expands to xor rd, rd, rd
            rd = parse_reg_operand(lineno, next_token())
            self.emit_r(lineno, 2, rd, rd, rd)
        else:
            if lookahead in INSTRS:
                opcode, format, swap_operands = INSTRS[lookahead]
            else:
                raise AssembleError(lineno, f'Invalid opcode {lookahead}')

            if format == FMT_RRR:
                rd = parse_reg_operand(lineno, next_token())
                match(',')
                rs1 = parse_reg_operand(lineno, next_token())
                match(',')
                rs2 = parse_reg_operand(lineno, next_token())
                if is_scalar_reg(rd) and (is_vector_reg(rs1) or is_vector_reg(rs2)):
                    raise AssembleError(lineno, 'Cannot use scalar dest with vector source')

                self.emit_r(lineno, opcode, rd, rs1, rs2)
            elif format == FMT_RR:
                rd = parse_reg_operand(lineno, next_token())
                match(',')
                rs = parse_reg_operand(lineno, next_token())
                if is_scalar_reg(rd) and is_vector_reg(rs):
                    raise AssembleError(lineno, 'Cannot use scalar dest with vector source')

                self.emit_r(lineno, opcode, rd, rs, 0)
            elif format == FMT_COMPARE:
                rd = parse_reg_operand(lineno, next_token())
                match(',')
                rs1 = parse_reg_operand(lineno, next_token())
                match(',')
                rs2 = parse_reg_operand(lineno, next_token())
                if is_vector_reg(rd):
                    raise AssembleError(lineno, 'Dest register for compare must be scalar')

                if swap_operands:
                    self.emit_r(lineno, opcode, rd, rs2, rs1)
                else:
                    self.emit_r(lineno, opcode, rd, rs1, rs2)
            elif format == FMT_COND_BRANCH:
                rd = parse_reg_operand(lineno, next_token())
                match(',')
                target = next_token()
                if is_vector_reg(rd):
                    raise AssembleError(lineno, 'Conditional branch must use scalar register')

                self.add_branch_fixup(target, lineno)
                self.emit_b(lineno, opcode, rd)
            elif format == FMT_UNCOND_BRANCH:
                target = next_token()
                self.add_branch_fixup(target, lineno)
                self.emit_b(lineno, opcode, 0)
            elif format == FMT_INHERENT:
                self.emit_x(lineno, opcode)
            else:
                raise Exception('Internal error: unknown format')

    def get_current_addr(self):
        return len(self.code) * 4

    def add_branch_fixup(self, symbol, lineno):
        """Create a fixup at the current address for a branch to a label that may not yet be defined."""
        self.fixups.append((self.get_current_addr(), symbol, lineno))

    def emit_label(self, name, lineno):
        """Assign a label to the current address."""
        if name in self.labels:
            raise AssembleError(lineno, f'Redefined label {name}')

        self.labels[name] = len(self.code) * 4

    def emit_r(self, lineno, opcode, rd, rs1, rs2):
        self.emit_raw(lineno, opcode | (rd << 7) | (rs1 << 14) | (rs2 << 21))

    def emit_b(self, lineno, opcode, rs1):
        # Target offset is always set using a fixup
        self.emit_raw(lineno, opcode | (rs1 << 14))

    def emit_x(self, lineno, opcode):
        self.emit_raw(lineno, opcode)

    def emit_k(self, lineno, opcode, rd, value):
        self.emit_raw(lineno, opcode | (rd << 7) | (value << 16))

    def emit_raw(self, lineno, value):
        self.line_map.setdefault(lineno, []).append(len(self.code))
        self.code.append(value)


if __name__ == "__main__":
    asm = Assembler()

    try:
        source = open(sys.argv[1], 'r').read()
        asm.assemble(source)
        path = Path(sys.argv[1])
        asm.write_list_file(source, path.with_suffix('.lst'))
        asm.write_hex_file(path.with_suffix('.hex'))
    except AssembleError as exc:
        print(str(exc))
