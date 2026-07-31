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
    'and': (0, FMT_RRR, False),
    'or': (1, FMT_RRR, False),
    'xor': (2, FMT_RRR, False),
    'addi': (3, FMT_RRR, False),
    'subi': (4, FMT_RRR, False),
    'muli': (5, FMT_RRR, False),
    'lsl': (6, FMT_RRR, False),
    'asr': (7, FMT_RRR, False),
    'lsr': (8, FMT_RRR, False),
    'addf': (9, FMT_RRR, True),
    'subf': (10, FMT_RRR, True),
    'mulf': (11, FMT_RRR, True),
    'recip': (12, FMT_RR, False),
    'ftoi': (13, FMT_RR, False),
    'itof': (14, FMT_RR, False),
    'setgtf': (15, FMT_COMPARE, True),
    'setltf': (16, FMT_COMPARE, True),
    'setgei': (17, FMT_COMPARE, False),
    'setlei': (17, FMT_COMPARE, False),
    'setlti': (18, FMT_COMPARE, False),
    'setgti': (18, FMT_COMPARE, True),
    'setgeu': (19, FMT_COMPARE, False),
    'setleu': (19, FMT_COMPARE, True),
    'setltu': (20, FMT_COMPARE, False),
    'setgtu': (20, FMT_COMPARE, True),
    'seteq': (21, FMT_COMPARE, False),
    'setne': (22, FMT_COMPARE, False),
    'bnz': (23, FMT_COND_BRANCH, False),
    'bz': (24, FMT_COND_BRANCH, False),
    'j': (25, FMT_UNCOND_BRANCH, False),
    'halt': (127, FMT_INHERENT, False)
}

BRANCH_OFFSET_WIDTH = 19


def is_vector_reg(val):
    return val >= 64


def is_scalar_reg(val):
    return val < 64


# returns 0-63 for scalar registers
# 64-127 for vector registers
def parse_reg_operand(lineno, token):
    if token == 'exec':
        return 32
    elif token.startswith('r'):
        index = int(token[1:])
        if index > 63:
            raise AssembleError(lineno, 'Invalid register index' + index)

        return index
    elif token.startswith('v'):
        index = int(token[1:])
        if index > 63:
            raise AssembleError(lineno, 'Invalid register index' + index)

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

    def assemble(self, source_filename):
        with open(source_filename, 'r') as f:
            for lineno, line in enumerate(f, 1):
                self.assemble_line(lineno, line)

        for from_addr, symbol, lineno in self.fixups:
            if symbol not in self.labels:
                raise AssembleError(lineno, 'Unknown label ' + symbol)

            # Note, this assumes the instruction at the source already has its
            # branch offset zeroed out
            # XXX does not do range checking, although it's unlikely you'd
            # ever hit that.
            to_addr = self.labels[symbol]
            offset = (((to_addr - (from_addr + 4)) // 4) &
                      ((1 << BRANCH_OFFSET_WIDTH) - 1))
            self.code[from_addr // 4] |= (((offset & 0x3f) << 7)
                                          | ((offset >> 6) << 19))

        self.write_list_file(source_filename)
        self.write_hex_file(source_filename)

    def write_list_file(self, source_filename):
        list_file_name = Path(source_filename).with_suffix(".lst")
        with open(source_filename, 'r') as source_file, open(list_file_name, 'w') as list_file:
            for lineno, source_line in enumerate(source_file, 1):
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

    def write_hex_file(self, source_filename):
        hex_file_name = Path(source_filename).with_suffix(".hex")
        with open(hex_file_name, 'w') as hex_file:
            for instruction in self.code:
                hex_file.write(f'{instruction:08x}\n')

    def assemble_line(self, lineno, line):
        # Strip comments
        line = line.split(';', 1)[0].strip()
        if not line:
            return

        index = 0
        tokens = line.replace(',', ' , ').split()

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

        if lookahead is None:
            return

        if lookahead == 'loadf' or lookahead == 'loadi':
            rd = parse_reg_operand(lineno, next_token())
            match(',')
            value = next_token()
            if lookahead == 'loadf':
                raw_int = struct.unpack('<I', struct.pack('<f', float(value)))[0]
            else:
                raw_int = int(value, 0)

            self.emit_k(lineno, 0x43, rd, (raw_int >> 16) & 0xffff)  # loadhi
            self.emit_k(lineno, 0x44, rd, raw_int & 0xffff)  # loadlo
        elif lookahead == 'move':
            rd = parse_reg_operand(lineno, next_token())
            match(',')
            rs = parse_reg_operand(lineno, next_token())
            self.emit_rv(lineno, 0, rd, rs, rs)
        elif lookahead == 'clear':
            rd = parse_reg_operand(lineno, next_token())
            self.emit_rv(lineno, 2, rd, rd, rd)  # xor rd, rd, rd
        else:
            if lookahead in INSTRS:
                opcode, format, swap_operands = INSTRS[lookahead]
            else:
                raise AssembleError(lineno, 'Invalid opcode ' + lookahead)

            if format == FMT_RRR:
                rd = parse_reg_operand(lineno, next_token())
                match(',')
                rs1 = parse_reg_operand(lineno, next_token())
                match(',')
                rs2 = parse_reg_operand(lineno, next_token())
                if is_scalar_reg(rd) and (is_vector_reg(rs1) or is_vector_reg(rs2)):
                    raise AssembleError(lineno, 'Cannot use scalar dest with vector source')

                self.emit_rv(lineno, opcode, rd, rs1, rs2)
            elif format == FMT_RR:
                rd = parse_reg_operand(lineno, next_token())
                match(',')
                rs = parse_reg_operand(lineno, next_token())
                if is_scalar_reg(rd) and is_vector_reg(rs):
                    raise AssembleError(lineno, 'Cannot use scalar dest with vector source')

                self.emit_rv(lineno, opcode, rd, rs, 0)
            elif format == FMT_COMPARE:
                rd = parse_reg_operand(lineno, next_token())
                match(',')
                rs1 = parse_reg_operand(lineno, next_token())
                match(',')
                rs2 = parse_reg_operand(lineno, next_token())
                if is_vector_reg(rd):
                    raise AssembleError(lineno, 'Dest register for compare must be scalar')

                if swap_operands:
                    self.emit_rv(lineno, opcode, rd, rs2, rs1)
                else:
                    self.emit_rv(lineno, opcode, rd, rs1, rs2)
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
        self.fixups.append((self.get_current_addr(), symbol, lineno))

    def emit_label(self, name, lineno):
        if name in self.labels:
            raise AssembleError(lineno, 'Redefined label ' + name)

        self.labels[name] = len(self.code)

    def emit_rv(self, lineno, opcode, rd, rs1, rs2):
        self.emit_raw(lineno, opcode | (rd << 7) | (rs1 << 13) | (rs2 << 20))

    def emit_b(self, lineno, opcode, rs1):
        # Destination is always set using a fixup
        self.emit_raw(lineno, opcode | (rs1 << 13))

    def emit_x(self, lineno, opcode):
        self.emit_raw(lineno, opcode)

    def emit_k(self, lineno, opcode, rd, value):
        self.emit_raw(lineno, opcode | (rd << 7) | (value << 16))

    def emit_raw(self, lineno, value):
        self.line_map.setdefault(lineno, []).append(len(self.code))
        self.code.append(value)


asm = Assembler()

try:
    asm.assemble(sys.argv[1])
except AssembleError as exc:
    print(str(exc))
