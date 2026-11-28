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

"""Assembler for CommandProcessor firmware"""

import argparse
from pathlib import Path
import re
import sys

#
# Instructions have a single format:
#  31                               14 13 11 10  8 7   5 4       0
# +-----------------------------------+-----+-----+-----+---------+
# |             imm (18)              | rs2 | rs1 |  rd | opcode  |
# +-----------------------------------+-----+-----+-----+---------+
#

FMT_RRR = 0  # <mnenomic> r0, r1, r2
FMT_RRI = 1  # <mnenomic> r0, r1, num
FMT_LOAD = 2 # <mnenomic> r0, offset(r1)
FMT_STORE = 3 # <mnenomic> r0, offset(r1)
FMT_RI = 4  # <mnenomic> r0, num
FMT_COND_BRANCH = 5  # <mnenomic> r0, r1, label
FMT_ZERO_BRANCH = 6  # <mnemonic> r0, label
FMT_JAL = 7  # <mnenomic> r0, label
FMT_JALR = 8  # <mnenomic> r0, r1

# Each entry is a tuple of (opcode, format)
INSTRS = {
    'and': (0, FMT_RRR),
    'or': (1, FMT_RRR),
    'xor': (2, FMT_RRR),
    'add': (3, FMT_RRR),
    'sub': (4, FMT_RRR),
    'sltu': (5, FMT_RRR),
    'sll': (6, FMT_RRR),
    'srl': (7, FMT_RRR),
    'beq': (8, FMT_COND_BRANCH),
    'bne': (9, FMT_COND_BRANCH),
    'bz': (10, FMT_ZERO_BRANCH),
    'bnz': (11, FMT_ZERO_BRANCH),
    'andi': (16, FMT_RRI),
    'ori': (17, FMT_RRI),
    'xori': (18, FMT_RRI),
    'addi': (19, FMT_RRI),
    'subi': (20, FMT_RRI),
    'sltui': (21, FMT_RRI),
    'slli': (22, FMT_RRI),
    'srli': (23, FMT_RRI),
    'lw': (26, FMT_LOAD),
    'sw': (27, FMT_STORE),
    'movehi': (28, FMT_RRI),
    'movei': (29, FMT_RI),
    'jal': (30, FMT_JAL),
    'jalr': (31, FMT_JALR)
}

class AssembleError(Exception):
    def __init__(self, lineno, message):
        super().__init__(lineno, message)
        self.lineno = lineno
        self.message = message

    def __str__(self):
        return f'Line {self.lineno}: {self.message}'


def parse_reg_operand(lineno, token):
    if token is None or not re.fullmatch(r'r[0-9]+', token):
        raise AssembleError(lineno, f'Invalid register {token}')

    index = int(token[1:])
    if index > 7:
        raise AssembleError(lineno, f'Invalid register index {index}')

    return index


def parse_int_operand(lineno, token):
    try:
        return int(token, 0)
    except (TypeError, ValueError):
        raise AssembleError(lineno, f'Invalid integer {token}') from None


def is_int_operand(token):
    return token is not None and (token[0].isdigit() or token[0] in '+-')


def parse_imm_operand(lineno, token):
    imm = parse_int_operand(lineno, token)
    if not -(1 << 17) <= imm < (1 << 18):
        raise AssembleError(lineno, f'Immediate {token} out of range')

    return imm & ((1 << 18) - 1)


def parse_word_operand(lineno, token):
    value = parse_int_operand(lineno, token)
    if not -(1 << 31) <= value < (1 << 32):
        raise AssembleError(lineno, f'Value {token} out of range for 32 bits')

    return value & 0xffffffff

class Assembler:
    def __init__(self):
        self.labels = {}
        self.code = []
        self.fixups = []
        self.line_map = {}

    def assemble(self, source):
        for lineno, line in enumerate(source.split('\n'), 1):
            self.assemble_line(lineno, line)

        for from_addr, kind, symbol, lineno in self.fixups:
            if symbol not in self.labels:
                raise AssembleError(lineno, f'Unknown label {symbol}')

            to_addr = self.labels[symbol]
            if kind == 'imm':
                if to_addr >= (1 << 18):
                    raise AssembleError(lineno, f'Data address {to_addr} out of range')

                self.code[from_addr] |= to_addr << 14
            else:
                # Direct data fixup
                self.code[from_addr] = to_addr


    def write_list_file(self, source, output_filename):
        """Create an annotated listing file with source code and generated machine code."""
        with open(output_filename, 'w') as list_file:
            for lineno, source_line in enumerate(source.split('\n'), 1):
                source_line = source_line.rstrip()
                instructions = self.line_map.get(lineno)
                if instructions is not None:
                    for i, addr in enumerate(instructions):
                        if i == 0:
                            list_file.write(f'{addr:04x}   {self.code[addr]:08x}   {source_line}\n')
                        else:
                            list_file.write(f'{addr:04x}   {self.code[addr]:08x}\n')
                else:
                    list_file.write(f'{"":15}   {source_line}\n')

        print(f'wrote {output_filename}')

    def write_hex_file(self, output_filename):
        """Write the assembled code to a hex file, one instruction per line."""
        with open(output_filename, 'w') as hex_file:
            for instruction in self.code:
                hex_file.write(f'{instruction:08x}\n')

        print(f'wrote {output_filename}')

    def assemble_line(self, lineno, line):
        # Strip comments
        line = line.split(';', 1)[0].strip()
        if not line:
            return

        index = 0
        # ensure commas and parentheses are separate tokens
        tokens = line.replace(',', ' , ').replace('(', ' ( ').replace(')', ' ) ').split()

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

        if lookahead == '.word':
            while True:
                value = next_token()
                if value is None:
                    raise AssembleError(lineno, 'Unexpected end of line in .word directive')

                self.line_map.setdefault(lineno, []).append(len(self.code))
                if is_int_operand(value):
                    self.code.append(parse_word_operand(lineno, value))
                else:
                    self.add_fixup('data', value, lineno)
                    self.code.append(0)

                if index == len(tokens):
                    break
                match(',')

        elif lookahead == '.fill':
            count = parse_int_operand(lineno, next_token())
            if count < 0:
                raise AssembleError(lineno, 'Negative .fill count')
            match(',')
            value = parse_word_operand(lineno, next_token())
            for _ in range(count):
                self.line_map.setdefault(lineno, []).append(len(self.code))
                self.code.append(value)
        elif lookahead == 'nop':
            self.emit_inst(lineno, 1, 0, 0, 0, 0)  # addi r0, r0, 0
        elif lookahead == 'loadi':
            # Load constant. This is a pseudo-instruction that expands to two
            # instructions: movei and movehi.
            rd = parse_reg_operand(lineno, next_token())
            match(',')
            raw_int = parse_word_operand(lineno, next_token())
            self.emit_inst(lineno, INSTRS['movei'][0], rd, 0, 0, raw_int & 0x3fff)
            self.emit_inst(lineno, INSTRS['movehi'][0], rd, rd, 0, (raw_int >> 14) & 0x3ffff)
        elif lookahead == 'lea':
            rd = parse_reg_operand(lineno, next_token())
            match(',')
            target_label = next_token()
            self.add_fixup('imm', target_label, lineno)
            self.emit_inst(lineno, INSTRS['movei'][0], rd, 0, 0, 0)
        elif lookahead == 'move':
            # Pseudo-instruction that expands to addi rd, rs, 0
            rd = parse_reg_operand(lineno, next_token())
            match(',')
            rs = parse_reg_operand(lineno, next_token())
            self.emit_inst(lineno, INSTRS['addi'][0], rd, rs, 0)
        elif lookahead == 'clear':
            # Pseudo-instruction that expands to xor rd, rd, rd
            rd = parse_reg_operand(lineno, next_token())
            self.emit_inst(lineno, INSTRS['xor'][0], rd, rd, rd)
        elif lookahead == 'b':
            # Unconditional branch. This is a pseudo-instruction that expands to beq r0, r0, target
            target = next_token()
            self.add_fixup('imm', target, lineno)
            self.emit_inst(lineno, INSTRS['beq'][0], 0, 0, 0)
        else:
            if lookahead in INSTRS:
                opcode, format = INSTRS[lookahead]
            else:
                raise AssembleError(lineno, f'Invalid opcode {lookahead}')

            if format == FMT_RRR:
                rd = parse_reg_operand(lineno, next_token())
                match(',')
                rs1 = parse_reg_operand(lineno, next_token())
                match(',')
                rs2 = parse_reg_operand(lineno, next_token())
                self.emit_inst(lineno, opcode, rd, rs1, rs2)
            elif format == FMT_RI:
                rd = parse_reg_operand(lineno, next_token())
                match(',')
                imm = parse_imm_operand(lineno, next_token())
                self.emit_inst(lineno, opcode, rd, 0, 0, imm)
            elif format == FMT_RRI:
                rd = parse_reg_operand(lineno, next_token())
                match(',')
                rs = parse_reg_operand(lineno, next_token())
                match(',')
                imm = parse_imm_operand(lineno, next_token())
                self.emit_inst(lineno, opcode, rd, rs, 0, imm)
            elif format in (FMT_LOAD, FMT_STORE):
                op0 = parse_reg_operand(lineno, next_token())
                match(',')
                lookahead = next_token()
                if is_int_operand(lookahead):
                    # Offset
                    imm = parse_imm_operand(lineno, lookahead)

                    match('(')
                elif lookahead == '(':
                    imm = 0
                else:
                    if lookahead is None:
                        raise AssembleError(lineno, 'Expected load/store offset or base register')
                    # Identifier, use label
                    self.add_fixup('imm', lookahead, lineno)
                    match('(')
                    imm = 0

                ptr_op = parse_reg_operand(lineno, next_token())
                match(')')

                if format == FMT_STORE:
                    self.emit_inst(lineno, opcode, 0, ptr_op, op0, imm)
                else:
                    self.emit_inst(lineno, opcode, op0, ptr_op, 0, imm)
            elif format == FMT_COND_BRANCH:
                rs1 = parse_reg_operand(lineno, next_token())
                match(',')
                rs2 = parse_reg_operand(lineno, next_token())
                match(',')
                target = next_token()
                self.add_fixup('imm', target, lineno)
                self.emit_inst(lineno, opcode, 0, rs1, rs2)
            elif format == FMT_ZERO_BRANCH:
                rs1 = parse_reg_operand(lineno, next_token())
                match(',')
                target = next_token()
                self.add_fixup('imm', target, lineno)
                self.emit_inst(lineno, opcode, 0, rs1, 0)
            elif format == FMT_JAL:
                rd = parse_reg_operand(lineno, next_token())
                match(',')
                target = next_token()
                self.add_fixup('imm', target, lineno)
                self.emit_inst(lineno, opcode, rd, 0, 0)
            elif format == FMT_JALR:
                rd = parse_reg_operand(lineno, next_token())
                match(',')
                rs = parse_reg_operand(lineno, next_token())
                self.emit_inst(lineno, opcode, rd, rs, 0, 0)
            else:
                raise Exception('Internal error: unknown format')

        if index != len(tokens):
            raise AssembleError(lineno, f'Unexpected token {tokens[index]}')

    def get_current_addr(self):
        return len(self.code)

    def add_fixup(self, kind, symbol, lineno):
        self.fixups.append((self.get_current_addr(), kind, symbol, lineno))

    def emit_label(self, name, lineno):
        """Assign a label to the current address."""
        if name in self.labels:
            raise AssembleError(lineno, f'Redefined label {name}')

        self.labels[name] = len(self.code)

    def emit_inst(self, lineno, opcode, rd, rs1, rs2, imm = 0):
        value = (imm << 14) | (rs2 << 11) | (rs1 << 8) | (rd << 5) | opcode
        self.line_map.setdefault(lineno, []).append(len(self.code))
        self.code.append(value)

def preprocess(source: str) -> str:
    macros = {}
    result = ''

    define_re = re.compile(r'^\s*\.define\s+([A-Za-z_][A-Za-z0-9_]*)\s+(.+)$')

    for line in source.splitlines():
        line = line.split(';', 1)[0].strip() # Remove comments
        if not line:
            result += '\n'
            continue

        match = define_re.match(line)
        if match:
            name, value = match.group(1), match.group(2).strip()
            macros[name] = value

            # Emit blank line to preserve line numbers
            result += '\n'
            continue

        for name in sorted(macros, key=len, reverse=True):
            line = re.sub(rf'\b{re.escape(name)}\b', macros[name], line)

        result += line + '\n'

    return result

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('source', type=Path, help='assembly source file')
    args = parser.parse_args()
    asm = Assembler()

    try:
        source = args.source.read_text()
        asm.assemble(preprocess(source))
        asm.write_list_file(source, args.source.with_suffix('.lst'))
        asm.write_hex_file(args.source.with_suffix('.hex'))
    except (AssembleError, OSError) as exc:
        print(str(exc), file=sys.stderr)
        sys.exit(1)
