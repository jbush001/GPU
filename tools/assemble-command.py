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

"""Assembler for CommandProcessor"""

import sys
from pathlib import Path

#
# Instructions have a single format:
#  31                               14 13 11 10  8 7   5 4       0
# +-----------------------------------+-----+-----+-----+---------+
# |             imm (18)              | rs1 | rs2 |  rd | opcode  |
# +-----------------------------------+-----+-----+-----+---------+
#

FMT_RRR = 0  # mnenomic r0, r1, r2
FMT_RRI = 1  # mnenomic r0, r1, num
FMT_LOAD = 2 # mnenomic r0, offset(r1)
FMT_STORE = 3 # mnenomic r0, offset(r1)
FMT_RI = 4  # mnenomic r0, num
FMT_COND_BRANCH = 5  # mnenomic r0, r1, label
FMT_JAL = 6  # mnenomic r0, label
FMT_JALR = 7  # mnenomic r0, r1

# Each entry is a tuple of (opcode, format)
INSTRS = {
    'and': (0, FMT_RRR),
    'or': (1, FMT_RRR),
    'xor': (2, FMT_RRR),
    'add': (3, FMT_RRR),
    'sub': (4, FMT_RRR),
    'sltu': (5, FMT_RRR),
    'sll': (6, FMT_RRR),
    'srl': (8, FMT_RRR),
    'addi': (9, FMT_RRI),
    'sltui': (10, FMT_RRI),
    'slli': (11, FMT_RRI),
    'srli': (12, FMT_RRI),
    'lw': (13, FMT_LOAD),
    'sw': (14, FMT_STORE),
    'movehi': (15, FMT_RRI),
    'movei': (16, FMT_RI),
    'beq': (17, FMT_COND_BRANCH),
    'bne': (18, FMT_COND_BRANCH),
    'jal': (19, FMT_JAL),
    'jalr': (20, FMT_JALR)
}

class AssembleError(Exception):
    def __init__(self, lineno, message):
        super().__init__(lineno, message)
        self.lineno = lineno
        self.message = message

    def __str__(self):
        return f'Line {self.lineno}: {self.message}'


def parse_reg_operand(lineno, token):
    if token.startswith('r'):
        index = int(token[1:])
        if index > 7:
            raise AssembleError(lineno, f'Invalid register index {index}')

        return index
    else:
        raise AssembleError(lineno, f'Invalid register {token}')


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
            offset = (((to_addr - from_addr) // 4) &
                      ((1 << 18) - 1))
            self.code[from_addr // 4] |= offset << 14

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

        if lookahead == 'nop':
            self.emit_inst(lineno, 1, 0, 0, 0, 0)  # addi r0, r0, 0
        elif lookahead == 'loadi':
            # Load constant. This is a pseudo-instruction that expands to two
            # instructions: movei and movehi.
            rd = parse_reg_operand(lineno, next_token())
            match(',')
            raw_int = int(next_token(), 0)
            self.emit_inst(lineno, INSTRS['movei'][0], rd, 0, 0, raw_int & 0x3fff)
            self.emit_inst(lineno, INSTRS['movehi'][0], rd, rd, 0, (raw_int >> 14) & 0x3ffff)
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
            self.add_branch_fixup(target, lineno)
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
                imm = self.encode_imm(int(next_token(), 0))
                self.emit_inst(lineno, opcode, rd, 0, 0, imm)
            elif format == FMT_RRI:
                rd = parse_reg_operand(lineno, next_token())
                match(',')
                rs = parse_reg_operand(lineno, next_token())
                match(',')
                imm = int(next_token(), 0)
                imm = self.encode_imm(imm)
                self.emit_inst(lineno, opcode, rd, rs, 0, imm)
            elif format in (FMT_LOAD, FMT_STORE):
                op0 = parse_reg_operand(lineno, next_token())
                match(',')
                lookahead = next_token()
                # Check if the lookahead is a number
                if lookahead[0].isdigit() or (lookahead[0] == '-' and lookahead[1:].isdigit()):
                    imm = int(lookahead, 0)
                    imm = self.encode_imm(imm)

                    match('(')
                else:
                    if lookahead != '(':
                        raise AssembleError(lineno, f'Expected immediate or "(" but got {lookahead}')

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
                self.add_branch_fixup(target, lineno)
                self.emit_inst(lineno, opcode, 0, rs1, rs2)
            elif format == FMT_JAL:
                rd = parse_reg_operand(lineno, next_token())
                match(',')
                target = next_token()
                self.add_branch_fixup(target, lineno)
                self.emit_inst(lineno, opcode, rd, 0, 0)
            elif format == FMT_JALR:
                rd = parse_reg_operand(lineno, next_token())
                match(',')
                rs = parse_reg_operand(lineno, next_token())
                self.emit_inst(lineno, opcode, rd, rs, 0, 0)
            else:
                raise Exception('Internal error: unknown format')

    def encode_imm(self, imm):
        """Convert an signed immediate value to its 18-bit two's complement
        representation.
        """
        if imm < 0:
            imm = (1 << 18) + imm
        return imm

    def get_current_addr(self):
        return len(self.code) * 4

    def add_branch_fixup(self, symbol, lineno):
        """Create a fixup at the current address for a branch to a label that
        may not yet be defined.
        """
        self.fixups.append((self.get_current_addr(), symbol, lineno))

    def emit_label(self, name, lineno):
        """Assign a label to the current address."""
        if name in self.labels:
            raise AssembleError(lineno, f'Redefined label {name}')

        self.labels[name] = len(self.code) * 4

    def emit_inst(self, lineno, opcode, rd, rs1, rs2, imm = 0):
        value = (imm << 14) | (rs2 << 11) | (rs1 << 8) | (rd << 5) | opcode
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
        sys.exit(1)
