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

import unittest
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from assemble import Assembler, AssembleError

def make_r(opcode, dest, src1, src2):
    return opcode | (dest << 7) | (src1 << 14) | (src2 << 21)

def make_k(opcode, dest, immediate):
    return opcode | (dest << 7) | (immediate << 16)

def make_b(opcode, reg, offset):
    offset &= 0x7ffff
    return opcode | ((offset & 0x7f) << 7) | (reg << 14) | ((offset >> 7) << 20)


class TestAssemblerConstants(unittest.TestCase):
    def test_const_int(self):
        asm = Assembler()
        # test various bases
        asm.assemble('''
            loadi r2, 0x12345678
            loadi v3, 0b10101011110011011110111100010010
            loadi v4, 3735928559
            loadi r5, -1234567890
        ''')
        self.assertEqual(asm.code, [
            make_k(29, 2, 0x1234),
            make_k(28, 2, 0x5678),
            make_k(29, 64 + 3, 0xabcd),
            make_k(28, 64 + 3, 0xef12),
            make_k(29, 64 + 4, 0xdead),
            make_k(28, 64 + 4, 0xbeef),
            make_k(29, 5, 0xb669),
            make_k(28, 5, 0xfd2e),
    ])

    def test_const_float(self):
        asm = Assembler()
        asm.assemble('''
            loadf r2, 1.234
            loadf v3, -0.123
        ''')
        self.assertEqual(asm.code, [
            make_k(29, 2, 0x3f9d),
            make_k(28, 2, 0xf3b6),
            make_k(29, 64 + 3, 0xbdfb),
            make_k(28, 64 + 3, 0xe76d)
        ])

    def test_arithmetic(self):
        asm = Assembler()
        asm.assemble('''
            and r1, r2, r3
            or v4, v5, v6
            xor r7, r8, r9
            addi v10, v11, r12
            subi v13, r14, v15
            muli v16, r17, r18
            mulih v16, r17, r18
            lsl r19, r20, r21
            asr r22, r23, r24
            lsr r25, r26, r27
            addf r28, r29, r30
            subf r31, r32, r33
            mulf r34, r35, r36
        ''')
        self.assertEqual(asm.code, [
            make_r(1, 1, 2, 3),
            make_r(2, 64 + 4, 64 + 5, 64 | 6),
            make_r(3, 7, 8, 9),
            make_r(4, 64 + 10, 64 + 11, 12),
            make_r(5, 64 + 13, 14, 64 | 15),
            make_r(6, 64 + 16, 17, 18),
            make_r(7, 64 + 16, 17, 18),
            make_r(8, 19, 20, 21),
            make_r(9, 22, 23, 24),
            make_r(10, 25, 26, 27),
            make_r(11, 28, 29, 30),
            make_r(12, 31, 32, 33),
            make_r(13, 34, 35, 36),
        ])

    def test_error_scalar_dest(self):
        asm = Assembler()
        with self.assertRaises(AssembleError) as context:
            asm.assemble('''
                addi r1, v2, r3
            ''')

        assert 'Line 2: Cannot use scalar dest with vector source' in str(context.exception)

    def test_error_register_out_of_range(self):
        asm = Assembler()
        with self.assertRaises(AssembleError) as context:
            asm.assemble('''
                addi r1, r2, r65
            ''')

        assert 'Line 2: Invalid register index 65' in str(context.exception)

    def test_error_unknown_label(self):
        asm = Assembler()
        with self.assertRaises(AssembleError) as context:
            asm.assemble('''
                j unknown_label
            ''')

        assert 'Line 2: Unknown label unknown_label' in str(context.exception)

    def test_error_redefined_label(self):
        asm = Assembler()
        with self.assertRaises(AssembleError) as context:
            asm.assemble('''
                label1:
                j label1
                label1:
            ''')

        assert 'Line 4: Redefined label label1' in str(context.exception)

    def test_invalid_register(self):
        asm = Assembler()
        with self.assertRaises(AssembleError) as context:
            asm.assemble('''
                addi q1, r2, r3
            ''')

        assert 'Line 2: Invalid register q1' in str(context.exception)

    def test_invalid_opcode(self):
        asm = Assembler()
        with self.assertRaises(AssembleError) as context:
            asm.assemble('''
                invalid_opcode r1, r2, r3
            ''')

        assert 'Line 2: Invalid opcode invalid_opcode' in str(context.exception)

    def test_bad_separator(self):
        asm = Assembler()
        with self.assertRaises(AssembleError) as context:
            asm.assemble('''
                addi r1 r2, r3
            ''')

        assert 'Line 2: Invalid token, got r2 expected ,' in str(context.exception)

    def test_rr(self):
        asm = Assembler()
        asm.assemble('''
            recip r2, r3
            ftoi r4, r5
            itof r6, r7
        ''')
        self.assertEqual(asm.code, [
            make_r(14, 2, 3, 0),
            make_r(15, 4, 5, 0),
            make_r(16, 6, 7, 0),
        ])

    def test_b(self):
        asm = Assembler()
        asm.assemble('''
            label3: bnz r1, label1
            bz r2, label2
            j label3 ; Backward branch
            nop
            label2: nop
            label1: nop
        ''')
        self.assertEqual(asm.code, [
            make_b(25, 1, 4),
            make_b(26, 2, 2),
            make_b(27, 0, -3),
            0, 0, 0
        ])

    def test_compare(self):
        asm = Assembler()
        asm.assemble('''
            setgtf r1, v2, v3
            setltf r4, v5, r6
            setgei r7, r8, v9
            setlei r10, r11, r12
            setlti r13, v14, v15
            setgti r16, r17, r18
            setgeu r20, v21, v22
            setleu r23, r24, v25
            setltu r26, r27, r28
            setgtu r29, r30, r31
            seteq r32, v33, v34,
            setne r35, r36, r37
        ''')

        self.assertEqual(asm.code, [
            make_r(17, 1, 64 + 2, 64 + 3),
            make_r(18, 4, 64 + 5, 6),
            make_r(19, 7, 8, 64 + 9),
            make_r(19, 10, 12, 11),
            make_r(20, 13, 64 + 14, 64 + 15),
            make_r(20, 16, 18, 17),
            make_r(21, 20, 64 + 21, 64 + 22),
            make_r(21, 23, 64 + 25, 24),
            make_r(22, 26, 27, 28),
            make_r(22, 29, 31, 30),
            make_r(23, 32, 64 + 33, 64 + 34),
            make_r(24, 35, 36, 37)
        ])

    def test_compare_bad_dest(self):
        asm = Assembler()
        with self.assertRaises(AssembleError) as context:
            asm.assemble('''
                setgtf v1, v2, v3
            ''')

        assert 'Line 2: Dest register for compare must be scalar' in str(context.exception)

if __name__ == "__main__":
    unittest.main()

