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
import emulate

def make_r(opcode, dest, src1, src2):
    return opcode | (dest << 7) | (src1 << 14) | (src2 << 21)

def make_k(opcode, dest, immediate):
    return opcode | (dest << 7) | (immediate << 16)

def make_b(opcode, reg, offset):
    offset &= 0x7ffff
    return opcode | ((offset & 0x7f) << 7) | (reg << 14) | ((offset >> 7) << 20)

HALT = 0

class TestEmulator(unittest.TestCase):
    def run_test(self, code, init_regs, final_regs):
        cpu = emulate.Emulator()
        for reg, value in init_regs.items():
            if reg >= 64:
                assert(isinstance(value, list))
                for i in range(8):
                    cpu.registers[reg][i] = value[i] & 0xffffffff
            else:
                assert(isinstance(value, int))
                cpu.registers[reg] = value & 0xffffffff

        cpu.instructions += code
        cpu.instructions += [HALT]

        cpu.run()

        for reg, value in final_regs.items():
            if cpu.registers[reg] != value:
                self.fail(f'mismatch: register {reg} expected {value} got {cpu.registers[reg]}')

    # Test all instruction formats
    def test_and_sss(self):
        self.run_test([make_r(1, 3, 1, 2)],
                      init_regs={1: 0b10010101011001010100110010101001, 2: 0b10010101011001010100110010101001},
                      final_regs={3: 0b10010101011001010100110010101001})

    def test_and_vsv(self):
        self.run_test([make_r(1, 64 + 3, 1, 64 + 2)],
                      init_regs={1: 0b10010101011001010100110010101001,
                       (64 + 2): [0xf, 0xf0, 0xf00, 0xf000, 0xf0000, 0xf00000, 0xf000000, 0xf0000000]},
                      final_regs={(64 + 3): [
                        0b1001,
                        0b10100000,
                        0b110000000000,
                        0b0100000000000000,
                        0b01010000000000000000,
                        0b011000000000000000000000,
                        0b0101000000000000000000000000,
                        0b10010000000000000000000000000000
                       ]})

    def test_and_vvs(self):
        self.run_test([make_r(1, 64 + 3, 64 + 2, 1)],
                      init_regs={1: 0b10010101011001010100110010101001,
                       (64 + 2): [0xf, 0xf0, 0xf00, 0xf000, 0xf0000, 0xf00000, 0xf000000, 0xf0000000]},
                      final_regs={(64 + 3): [
                        0b1001,
                        0b10100000,
                        0b110000000000,
                        0b0100000000000000,
                        0b01010000000000000000,
                        0b011000000000000000000000,
                        0b0101000000000000000000000000,
                        0b10010000000000000000000000000000
                       ]})

    def test_and_vvv(self):
        self.run_test([make_r(1, 64 + 2, 64 + 3, 64 + 4)],
                      init_regs={(64 + 3): [0xaaaaaaaa, 0xbbbbbbbb, 0xcccccccc, 0xdddddddd, 0xeeeeeeee, 0xffffffff, 0x55555555, 0xcccccccc],
                       (64 + 4): [0xf, 0xf0, 0xf00, 0xf000, 0xf0000, 0xf00000, 0xf000000, 0xf0000000]},
                      final_regs={(64 + 2): [0xa, 0xb0, 0xc00, 0xd000, 0xe0000, 0xf00000, 0x5000000, 0xc0000000]})

    def test_exec_mask_rrr(self):
        self.run_test([make_r(2, 64 + 3, 64 + 1, 64 + 1)],  # or v3, v1, v1
                      init_regs={32: 0x55,
                        (64 + 1): [0x11111111, 0x22222222, 0x33333333, 0x44444444, 0x55555555, 0x66666666, 0x77777777, 0x88888888],
                        (64 + 3): [0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff],
                       },
                      final_regs={(64 + 3): [0x11111111, 0xffffffff, 0x33333333, 0xffffffff, 0x55555555, 0xffffffff, 0x77777777, 0xffffffff]})

    # Test individual instructions (only scalar form)
    def test_or(self):
        self.run_test([make_r(2, 2, 3, 4)],
                      init_regs={3: 0b10100101110000110110100111110000, 4: 0b10101010101010101010101010101010},
                      final_regs={2: 0b10101111111010111110101111111010})

    def test_xor(self):
        self.run_test([make_r(3, 2, 3, 4)],
                      init_regs={3: 0b10101010101010101010101010101010, 4: 0b11001100110011001100110011001100},
                      final_regs={2: 0b01100110011001100110011001100110})

    def test_addi(self):
        self.run_test([make_r(4, 2, 3, 4)],
                      init_regs={3: 0x12345678, 4: 0xf00d1234},
                      final_regs={2: 0x024168ac})

    # Note: this has a negative result. Ensure it is properly clamped to a 32-bit register
    def test_subi(self):
        self.run_test([make_r(5, 2, 3, 4)],
                      init_regs={3: 0x12345678, 4: 0xf00d1234},
                      final_regs={2: 0x22274444})

    def test_muli(self):
        self.run_test([make_r(6, 2, 3, 4)],
                      init_regs={3: 1234, 4: 567},
                      final_regs={2: 699678})

    def test_mulih(self):
        self.run_test([make_r(7, 2, 3, 4)],
                      init_regs={3: 0x2, 4: 0x80000000},
                      final_regs={2: 0xffffffff})

    def test_mulihu(self):
        self.run_test([make_r(33, 2, 3, 4)],
                      init_regs={3: 0x2, 4: 0x80000000},
                      final_regs={2: 0x1})

    def test_lsl(self):
        self.run_test([make_r(8, 2, 3, 4)],
                      init_regs={3: 1234, 4: 3},
                      final_regs={2: 9872})

    # Ensure 1s are shifted in.
    def test_asr(self):
        self.run_test([make_r(9, 2, 3, 4)],
                      init_regs={3: 0x80123456, 4: 4},
                      final_regs={2: 0xf8012345})

    def test_lsr(self):
        self.run_test([make_r(10, 2, 3, 4)],
                      init_regs={3: 0x80123456, 4: 4},
                      final_regs={2: 0x8012345})


    def test_addf(self):
        self.run_test([make_r(11, 2, 3, 4)],
                      init_regs={3: 0x402df8a1, 4: 0x40490e56}, # 2.7183 + 3.1415
                      final_regs={2: 0x40bb837c})  # 5.8598

    def test_subf(self):
        self.run_test([make_r(12, 2, 3, 4)],
                      init_regs={3: 0x402df8a1, 4: 0x40490e56}, # 2.7183 - 3.1415
                      final_regs={2: 0xbed8ada8})  # -0.4232

    def test_mulf(self):
        self.run_test([make_r(13, 2, 3, 4)],
                      init_regs={3: 0x3fc00000, 4: 0x40200000}, # 1.5 * 2.5
                      final_regs={2: 0x40700000})  # 3.75

    # Test all formats for RR
    def test_recip_vv(self):
        self.run_test([make_r(14, 64 + 2, 64 + 3, 0)],
                      # 1.0, 0.5, 0.25, 0.125...
                      init_regs={64 + 3: [0x3f800000, 0x3f000000, 0x3e800000, 0x3e000000, 0x3d800000, 0x3d000000, 0x3c800000, 0x3c000000]},
                      # 1.0, 2.0, 4.0, 8.0...
                      final_regs={64 + 2: [0x3f800000, 0x40000000, 0x40800000, 0x41000000, 0x41800000, 0x42000000, 0x42800000, 0x43000000]})

    def test_recip_vs(self):
        self.run_test([make_r(14, 64 + 2, 3, 0)],
                      init_regs={3: 0x3dfbe76d},  # 0.123
                      final_regs={64 + 2: [0x41021480, 0x41021480, 0x41021480, 0x41021480, 0x41021480, 0x41021480, 0x41021480, 0x41021480]})  # 8.125

    def test_recip_ss(self):
        self.run_test([make_r(14, 2, 3, 0)],
                      init_regs={3: 0x3dfbe76d},  # 0.123
                      final_regs={2: 0x41021480})  # 8.125

    def test_f2i(self):
        self.run_test([make_r(15, 2, 3, 0)],
                      init_regs={3: 0x4144cccd},  # 12.3
                      final_regs={2: 12})

    def test_i2f(self):
        self.run_test([make_r(16, 2, 3, 0)],
                      init_regs={3: 17},
                      final_regs={2: 0x41880000})

    def test_setgtf(self):
        self.run_test([make_r(17, 2, 64 + 3, 4)],
                      # -3.0, -2.0, -1.0, 0.0, 1.0, 2.0, 3.0, 4.0
                      init_regs={64 + 3: [0xc0400000, 0xc0000000, 0xbf800000, 0, 0x3f800000, 0x40000000, 0x40400000, 0x40800000],
                                 4: 0x3f800000}, # 1.0
                      final_regs={2: 0b11100000})

    def test_setgei(self):
        self.run_test([make_r(19, 2, 64 + 3, 4)],
                      init_regs={64 + 3: [-3, -2, -1, 0, 1, 2, 3, 4],
                                 4: 1},
                      final_regs={2: 0b11110000})

    def test_setlti(self):
        self.run_test([make_r(20, 2, 64 + 3, 4)],
                      init_regs={64 + 3: [-3, -2, -1, 0, 1, 2, 3, 4],
                                 4: 1},
                      final_regs={2: 0b00001111})

    # When these negatives are interpreted as unsigned, they will be large values
    def test_setgeu(self):
        self.run_test([make_r(21, 2, 64 + 3, 4)],
                      init_regs={64 + 3: [-3, -2, -1, 0, 1, 2, 3, 4],
                                 4: 1},
                      final_regs={2: 0b11110111})

    def test_setltu(self):
        self.run_test([make_r(22, 2, 64 + 3, 4)],
                      init_regs={64 + 3: [-3, -2, -1, 0, 1, 2, 3, 4],
                                 4: 1},
                      final_regs={2: 0b00001000})

    def test_seteq(self):
        self.run_test([make_r(23, 2, 64 + 3, 4)],
                      init_regs={64 + 3: [-3, -2, -1, 0, 1, 2, 3, 4],
                                 4: 1},
                      final_regs={2: 0b00010000})

    def test_setne(self):
        self.run_test([make_r(24, 2, 64 + 3, 4)],
                      init_regs={64 + 3: [-3, -2, -1, 0, 1, 2, 3, 4],
                                 4: 1},
                      final_regs={2: 0b11101111})

    def test_bnz_taken(self):
        self.run_test([
                make_b(25, 2, 1), # This will jump over XOR, leaving reg intact
                make_r(2, 3, 3, 3) # XOR r3, r3, r3
            ],
            init_regs={2: 1, 3: 0xffffffff},
            final_regs={3: 0xffffffff})

    def test_bnz_not_taken(self):
        self.run_test([
                make_b(25, 2, 1),
                make_r(3, 3, 3, 3) # XOR r3, r3, r3
            ],
            init_regs={2: 0, 3: 0xffffffff},
            final_regs={3: 0})

    def test_bz_taken(self):
        self.run_test([
                make_b(26, 2, 1), # This will jump over XOR, leaving reg intact
                make_r(3, 3, 3, 3) # XOR r3, r3, r3
            ],
            init_regs={2: 0, 3: 0xffffffff},
            final_regs={3: 0xffffffff})

    def test_bz_not_taken(self):
        self.run_test([
                make_b(26, 2, 1),
                make_r(3, 3, 3, 3) # XOR r3, r3, r3
            ],
            init_regs={2: 1, 3: 0xffffffff},
            final_regs={3: 0})

    def test_j(self):
        self.run_test([
                make_b(27, 0, 1),
                make_r(3, 3, 3, 3) # XOR r3, r3, r3
            ],
            init_regs={3: 0xffffffff},
            final_regs={3: 0xffffffff})

    def test_loadlo_s(self):
        self.run_test([
                make_k(28, 3, 0xabcd),
            ],
            init_regs={3: 0xffffffff},
            final_regs={3: 0xffffabcd})

    def test_loadlo_v(self):
        self.run_test([
                make_k(28, 64 + 3, 0xabcd),
            ],
            init_regs={64 + 3: [0x11111111, 0x22222222, 0x33333333, 0x44444444, 0x55555555, 0x66666666, 0x77777777, 0x88888888]},
            final_regs={64 + 3: [0x1111abcd, 0x2222abcd, 0x3333abcd, 0x4444abcd, 0x5555abcd, 0x6666abcd, 0x7777abcd, 0x8888abcd]})

    def test_loadhi_s(self):
        self.run_test([
                make_k(29, 3, 0xabcd),
            ],
            init_regs={3: 0xffffffff},
            final_regs={3: 0xabcdffff})

    def test_loadhi_v(self):
        self.run_test([
                make_k(29, 64 + 3, 0xabcd),
            ],
            init_regs={64 + 3: [0x11111111, 0x22222222, 0x33333333, 0x44444444, 0x55555555, 0x66666666, 0x77777777, 0x88888888]},
            final_regs={64 + 3: [0xabcd1111, 0xabcd2222, 0xabcd3333, 0xabcd4444, 0xabcd5555, 0xabcd6666, 0xabcd7777, 0xabcd8888]})

    def test_fmin(self):
        self.run_test([make_r(30, 2, 3, 4)],
                      init_regs={3: 0x402df8a1, 4: 0x40490e56}, # 2.7183, 3.1415
                      final_regs={2: 0x402df8a1})  # 2.7183

    def test_fmax(self):
        self.run_test([make_r(31, 2, 3, 4)],
                      init_regs={3: 0x402df8a1, 4: 0x40490e56}, # 2.7183, 3.1415
                      final_regs={2: 0x40490e56})  # 3.1415

    def test_fabs(self):
        self.run_test([make_r(32, 2, 3, 0)],
                      init_regs={3: 0xc02df8a1}, # -2.7183
                      final_regs={2: 0x402df8a1})  # 2.7183

    def test_get_laneid(self):
        cpu = emulate.Emulator()

        cpu.instructions += [
            make_r(2, 66, 112, 53), # move v2, LANE_ID
            HALT
        ]

        cpu.run()

        self.assertEqual(cpu.get_register(66), [0,1,2,3,4,5,6,7])

    def test_fault_illegal_scalar_source1(self):
        cpu = emulate.Emulator()

        cpu.instructions += [
            make_r(1, 2, 64 + 3, 4), # and r2, v3, r4
            HALT
        ]

        with self.assertRaises(emulate.RuntimeFault) as context:
            cpu.run()

        assert 'Illegal source register v3 for scalar destination r2' in str(context.exception)

    def test_fault_illegal_scalar_source2(self):
        cpu = emulate.Emulator()

        cpu.instructions += [
            make_r(1, 2, 3, 64 + 4), # and r2, r3, v4
            HALT
        ]

        with self.assertRaises(emulate.RuntimeFault) as context:
            cpu.run()

        assert 'Illegal source register v4 for scalar destination r2' in str(context.exception)

    def test_invalid_opcode(self):
        cpu = emulate.Emulator()

        cpu.instructions += [
            0xffffffff, # Invalid opcode
            HALT
        ]

        with self.assertRaises(emulate.RuntimeFault) as context:
            cpu.run()

        assert 'Illegal instruction' in str(context.exception)


if __name__ == "__main__":
    unittest.main()
