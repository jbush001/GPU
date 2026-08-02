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
import assemble
import struct

def convert_to_reg(value):
    if isinstance(value, float):
        return struct.unpack('I', struct.pack('f', value))[0] & 0xffffffff
    else:
        assert(isinstance(value, int))
        return value & 0xffffffff

class TestAsmEmuIntegration(unittest.TestCase):
    def run_program(self, source, init_regs, final_regs):
        asm = assemble.Assembler()
        asm.assemble(source)
        cpu = emulate.Emulator()
        cpu.instructions += asm.code
        cpu.instructions += [0]

        for reg, value in init_regs.items():
            if reg >= 64:
                assert(isinstance(value, list))
                for i in range(8):
                    cpu.registers[reg][i] = convert_to_reg(value[i])
            else:
                cpu.registers[reg] = convert_to_reg(value)

        cpu.run()
        for reg, value in final_regs.items():
            if reg >= 64:
                assert(isinstance(value, list))
                for i in range(8):
                    if cpu.registers[reg][i] != convert_to_reg(value[i]):
                        self.fail(f'mismatch: register {reg} lane {i} expected {convert_to_reg(value[i]):08x} got {cpu.registers[reg][i]:08x}')
            else:
                if cpu.registers[reg] != convert_to_reg(value):
                    self.fail(f'mismatch: register {reg} expected {convert_to_reg(value):08x} got {cpu.registers[reg]:08x}')

    def test_lerp(self):
        self.run_program(
            source = '''
                subf v2, v1, v0 ; B - A
                mulf v2, v2, r0 ; (B - A) * t
                addf v3, v0, v2 ; A + t * (B - A)
            ''',
            init_regs = {
                0: 0.25, # Scale vector
                64: [0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0], # starting values
                65: [200.0, 190.0, 180.0, 170.0, 160.0, 150.0, 140.0, 130.0] # Ending values
            },
            final_regs = {
                67: [50.0, 55.0, 60.0, 65.0, 70.0, 75.0, 80.0, 85.0]
            }
        )



