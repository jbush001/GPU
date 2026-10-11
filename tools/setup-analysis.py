#
#   Copyright 2026 Jeff Bush
#
#   Licensed under the Apache License, Version 2.0 (the 'License');
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an 'AS IS' BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#

"""
This uses a SAT solver to find an optimal program schedule for a given
program and VLIW-ish datapath. It allows exploration of different pipeline
configurations to find the best.
"""

import z3
from collections import Counter
from enum import Enum, auto
import time

class OpCode(Enum):
    load = auto()
    recip = auto()
    fsub = auto()
    fadd = auto()
    fmul = auto()
    ftoi = auto()
    flte = auto()
    igtr = auto()
    eq = auto()
    and_ = auto()
    or_ = auto()
    select = auto()
    iadd = auto()
    const = auto()
    store = auto()

class Value:
    """A node in the immediate representation of the program."""
    def __init__(self, name):
        self.name = name

    def __str__(self):
        if isinstance(self.name, int):
            return f'{self.name}'
        else:
            return f'%{self.name}'

    def __sub__(self, other):
        return Instruction(OpCode.fsub, self, other)

    def __add__(self, other):
        return Instruction(OpCode.fadd, self, other)

    def __mul__(self, other):
        return Instruction(OpCode.fmul, self, other)

    def __le__(self, other):
        return Instruction(OpCode.flte, self, other)

    def recip(self):
        return Instruction(OpCode.recip, self)

    def ftoi(self):
        return Instruction(OpCode.ftoi, self)

class Instruction(Value):
    """
    An instruction in the program DAG. All instructions are Values, which
    implicitly enforces SSA form.
    """
    def __init__(self, opcode, *operands):
        name = f'inst_{len(Program.current.instructions)}'
        super().__init__(name)
        self.opcode = opcode
        self.operands = list(operands)
        self.id = Program.current.emit(self)
        self.start_cycle = None
        self.pipeline = None
        self.latency = None

    def get_arg_str(self):
        return ', '.join(f'%{op.id}' if isinstance(op, Instruction) else str(op) for op in self.operands)

    def __str__(self):
        operand_str = self.get_arg_str()
        return f'%{self.id} = {self.opcode.name} {operand_str}'

class StoreInstruction(Instruction):
    """Store instructions are special in that they don't produce a value."""
    def __init__(self, dest, *operands):
        super().__init__(OpCode.store, dest, *operands)

    def __str__(self):
        operand_str = self.get_arg_str()
        return f'store {operand_str}'

class Program:
    current = None

    def __init__(self):
        # The instruction list is automatically topologically ordered.
        self.instructions = []

    def emit(self, instruction):
        self.instructions.append(instruction)
        return len(self.instructions) - 1

    def __enter__(self):
        assert(Program.current is None)
        Program.current = self
        return self

    def __exit__(self, *exc):
        Program.current = None

    def print(self):
        """Print the program DAG."""
        for node in self.instructions:
            print(node)

    def get_op_type_counts(self):
        """Return the count of each operation type in the program as a dictionary."""
        op_counter = Counter(node.opcode for node in self.instructions)
        return dict(op_counter)

    # Each pipeline is (name, capacity, latency, op_list)
    def compile(self, pipelines):
        program = self.schedule(pipelines)
        program = self.allocate_registers(program)
        return program

    def schedule(self, pipelines):
        # Map all instructions to a specific pipeline
        instr_to_pipe = {}
        for index, (_, capacity, _, op_list) in enumerate(pipelines):
            for op in op_list:
                instr_to_pipe[op] = index

        for node in self.instructions:
            node.pipeline = instr_to_pipe[node.opcode]
            node.latency = pipelines[node.pipeline][2]

        optimizer = z3.Optimize()

        # This is what we are solving for: assigning each node a start cycle
        for node in self.instructions:
            node.start_cycle = z3.Int(f'start_{node.id}')

        # Now specify constraints for the solver.
        # First, every node needs to start at time >= 0.
        for node in self.instructions:
            optimizer.add(node.start_cycle >= 0)

        # Add all RAW dependency constraints
        for node in self.instructions:
            for operand in node.operands:
                if isinstance(operand, Instruction):
                    optimizer.add(node.start_cycle >= operand.start_cycle + operand.latency)

        worst_case_duration = sum(pipelines[node.pipeline][2] for node in self.instructions)

        # We want to minimize the total program duration, so encode
        # that specifially as a constraint.
        total_duration = z3.Int('total_duration')
        for node in self.instructions:
            optimizer.add(total_duration >= node.start_cycle + node.latency)

        optimizer.add(total_duration >= 0)
        optimizer.add(total_duration <= worst_case_duration)
        optimizer.minimize(total_duration)

        # Add pipeline capacity constraints
        for index, (_, capacity, _, op_list) in enumerate(pipelines):
            pipeline_nodes = [node for node in self.instructions if node.pipeline == index]
            for cycle in range(worst_case_duration):
                optimizer.add(z3.Sum([z3.If(node.start_cycle == cycle, 1, 0) for node in pipeline_nodes]) <= capacity)

        start_time = time.time()
        if optimizer.check() != z3.sat:
            raise Exception('Unsatisfiable scheduling constraints')

        model = optimizer.model()
        end_time = time.time()
        print(f'Solved instruction scheduling in {end_time - start_time} seconds')

        # Flatten into a program, convert to three-address form.
        total_cycles = model.eval(total_duration).as_long() - 1
        program = [[] for _ in range(total_cycles + 1)]
        for node in self.instructions:
            start = model.eval(node.start_cycle).as_long()
            operands = [operand.id if isinstance(operand, Instruction) else str(operand) for operand in node.operands]
            if isinstance(node, StoreInstruction):
                # Store doesn't have a destination register
                program[start].append((node.pipeline, node.opcode.name, *operands))
            else:
                program[start].append((node.pipeline, node.opcode.name, node.id, *operands))

        return program

    def allocate_registers(self, program):
        """Map from SSA virtual registers to physical registers"""
        virt_to_phys_reg = {}
        free_registers = []
        last_use = {}

        # Determine which virtual registers are killed in which cycles
        for cycle, bundle in reversed(list(enumerate(program))):
            for inst in bundle:
                for source_op in inst[3:]:
                    if isinstance(source_op, int):
                        if source_op not in last_use:
                            last_use[source_op] = cycle

        kill_list = {}
        for virt_reg, cycle in last_use.items():
            kill_list[cycle] = kill_list.get(cycle, []) + [virt_reg]

        next_phys_reg = 0

        # Create the virtual to physical mapping list
        for cycle, bundle in enumerate(program):
            for (_pipeline, opcode, dest, *rest) in bundle:
                if (opcode != 'store'):
                    assert(dest not in virt_to_phys_reg)
                    if free_registers:
                        virt_to_phys_reg[dest] = free_registers.pop()
                    else:
                        virt_to_phys_reg[dest] = next_phys_reg
                        next_phys_reg += 1

            # Free registers for killed virtual registers
            for virt_reg in kill_list.get(cycle, []):
                assert(virt_reg in virt_to_phys_reg)
                free_registers.append(virt_to_phys_reg[virt_reg])

        # Rewrite and replace with physical registers
        return next_phys_reg, [
            [
                tuple([inst[0]] + [
                    virt_to_phys_reg[operand] if isinstance(operand, int) else operand
                    for operand in inst[1:]
                ])
                for inst in bundle
            ]
            for bundle in program
        ]

# Build the the program using an embedded DSL.
with Program() as program:
    # Depth interpolator
    z0 = Instruction(OpCode.load, Value('z0'))
    z1 = Instruction(OpCode.load, Value('z1'))
    z2 = Instruction(OpCode.load, Value('z2'))
    invw0 = z0.recip()
    invw1 = z1.recip()
    invw2 = z2.recip()
    invdw1 = invw1 - invw0
    invdw2 = invw2 - invw0
    StoreInstruction(Value('invw0'), invw0)
    StoreInstruction(Value('invw1'), invw1)
    StoreInstruction(Value('invw2'), invw2)
    StoreInstruction(Value('invdw1'), invdw1)
    StoreInstruction(Value('invdw2'), invdw2)

    # Rasterizer
    x0 = Instruction(OpCode.load, Value('x0'))
    y0 = Instruction(OpCode.load, Value('y0'))
    x1 = Instruction(OpCode.load, Value('x1'))
    y1 = Instruction(OpCode.load, Value('y1'))
    x2 = Instruction(OpCode.load, Value('x2'))
    y2 = Instruction(OpCode.load, Value('y2'))

    dx0 = y0 - y1
    dy0 = x1 - x0
    iv0 = ((Value('bbleft') - x0) * dx0) + ((Value('bbTop') - y0) * dy0)
    area2 = iv0

    xStep0 = dx0 * Value('xPixelPitch')
    yStep0 = dy0 * Value('yPixelPitch')

    dx1 = y1 - y2
    dy1 = x2 - x1
    iv1 = ((Value('bbleft') - x1) * dx1) + ((Value('bbTop') - y1) * dy1)
    area2 = area2 + iv1

    xStep1 = dx1 * Value('xPixelPitch')
    yStep1 = dy1 * Value('yPixelPitch')

    dx2 = y2 - y0
    dy2 = x0 - x2
    iv2 = ((Value('bbleft') - x2) * dx2) + ((Value('bbTop') - y2) * dy2)
    area2 = area2 + iv2

    xStep2 = dx2 * Value('xPixelPitch')
    yStep2 = dy2 * Value('yPixelPitch')

    # XXX implement backface/colinear culling if area2 <= 0
    cull = area2 <= Value(0)

    normFactor = area2.recip()

    normX0 = normFactor * x0
    normY0 = normFactor * y0
    normIv0 = normFactor * iv0
    x0int = normX0.ftoi()
    y0int = normY0.ftoi()
    iv0int = normIv0.ftoi()

    # Apply top-left pixel hit rule
    if False:
        topLeft0_a = Instruction(OpCode.igtr, xStep0, 0)
        topLeft0_b = Instruction(OpCode.eq, xStep0, 0)
        topLeft0_c = Instruction(OpCode.igtr, yStep0, 0)
        topLeft0_d = Instruction(OpCode.and_, topLeft0_b, topLeft0_c)
        topLeft0 = Instruction(OpCode.or_, topLeft0_a, topLeft0_d)
        iv0int = Instruction(OpCode.select, topLeft0, Instruction(OpCode.iadd, iv0int, 1), iv0int)

    StoreInstruction(Value('xStep0'), x0int)
    StoreInstruction(Value('yStep0'), y0int)
    StoreInstruction(Value('iv0'), iv0int)

    normX1 = normFactor * x1
    normY1 = normFactor * y1
    normIv1 = normFactor * iv1
    x1int = normX1.ftoi()
    y1int = normY1.ftoi()
    iv1int = normIv1.ftoi()

    # Apply top-left pixel hit rule
    if False:
        topLeft1_a = Instruction(OpCode.igtr, xStep1, 0)
        topLeft1_b = Instruction(OpCode.eq, xStep1, 0)
        topLeft1_c = Instruction(OpCode.igtr, yStep1, 0)
        topLeft1_d = Instruction(OpCode.and_, topLeft1_b, topLeft1_c)
        topLeft1 = Instruction(OpCode.or_, topLeft1_a, topLeft1_d)
        iv1int = Instruction(OpCode.select, topLeft1, Instruction(OpCode.iadd, iv1int, 1), iv1int)

    StoreInstruction(Value('xStep1'), x1int)
    StoreInstruction(Value('yStep1'), y1int)
    StoreInstruction(Value('iv1'), iv1int)

    normX2 = normFactor * x2
    normY2 = normFactor * y2
    normIv2 = normFactor * iv2
    x2int = normX2.ftoi()
    y2int = normY2.ftoi()
    iv2int = normIv2.ftoi()

    # Apply top-left pixel hit rule
    if False:
        topLeft2_a = Instruction(OpCode.igtr, xStep2, 0)
        topLeft2_b = Instruction(OpCode.eq, xStep2, 0)
        topLeft2_c = Instruction(OpCode.igtr, yStep2, 0)
        topLeft2_d = Instruction(OpCode.and_, topLeft2_b, topLeft2_c)
        topLeft2 = Instruction(OpCode.or_, topLeft2_a, topLeft2_d)
        iv2int = Instruction(OpCode.select, topLeft2, Instruction(OpCode.iadd, iv2int, 1), iv2int)

    StoreInstruction(Value('xStep2'), x2int)
    StoreInstruction(Value('yStep2'), y2int)
    StoreInstruction(Value('iv2'), iv2int)

    program.print()

    print('---------------------------------------------------------------------')
    print('\nOperation type counts')
    for op_name, count in program.get_op_type_counts().items():
        print(f'{op_name}: {count}')

    print('---------------------------------------------------------------------')

    num_regs, program = program.compile([
        ('loadstore', 1, 1, [OpCode.load, OpCode.store]),
        ('multiply', 1, 2, [OpCode.fmul]),
        ('add', 1, 3, [OpCode.fsub, OpCode.fadd, OpCode.ftoi, OpCode.flte, OpCode.igtr, OpCode.eq, OpCode.and_, OpCode.or_, OpCode.select, OpCode.iadd]),
        ('reciprocal', 1, 5, [OpCode.recip])])

    print(f'physical registers used {num_regs}')
    for pc, bundle in enumerate(program):
        print(f'{pc} {bundle}')

