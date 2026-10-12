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
The current design direction for the triangle setup engine is to use a
microcoded state machine to control several computational pipelines. In
order to produce an optimal implementation from an area, latency, and
throughput perspective, this uses the z3 SMT solver to find a the optimal
encoding for a given microprogram and specified datapath. It works by first
specifying the program in SSA/DAG format using an embedded domain specific
language, then building mathematical proofs from that representation and
solving for the set of parameters that satisfies them. This allows
experimentation with various configurations to explore tradeoffs.
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
    move = auto()

class Value:
    """A node in the immediate representation of the program."""
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

class Literal(Value):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)

class SpecialRegister(Value):
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return f'%{self.name}'

class Instruction(Value):
    """
    An instruction in the program DAG. All instructions are Values, which
    implicitly enforces static single assignment (SSA) form.
    """
    def __init__(self, opcode, *operands):
        self.opcode = opcode
        self.operands = list(operands)
        self.id = Program.current.emit(self)
        self.start_cycle = None
        self.pipeline = None
        self.latency = None
        self.dest_reg = None

    def get_arg_str(self):
        return ', '.join(f'%{op.id}' if isinstance(op, Instruction) else str(op) for op in self.operands)

    def set_dest_reg(self, name):
        """
        Hard code the physical destination register for this instruction.
        This is used for special function registers. Normally the instruction
        is just an SSA value that will have registers allocated as needed during
        the register allocation phase.
        """
        self.dest_reg = name

    def __str__(self):
        operand_str = self.get_arg_str()
        if self.dest_reg is not None:
            return f'%{self.dest_reg} = {self.opcode.name} {operand_str}'
        else:
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
        self.validate_constraints(program)
        return program

    def schedule(self, pipelines):
        # Map all instructions to a specific pipeline
        instr_to_pipe = {}
        for index, (_, capacity, _, op_list) in enumerate(pipelines):
            for op in op_list:
                instr_to_pipe[op] = index

        for node in self.instructions:
            if node.opcode not in instr_to_pipe:
                raise ValueError(f"No pipeline assigned for opcode {node.opcode}")

            node.pipeline = instr_to_pipe[node.opcode]
            node.latency = pipelines[node.pipeline][2]

        solver = z3.Optimize()

        # This is what we are solving for: assigning each node a start cycle
        for node in self.instructions:
            node.start_cycle = z3.Int(f'start_{node.id}')

        # Now specify constraints for the solver.
        # First, every node needs to start at time >= 0.
        for node in self.instructions:
            solver.add(node.start_cycle >= 0)

        # Add all RAW dependency constraints
        for node in self.instructions:
            for operand in node.operands:
                if isinstance(operand, Instruction):
                    solver.add(node.start_cycle >= operand.start_cycle + operand.latency)

        worst_case_duration = sum(pipelines[node.pipeline][2] for node in self.instructions)

        # We want to minimize the total program duration, so encode
        # that specifially as a constraint.
        total_duration = z3.Int('total_duration')
        for node in self.instructions:
            solver.add(total_duration >= node.start_cycle + node.latency)

        solver.add(total_duration >= 0)
        solver.add(total_duration <= worst_case_duration)
        solver.minimize(total_duration)

        # Add pipeline capacity constraints
        for index, (_, capacity, _, op_list) in enumerate(pipelines):
            pipeline_nodes = [node for node in self.instructions if node.pipeline == index]
            for cycle in range(worst_case_duration):
                solver.add(z3.Sum([z3.If(node.start_cycle == cycle, 1, 0) for node in pipeline_nodes]) <= capacity)

        # XXX I don't have a constraint here for the number of source or dest operands,
        # but in practice I don't seem to be hitting that.

        start_time = time.time()
        if solver.check() != z3.sat:
            raise Exception('Unsatisfiable scheduling constraints')

        model = solver.model()
        end_time = time.time()
        print(f'Solved instruction scheduling in {end_time - start_time} seconds')

        # Flatten into a program, convert to three-address form.
        total_cycles = model.eval(total_duration).as_long()
        program = [[] for _ in range(total_cycles + 1)]
        for node in self.instructions:
            start = model.eval(node.start_cycle).as_long()
            def map_operand(operand):
                if isinstance(operand, Instruction):
                    if operand.dest_reg is not None:
                        return f'%{operand.dest_reg}'
                    else:
                        return operand.id
                else:
                    return str(operand)

            operands = [map_operand(operand) for operand in node.operands]
            if isinstance(node, StoreInstruction):
                # Store doesn't have a destination register
                program[start].append((node.pipeline, node.opcode.name, *operands))
            elif node.dest_reg is not None:
                program[start].append((node.pipeline, node.opcode.name, f'%{node.dest_reg}', *operands))
            else:
                program[start].append((node.pipeline, node.opcode.name, node.id, *operands))

        return program

    def compute_live_ranges(self, program):
        live_ranges = {}
        for cycle, bundle in enumerate(program):
            for inst in bundle:
                _pipeline, opcode, dest, *rest = inst
                if opcode != 'store' and isinstance(dest, int):
                    # This is strict SSA...
                    assert dest not in live_ranges, 'Redefined virtual register'
                    live_ranges[dest] = [cycle, cycle]

                for source_op in rest:
                    if isinstance(source_op, int):
                        assert source_op in live_ranges, "Use of undefined virtual register"
                        live_ranges[source_op][1] = cycle



        return live_ranges

    def build_register_interference_graph(self, live_ranges):
        interference_graph = []
        vregs = sorted(live_ranges.keys())
        for i in range(len(vregs)):
            for j in range(i + 1, len(vregs)):
                if live_ranges[vregs[i]][0] <= live_ranges[vregs[j]][1] and live_ranges[vregs[j]][0] <= live_ranges[vregs[i]][1]:
                    interference_graph.append((vregs[i], vregs[j]))

        return interference_graph

    def allocate_registers(self, program):
        live_ranges = self.compute_live_ranges(program)
        interference_edges = self.build_register_interference_graph(live_ranges)
        virtual_regs = list(live_ranges.keys())

        solver = z3.Optimize()

        # Hardware constants
        NUM_PHYS_REGS = 16
        MAX_READ_PORTS_PER_BANK = 2
        MAX_WRITE_PORTS_PER_BANK = 1

        # We are solving for this: virtual to physical register mapping.
        phys_reg_map = {v: z3.Int(f'phys_{v}') for v in virtual_regs}

        for v in virtual_regs:
            solver.add(phys_reg_map[v] >= 0)
            solver.add(phys_reg_map[v] < NUM_PHYS_REGS)

        def get_bank(p):
            return p & 1

        # Add interference constraints.
        for u, v in interference_edges:
            solver.add(phys_reg_map[u] != phys_reg_map[v])

        # Add bank and writeback constraints.
        for bundle in program:
            read_vregs = set()
            write_vregs = []

            for inst in bundle:
                _pipeline, opcode, dest, *operands = inst
                for op in operands:
                    if isinstance(op, int):
                        read_vregs.add(op)

                if opcode != 'store' and isinstance(dest, int):
                    write_vregs.append(dest)

            # Add constraints
            bank_0_reads = []
            bank_1_reads = []

            for v in read_vregs:
                for p in range(NUM_PHYS_REGS):
                    is_p = (phys_reg_map[v] == p)
                    if get_bank(p) == 0:
                        bank_0_reads.append(z3.If(is_p, 1, 0))
                    else:
                        bank_1_reads.append(z3.If(is_p, 1, 0))

            if bank_0_reads:
                solver.add(z3.Sum(bank_0_reads) <= MAX_READ_PORTS_PER_BANK)

            if bank_1_reads:
                solver.add(z3.Sum(bank_1_reads) <= MAX_READ_PORTS_PER_BANK)

            bank_0_writes = []
            bank_1_writes = []

            for dest_v in write_vregs:
                for p in range(NUM_PHYS_REGS):
                    is_p = (phys_reg_map[dest_v] == p)
                    if get_bank(p) == 0:
                        bank_0_writes.append(z3.If(is_p, 1, 0))
                    else:
                        bank_1_writes.append(z3.If(is_p, 1, 0))

            if bank_0_writes:
                solver.add(z3.Sum(bank_0_writes) <= MAX_WRITE_PORTS_PER_BANK)
            if bank_1_writes:
                solver.add(z3.Sum(bank_1_writes) <= MAX_WRITE_PORTS_PER_BANK)

        # XXX does not check special register writes

        # Solve
        start = time.time()
        if solver.check() == z3.sat:
            model = solver.model()
            virt_to_phys_reg = {v: model.eval(phys_reg_map[v]).as_long() for v in virtual_regs}
        else:
            raise Exception("Z3 could not find a valid register/bank allocation for this schedule!")

        end = time.time()
        print(f"Solved register allocation in {end - start} seconds")

        # Rewrite and replace with physical registers
        return [
            [
                tuple([inst[0]] + [
                    virt_to_phys_reg[operand] if isinstance(operand, int) else operand
                    for operand in inst[1:]
                ])
                for inst in bundle
            ]
            for bundle in program
        ]

    def validate_constraints(self, program):
        # Ensure there are no bank conflicts
        MAX_READ_PORTS_PER_BANK = 2
        MAX_WRITE_PORTS_PER_BANK = 1

        for bundle in program:
            bank_0_reads = 0
            bank_1_reads = 0
            bank_0_writes = 0
            bank_1_writes = 0
            for inst in bundle:
                dest_reg = inst[2]
                if isinstance(dest_reg, int):
                    if (dest_reg & 1) == 0:
                        bank_0_writes += 1
                    else:
                        bank_1_writes += 1

                for operand in inst[3:]:
                    if isinstance(operand, int):
                        if operand & 1 == 0:
                            bank_0_reads += 1
                        else:
                            bank_1_reads += 1

            if bank_0_reads > MAX_READ_PORTS_PER_BANK or bank_1_reads > MAX_READ_PORTS_PER_BANK:
                raise Exception("Bank conflict detected in read ports")
            if bank_0_writes > MAX_WRITE_PORTS_PER_BANK or bank_1_writes > MAX_WRITE_PORTS_PER_BANK:
                raise Exception("Bank conflict detected in write ports")

        print('Constraint validation passed')

use_load_store = False

# Generate the intermediate representation. These formula are implicitly
# translated into instruction nodes.
with Program() as program:
    # Set up depth interpolator coefficients
    if use_load_store:
        z0 = Instruction(OpCode.load, Value('z0'))
        z1 = Instruction(OpCode.load, Value('z1'))
        z2 = Instruction(OpCode.load, Value('z2'))
    else:
        z0 = SpecialRegister('z0')
        z1 = SpecialRegister('z1')
        z2 = SpecialRegister('z2')

    invw0 = z0.recip()
    invw1 = z1.recip()
    invw2 = z2.recip()
    invdw1 = invw1 - invw0
    invdw2 = invw2 - invw0
    if use_load_store:
        StoreInstruction('invw0', invw0)
        StoreInstruction('invw1', invw1)
        StoreInstruction('invw2', invw2)
        StoreInstruction('invdw1', invdw1)
        StoreInstruction('invdw2', invdw2)
    else:
        sprInvw0 = Instruction(OpCode.move, invw0)
        sprInvw1 = Instruction(OpCode.move, invw1)
        sprInvw2 = Instruction(OpCode.move, invw2)
        sprInvw0.set_dest_reg('invw0')
        sprInvw1.set_dest_reg('invw1')
        sprInvw2.set_dest_reg('invw2')
        invdw1.set_dest_reg('invdw1')
        invdw2.set_dest_reg('invdw2')

    # Set up rasterizer coefficients
    if use_load_store:
        x0 = Instruction(OpCode.load, 'x0')
        y0 = Instruction(OpCode.load, 'y0')
        x1 = Instruction(OpCode.load, 'x1')
        y1 = Instruction(OpCode.load, 'y1')
        x2 = Instruction(OpCode.load, 'x2')
        y2 = Instruction(OpCode.load, 'y2')
    else:
        x0 = SpecialRegister('x0')
        y0 = SpecialRegister('y0')
        x1 = SpecialRegister('x1')
        y1 = SpecialRegister('y1')
        x2 = SpecialRegister('x2')
        y2 = SpecialRegister('y2')

    dx0 = y0 - y1
    dy0 = x1 - x0
    iv0 = ((SpecialRegister('bbleft') - x0) * dx0) + ((SpecialRegister('bbTop') - y0) * dy0)
    area2 = iv0

    xStep0 = dx0 * SpecialRegister('xPixelPitch')
    yStep0 = dy0 * SpecialRegister('yPixelPitch')

    dx1 = y1 - y2
    dy1 = x2 - x1
    iv1 = ((SpecialRegister('bbleft') - x1) * dx1) + ((SpecialRegister('bbTop') - y1) * dy1)
    area2 = area2 + iv1

    xStep1 = dx1 * SpecialRegister('xPixelPitch')
    yStep1 = dy1 * SpecialRegister('yPixelPitch')

    dx2 = y2 - y0
    dy2 = x0 - x2
    iv2 = ((SpecialRegister('bbleft') - x2) * dx2) + ((SpecialRegister('bbTop') - y2) * dy2)
    area2 = area2 + iv2

    xStep2 = dx2 * SpecialRegister('xPixelPitch')
    yStep2 = dy2 * SpecialRegister('yPixelPitch')

    # XXX implement backface/colinear culling if area2 <= 0
    cull = area2 <= Literal(0)

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

    if use_load_store:
        StoreInstruction(Value('xStep0'), x0int)
        StoreInstruction(Value('yStep0'), y0int)
        StoreInstruction(Value('iv0'), iv0int)
    else:
        x0int.set_dest_reg('xStep0')
        y0int.set_dest_reg('yStep0')
        iv0int.set_dest_reg('iv0')

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

    if use_load_store:
        StoreInstruction('xStep1', x1int)
        StoreInstruction('yStep1', y1int)
        StoreInstruction('iv1', iv1int)
    else:
        x1int.set_dest_reg('xStep1')
        y1int.set_dest_reg('yStep1')
        iv1int.set_dest_reg('iv1')

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

    if use_load_store:
        StoreInstruction('xStep2', x2int)
        StoreInstruction('yStep2', y2int)
        StoreInstruction('iv2', iv2int)
    else:
        x2int.set_dest_reg('xStep2')
        y2int.set_dest_reg('yStep2')
        iv2int.set_dest_reg('iv2')

    program.print()

    print('---------------------------------------------------------------------')
    print('\nOperation type counts')
    for op_name, count in program.get_op_type_counts().items():
        print(f'{op_name}: {count}')

    print('---------------------------------------------------------------------')

    program = program.compile([
        ('loadstore', 1, 1, [OpCode.load, OpCode.store]),
        ('multiply', 1, 2, [OpCode.fmul]),
        ('add', 1, 3, [OpCode.fsub, OpCode.fadd, OpCode.ftoi, OpCode.flte, OpCode.igtr, OpCode.eq, OpCode.and_, OpCode.or_, OpCode.select, OpCode.iadd, OpCode.move]),
        ('reciprocal', 1, 5, [OpCode.recip])])

    for pc, bundle in enumerate(program):
        print(f'{pc} {bundle}')

