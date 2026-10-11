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
This uses a SAT solver to compile an optimal program schedule for a given
VLIW-ish datapath. It allows exploration of different pipeline configurations
to find the best.
"""

import z3
from collections import Counter

class Operation:
    """Represents a type of operation"""
    def __init__(self, name):
        self.name = name

    def __call__(self, *operands):
        return Program.current.add_node(self, list(operands))

OP_LOAD = Operation('load')
OP_RECIP = Operation('recip')
OP_FSUB = Operation('fsub')
OP_FADD = Operation('fadd')
OP_FMUL = Operation('fmul')
OP_FTOI = Operation('ftoi')
OP_FLTE = Operation('flte')
OP_IGTR = Operation('igtr')
OP_EQ = Operation('eq')
OP_AND = Operation('and')
OP_OR = Operation('or')
OP_SELECT = Operation('select')
OP_IADD = Operation('iadd')
OP_CONST = Operation('const')
OP_STORE = Operation('store')

class OpNode:
    """Instance of an operation in the program DAG"""
    def __init__(self, operation, operands, id):
        self.operation = operation
        self.operands = operands
        self.id = id

        # These will be set during compilation
        self.start_cycle = None
        self.pipeline = None
        self.latency = None

    def __sub__(self, other):
        return OP_FSUB(self, other)

    def __add__(self, other):
        return OP_FADD(self, other)

    def __mul__(self, other):
        return OP_FMUL(self, other)

    def ftoi(self):
        return OP_FTOI(self)

    def recip(self):
        return OP_RECIP(self)

    def __str__(self):
        def fmt_op(element):
            if isinstance(element, OpNode):
                return '%' + str(element.id)
            elif isinstance(element, int):
                return str(element)
            else:
                return '%' + str(element)

        operand_str = ', '.join(fmt_op(op) for op in self.operands)
        if self.operation.name in ('store', 'loop_start', 'loop_end'):
            return f'{self.operation.name} {operand_str}'

        return f'%{self.id} = {self.operation.name} {operand_str}'

class Program:
    """Represent the algorithm in DAG form"""
    current = None

    def __init__(self):
        self.nodes = []

    def add_node(self, operation, operands):
        node = OpNode(operation, operands, len(self.nodes))
        self.nodes.append(node)
        return node

    def __enter__(self):
        assert(Program.current is None)
        Program.current = self
        return self

    def __exit__(self, *exc):
        Program.current = None

    def print(self):
        """Print the program DAG."""
        for node in self.nodes:
            print(node)

    def get_op_type_counts(self):
        """Return the count of each operation type in the program as a dictionary."""
        op_counter = Counter(node.operation.name for node in self.nodes)
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

        for node in self.nodes:
            node.pipeline = instr_to_pipe[node.operation.name]
            node.latency = pipelines[node.pipeline][2]

        optimizer = z3.Optimize()

        # This is what we are solving for: assigning each node a start cycle
        for node in self.nodes:
            node.start_cycle = z3.Int(f'start_{node.id}')

        # Now specify constraints for the solver.
        # First, every node needs to start at time >= 0.
        for node in self.nodes:
            optimizer.add(node.start_cycle >= 0)

        # Add all RAW dependency constraints
        for node in self.nodes:
            for operand in node.operands:
                if isinstance(operand, OpNode):
                    operand_latency = pipelines[operand.pipeline][2]
                    optimizer.add(node.start_cycle >= operand.start_cycle + operand.latency)

        # We want to minimize the total program duration, so encode
        # that specifially as an objective
        total_duration = z3.Int('total_duration')
        for node in self.nodes:
            optimizer.add(total_duration >= node.start_cycle + node.latency)

        worst_case = sum(pipelines[node.pipeline][2] for node in self.nodes)
        optimizer.add(total_duration >= 0)
        optimizer.add(total_duration <= worst_case)
        optimizer.minimize(total_duration)

        # Add pipeline capacity constraints
        for index, (_, capacity, _, op_list) in enumerate(pipelines):
            pipeline_nodes = [node for node in self.nodes if node.pipeline == index]
            for cycle in range(worst_case):
                optimizer.add(z3.Sum([z3.If(node.start_cycle == cycle, 1, 0) for node in pipeline_nodes]) <= capacity)

        if optimizer.check() != z3.sat:
            raise Exception('Unsatisfiable scheduling constraints')

        model = optimizer.model()

        # Flatten into a program
        total_cycles = model.eval(total_duration).as_long() - 1
        program = [[] for _ in range(total_cycles + 1)]
        for node in self.nodes:
            start = model.eval(node.start_cycle).as_long()
            operands = [operand.id if isinstance(operand, OpNode) else operand for operand in node.operands]
            if node.operation.name == 'store':
                # Store doesn't have a destination register
                program[start].append((node.operation.name, *operands))
            else:
                program[start].append((node.operation.name, node.id, *operands))

        return program

    def allocate_registers(self, program):
        # Map from SSA virtual registers to physical registers
        virt_to_phys_reg = {}
        free_registers = []
        last_use = {}

        # Determine which virtual registers are killed in which cycles
        for cycle, bundle in reversed(list(enumerate(program))):
            for inst in bundle:
                for source_op in inst[2:]:
                    if isinstance(source_op, int):
                        if source_op not in last_use:
                            last_use[source_op] = cycle

        kill_list = {}
        for virt_reg, cycle in last_use.items():
            kill_list[cycle] = kill_list.get(cycle, []) + [virt_reg]

        next_phys_reg = 0

        # Create the virtual to physical mapping list
        for cycle, bundle in enumerate(program):
            for inst in bundle:
                if (inst[0] != 'store'):
                    dest_reg = inst[1]
                    assert(dest_reg not in virt_to_phys_reg)
                    if free_registers:
                        virt_to_phys_reg[dest_reg] = free_registers.pop()
                    else:
                        virt_to_phys_reg[dest_reg] = next_phys_reg
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


with Program() as program:
    # Depth interpolator
    z0 = OP_LOAD('z0')
    z1 = OP_LOAD('z1')
    z2 = OP_LOAD('z2')
    invw0 = z0.recip()
    invw1 = z1.recip()
    invw2 = z2.recip()
    invdw1 = invw1 - invw0
    invdw2 = invw2 - invw0

    # Rasterizer
    x0 = OP_LOAD('x0')
    y0 = OP_LOAD('y0')
    x1 = OP_LOAD('x1')
    y1 = OP_LOAD('y1')
    x2 = OP_LOAD('x2')
    y2 = OP_LOAD('y2')

    dx0 = x1 - x0
    dy0 = y1 - y0
    t0 = OP_FSUB('bbleft', x0)
    t1 = t0 * dx0
    t2 = OP_FSUB('bbTop', y0)
    t3 = t2 * dy0
    iv0 = t2 + t3
    area2 = iv0

    xStep0 = OP_FMUL(dx0, 'xPixelPitch')
    yStep0 = OP_FMUL(dy0, 'yPixelPitch')

    dx1 = x2 - x1
    dy1 = y2 - y1
    t0 = OP_FSUB('bbleft', x1)
    t1 = t0 * dx1
    t2 = OP_FSUB('bbTop', y1)
    t3 = t2 * dy1
    iv1 = t2 + t3
    area2 = area2 + iv1

    xStep1 = OP_FMUL(dx1, 'xPixelPitch')
    yStep1 = OP_FMUL(dy1, 'yPixelPitch')

    dx2 = x0 - x2
    dy2 = y0 - y2
    t0 = OP_FSUB('bbleft', x2)
    t1 = t0 * dx2
    t2 = OP_FSUB('bbTop', y2)
    t3 = t2 * dy2
    iv2 = t2 + t3
    area2 = area2 + iv2

    xStep2 = OP_FMUL(dx2, 'xPixelPitch')
    yStep2 = OP_FMUL(dy2, 'yPixelPitch')

    # XXX implement backface/colinear culling if area2 <= 0
    cull = OP_FLTE(area2, 0)


    normFactor = area2.recip()

    normX0 = normFactor * x0
    normY0 = normFactor * y0
    normIv0 = normFactor * iv0
    x0int = normX0.ftoi()
    y0int = normY0.ftoi()
    iv0int = normIv0.ftoi()

    # Apply top-left pixel hit rule
    if False:
        topLeft0_a = OP_IGTR(xStep0, 0)
        topLeft0_b = OP_EQ(xStep0, 0)
        topLeft0_c = OP_IGTR(yStep0, 0)
        topLeft0_d = OP_AND(topLeft0_b, topLeft0_c)
        topLeft0 = OP_OR(topLeft0_a, topLeft0_d)
        iv0int = OP_SELECT(topLeft0, OP_IADD(iv0int, 1), iv0int)

    OP_STORE('xStep0', x0int)
    OP_STORE('yStep0', y0int)
    OP_STORE('iv0', iv0int)

    normX1 = normFactor * x1
    normY1 = normFactor * y1
    normIv1 = normFactor * iv1
    x1int = normX1.ftoi()
    y1int = normY1.ftoi()
    iv1int = normIv1.ftoi()

    # Apply top-left pixel hit rule
    if False:
        topLeft1_a = OP_IGTR(xStep1, 0)
        topLeft1_b = OP_EQ(xStep1, 0)
        topLeft1_c = OP_IGTR(yStep1, 0)
        topLeft1_d = OP_AND(topLeft1_b, topLeft1_c)
        topLeft1 = OP_OR(topLeft1_a, topLeft1_d)
        iv1int = OP_SELECT(topLeft1, OP_IADD(iv1int, 1), iv1int)

    OP_STORE('xStep1', x1int)
    OP_STORE('yStep1', y1int)
    OP_STORE('iv1', iv1int)

    normX2 = normFactor * x2
    normY2 = normFactor * y2
    normIv2 = normFactor * iv2
    x2int = normX2.ftoi()
    y2int = normY2.ftoi()
    iv2int = normIv2.ftoi()

    # Apply top-left pixel hit rule
    if False:
        topLeft2_a = OP_IGTR(xStep2, 0)
        topLeft2_b = OP_EQ(xStep2, 0)
        topLeft2_c = OP_IGTR(yStep2, 0)
        topLeft2_d = OP_AND(topLeft2_b, topLeft2_c)
        topLeft2 = OP_OR(topLeft2_a, topLeft2_d)
        iv2int = OP_SELECT(topLeft2, OP_IADD(iv2int, 1), iv2int)

    OP_STORE('xStep2', x2int)
    OP_STORE('yStep2', y2int)
    OP_STORE('iv2', iv2int)

    program.print()

    print('---------------------------------------------------------------------')
    print('\nOperation type counts')
    for op_name, count in program.get_op_type_counts().items():
        print(f'{op_name}: {count}')

    print('---------------------------------------------------------------------')

    num_regs, program = program.compile([
        ('loadstore', 1, 1, ['load', 'store']),
        ('fmul', 1, 2, ['fmul']),
        ('fadd', 1, 3, ['fsub', 'fadd', 'ftoi', 'flte', 'igtr', 'eq', 'and', 'or', 'select' ,'iadd']),
        ('frecip', 1, 5, ['recip'])])

    print(f'physical registers used {num_regs}')
    for pc, bundle in enumerate(program):
        print(f'{pc} {bundle}')

