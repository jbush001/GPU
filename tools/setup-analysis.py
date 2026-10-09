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

"""
Allows exploration of implementation options for setup engine.
"""

from collections import Counter

class Operation:
    def __init__(self, name, latency):
        self.name = name
        self.latency = latency

    def __call__(self, *operands):
        return Program.current.add_node(self, list(operands))

OP_LOAD = Operation("load", 2)
OP_RECIP = Operation("recip", 5)
OP_FSUB = Operation("fsub", 3)
OP_FADD = Operation("fadd", 3)
OP_FMUL = Operation("fmul", 2)
OP_FMIN = Operation("fmin", 1)
OP_FMAX = Operation("fmax", 1)
OP_FTOI = Operation("ftoi", 1)
OP_IGTR = Operation("igtr", 1)
OP_EQ = Operation("eq", 1)
OP_AND = Operation("and", 1)
OP_OR = Operation("or", 1)
OP_SELECT = Operation("select", 1)
OP_IADD = Operation("iadd", 1)
OP_CONST = Operation("const", 0)


class OpNode:
    def __init__(self, operation, operands, id):
        self.operation = operation
        self.operands = operands
        self.id = id

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
        return f"%{self.id} = {self.operation.name} {operand_str}"

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
        for node in self.nodes:
            print(node)

    def critical_path(self):
        """Determine the 'speed of light' of this algorithm."""
        # These are already topologically sorted in self.nodes
        max_latency = [0] * len(self.nodes)
        for node in self.nodes:
            node_latency = node.operation.latency
            for operand in node.operands:
                if isinstance(operand, OpNode):
                    node_latency = max(node_latency, max_latency[operand.id]
                        + node.operation.latency)

            max_latency[node.id] = node_latency

        end_node = max(self.nodes, key=lambda n: max_latency[n.id], default=None)
        path = []
        curr = end_node
        while curr is not None:
            path.append((curr, max_latency[curr.id]))
            critical_parent = max((op for op in curr.operands if isinstance(op, OpNode)), key=lambda n: max_latency[n.id], default=None)
            curr = critical_parent

        path.reverse()  # Order from root/input to final output
        return max_latency[end_node.id] if end_node else 0, path

    def get_op_type_counts(self):
        """Return the count of each operation type in the program as a dictionary."""
        op_counter = Counter(node.operation.name for node in self.nodes)
        return dict(op_counter)

with Program() as program:
    z0 = OP_LOAD("z0")
    z1 = OP_LOAD("z1")
    z2 = OP_LOAD("z2")
    invw0 = z0.recip()
    invw1 = z1.recip()
    invw2 = z2.recip()
    invdw1 = invw1 - invw0
    invdw2 = invw2 - invw0

    x0 = OP_LOAD("x0")
    y0 = OP_LOAD("y0")
    x1 = OP_LOAD("x1")
    y1 = OP_LOAD("y1")
    x2 = OP_LOAD("x2")
    y2 = OP_LOAD("y2")

    # XXX these should be consts
    bbLeft = OP_CONST("bbLeft")
    bbTop = OP_CONST("bbTop")

    dx0 = x1 - x0
    dy0 = y1 - y0
    t0 = bbLeft - x0
    t1 = t0 * dx0
    t2 = bbTop - y0
    t3 = t2 * dy0
    iv0 = t2 + t3
    area2 = iv0

    xStep0 = OP_FMUL(dx0, "xPixelPitch")
    yStep0 = OP_FMUL(dy0, "yPixelPitch")

    dx1 = x2 - x1
    dy1 = y2 - y1
    t0 = bbLeft - x1
    t1 = t0 * dx1
    t2 = bbTop - y1
    t3 = t2 * dy1
    iv1 = t2 + t3
    area2 = area2 + iv1

    xStep1 = OP_FMUL(dx1, "xPixelPitch")
    yStep1 = OP_FMUL(dy1, "yPixelPitch")

    dx2 = x0 - x2
    dy2 = y0 - y2
    t0 = bbLeft - x2
    t1 = t0 * dx2
    t2 = bbTop - y2
    t3 = t2 * dy2
    iv2 = t2 + t3
    area2 = area2 + iv2

    xStep2 = OP_FMUL(dx2, "xPixelPitch")
    yStep2 = OP_FMUL(dy2, "yPixelPitch")

    # XXX implement backface/colinear culling if area2 <= 0

    normFactor = area2.recip()

    normX0 = normFactor * x0
    normY0 = normFactor * y0
    normIv0 = normFactor * iv0
    x0int = normX0.ftoi()
    y0int = normY0.ftoi()
    iv0int = normIv0.ftoi()

    # Apply top-left pixel hit rule
    topLeft0_a = OP_IGTR(xStep0, 0)
    topLeft0_b = OP_EQ(xStep0, 0)
    topLeft0_c = OP_IGTR(yStep0, 0)
    topLeft0_d = OP_AND(topLeft0_b, topLeft0_c)
    topLeft0 = OP_OR(topLeft0_a, topLeft0_d)
    iv0int = OP_SELECT(topLeft0, OP_IADD(iv0int, 1), iv0int)

    normX1 = normFactor * x1
    normY1 = normFactor * y1
    normIv1 = normFactor * iv1
    x1int = normX1.ftoi()
    y1int = normY1.ftoi()
    iv1int = normIv1.ftoi()

    # Apply top-left pixel hit rule
    topLeft1_a = OP_IGTR(xStep1, 0)
    topLeft1_b = OP_EQ(xStep1, 0)
    topLeft1_c = OP_IGTR(yStep1, 0)
    topLeft1_d = OP_AND(topLeft1_b, topLeft1_c)
    topLeft1 = OP_OR(topLeft1_a, topLeft1_d)
    iv1int = OP_SELECT(topLeft1, OP_IADD(iv1int, 1), iv1int)

    normX2 = normFactor * x2
    normY2 = normFactor * y2
    normIv2 = normFactor * iv2
    x2int = normX2.ftoi()
    y2int = normY2.ftoi()
    iv2int = normIv2.ftoi()

    # Apply top-left pixel hit rule
    topLeft2_a = OP_IGTR(xStep2, 0)
    topLeft2_b = OP_EQ(xStep2, 0)
    topLeft2_c = OP_IGTR(yStep2, 0)
    topLeft2_d = OP_AND(topLeft2_b, topLeft2_c)
    topLeft2 = OP_OR(topLeft2_a, topLeft2_d)
    iv2int = OP_SELECT(topLeft2, OP_IADD(iv2int, 1), iv2int)

    program.print()

    print("---------------------------------------------------------------------")
    latency, path = program.critical_path()
    print("Critical path, latency:", latency, "cycles")
    for node, node_latency in path:
        print(f"{node}  # total latency: {node_latency}")

    print("\nOperation type counts")
    for op_name, count in program.get_op_type_counts().items():
        print(f"{op_name}: {count}")



