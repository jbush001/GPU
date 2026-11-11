# Command Processor ISA Reference

## 1. Overview

The Command Processor contains a small RISC pipeline running firmware loaded
from the host CPU. This program parses command streams from shared memory and
coordinates activity across the GPU pipeline units.

## 2. Instruction Format

All instructions are 32 bits wide. The instruction format is highly regular,
containing an 18-bit signed immediate, three 3-bit register identifiers
(supporting 8 General Purpose Registers), and a 5-bit opcode.

```
 31                                 14 13 12 11  9 8   5 4       0
├─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┼─┴─┴─┼─┴─┴─┼─┴─┴─┼─┴─┴─┴─┴─┤
│              imm (18)               │ rs2 │ rs1 │  rd │ opcd (5)│
└─────────────────────────────────────┴─────┴─────┴─────┴─────────┘
```

## 3. Instruction Set (Opcodes)

| Opcode | Mnemonic              | Description                                              |
| ------ | --------------------- | -------------------------------------------------------- |
|    0   |   AND rd, rs0, rs1    | Bitwise AND |
|    1   |   OR rd, rs0, rs1     | Bitwise OR |
|    2   |   XOR rd, rs0, rs1    | Bitwise XOR |
|    3   |   ADD rd, rs0, rs1    | Integer Addition |
|    4   |   SUB rd, rs0, rs1    | Integer Subtraction |
|    5   |   SLTU rd, rs0, rs1   | Set Less Than Unsigned |
|    6   |   SLL rd, rs0, rs1    | Shift Left Logical |
|    8   |   SRL rd, rs0, rs1    | Shift Right Logical |
|    9   |   ADDI rd, rs0, imm   | Add Immediate |
|    10  |   SLTUI rd, rs0, imm  | Set Less Than Unsigned Immediate |
|    11  |   SLLI rd, rs0, imm   | Shift Left Logical Immediate |
|    12  |   SRLI rd, rs0, imm   | Shift Right Logical Immediate |
|    13  |   LW rd, imm(rs0)     | Load Word.<sup>1</sup> |
|    14  |   SW rs1, imm(rs0)    | Store Word |
|    15  |   MOVEHI rd, rs1, imm | Move High Immediate, loads the immediate into the upper bits of `rd` |
|    16  |   MOVEI rd, imm       | Move Immediate: `rd = imm` |
|    17  |   BEQ rs1, rs2, offs  | Branch if Equal: `if (rs1 == rs2) PC = PC + imm` |
|    18  |   BNE rs1, rs2, offs  | Branch if Not Equal: `if (rs1 != rs2) PC = PC + imm` |
|    19  |   JAL rd, offs        | Jump and Link: `rd = PC + 4; PC = PC + imm` |
|    20  |   JALR rd, rs1, offs  | Jump and Link Register: `rd = PC + 4; PC = rs1 + imm` |

1. Loads require a one-cycle delay slot before the loaded value can be safely used by a subsequent instruction.


## 4. Memory-Mapped I/O (MMIO)

The processor communicates with the host and GPU control units via memory-mapped special registers.

| Address  | R/W | Description |
| -------- | --- | ----------- |
| fffffe00 |  w  | DMA Source Address (Lower 32-bits) |
| fffffe01 |  w  | DMA Source Address (Upper 32-bits) |
| fffffe02 |  w  | Length of DMA transfer (words). Limited to 8 |
| fffffe03 |  r  | DMA Status |
| fffffe04 |  r  | DMA data |
| ffffff00 |  w  | Clear color value (ARGB) |
| ffffff01 |  w  | Clear depth value (Float32) |
| ffffff02 |  w  | Configuration flags for tile writing |
| ffffff04 |  w  | Vertex/Fragment Shader Base Address (Low) |
| ffffff05 |  w  | Vertex/Fragment Shader Base Address (High) |
| ffffff06 |  w  | Number of varying variables passed to fragment shader |
| ffffff07 |  w  | X offset for the current tile |
| ffffff08 |  w  | Y offset for the current tile |
| ffffff09 |  w  | Tile Output Memory Address (Low) |
| ffffff0a |  w  | Tile Output Memory Address (High) |
| ffffff0b |  w  | Tile output stride: bytes between the start of two adjacent rows |
| ffffff0c |  w  | Trigger register to begin writing tile |
| ffffff0d |  w  | Trigger register to begin clearing tile |

Direct Memory Access (DMA) allows the Command Processor to fetch command
packets and other data from host memory without stalling the CPU. To
initiate a DMA transfer, the firmware writes the 64-bit host memory
source address into the DmaAddrL and DmaAddrH registers, followed by
the transfer size in 32-bit words. Writing the DMA length starts the
transfer. Bit 0 of the DmaStatus register will be zero until the data is
available. Once they are, the processor can read them a word at a time.

It is illegal for the processor to start another transaction until it has
fully read all words that were requested.
