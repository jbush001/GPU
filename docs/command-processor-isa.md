# Command Processor ISA Reference

## Overview

The Command Processor contains a small RISC pipeline running firmware loaded
from the host CPU. This program parses command streams from shared memory and
coordinates activity across the GPU pipeline units.

## Instruction Format

All instructions are 32 bits wide. The instruction format is highly regular,
containing an 18-bit signed immediate, three 3-bit register identifiers
(supporting 8 General Purpose Registers), and a 5-bit opcode.

```
 31                                 14 13 12 11  9 8   5 4       0
├─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┼─┴─┴─┼─┴─┴─┼─┴─┴─┼─┴─┴─┴─┴─┤
│              imm (18)               │ rs2 │ rs1 │  rd │opcode(5)│
└─────────────────────────────────────┴─────┴─────┴─────┴─────────┘
```

## Instruction Set (Opcodes)

| Opcode | Mnemonic              | Description                                              |
| ------ | --------------------- | -------------------------------------------------------- |
|    0   |   AND rd, rs0, rs1    | Bitwise AND                                              |
|    1   |   OR rd, rs0, rs1     | Bitwise OR                                               |
|    2   |   XOR rd, rs0, rs1    | Bitwise XOR                                              |
|    3   |   ADD rd, rs0, rs1    | Integer Addition                                         |
|    4   |   SUB rd, rs0, rs1    | Integer Subtraction                                      |
|    5   |   SLTU rd, rs0, rs1   | Set Less Than Unsigned                                   |
|    6   |   SLL rd, rs0, rs1    | Shift Left Logical                                       |
|    7   |   SRL rd, rs0, rs1    | Shift Right Logical                                      |
|    8   |   BEQ rs1, rs2, offs  | Branch if Equal: `if (rs1 == rs2) PC = PC + imm`         |
|    9   |   BNE rs1, rs2, offs  | Branch if Not Equal: `if (rs1 != rs2) PC = PC + imm`     |
|   16   |   ANDI rd, rs0, rs1   | Bitwise AND immediate                                    |
|   17   |   ORI rd, rs0, rs1    | Bitwise OR immediate                                     |
|   18   |   XORI rd, rs0, rs1   | Bitwise XOR immediate                                    |
|   19   |   ADDI rd, rs0, rs1   | Integer Addition immediate                               |
|   20   |   SUBI rd, rs0, rs1   | Integer Subtraction immediate                            |
|   21   |   SLTUI rd, rs0, rs1  | Set Less Than Unsigned immediate                         |
|   22   |   SLLI rd, rs0, rs1   | Shift Left Logical immediate                             |
|   23   |   SRLI rd, rs0, rs1   | Shift Right Logical immediate                            |
|   24   |   BEQI rs1, rs2, offs | Branch if Equal immediate                                |
|   25   |   BNEI rs1, rs2, offs | Branch if Not Equal immediate                            |
|   26   |   LW rd, imm(rs0)     | Load Word.<sup>1</sup>                                   |
|   27   |   SW rs1, imm(rs0)    | Store Word                                               |
|   28   |   MOVEHI rd, rs1, imm | Move High Immediate `rd = (imm << 14) + (rs1 & 0x3fff)`  |
|   29   |   MOVEI rd, imm       | Move Immediate: `rd = imm`                               |
|   30   |   JAL rd, offs        | Jump and Link: `rd = PC + 4; PC = PC + imm`              |
|   31   |   JALR rd, rs1, offs  | Jump and Link Register: `rd = PC + 4; PC = rs1 + imm`    |

1. Loads require a one-cycle delay slot before the loaded value can be safely used by a subsequent instruction.


Pseudo operations are instruction forms supported by the assembler, but
actually encoded as other instruction types.

| Pseudo op              | Encoding                      |
|------------------------|-------------------------------|
| nop                    | and r0, r0, r0                |
| move rd, rs            | addi rd, rs, 0                |
| clear rd               | xor rd, rd, rd                |
| b target               | beq r0, r0, target            |
| bz rs, target          | beqi r0, 0, target            |
| bnz rs, target         | bnei r0, 0, target            |

## Memory-Mapped Control Registers

The processor communicates with the host and GPU control units via special registers
that are memory-mapped into the command processor address space.

| Address  | R/W | Description                                                      |
| -------- | --- | ---------------------------------------------------------------- |
| fffffe00 |  w  | DMA Source Address (Lower 32-bits)                               |
| fffffe01 |  w  | DMA Source Address (Upper 32-bits)                               |
| fffffe02 |  w  | Length of DMA transfer (words). Limited to 16                    |
| fffffe03 |  r  | DMA Status                                                       |
| fffffe04 |  r  | DMA data                                                         |
| ffffff00 |  w  | Clear color value (ARGB)                                         |
| ffffff01 |  w  | Clear depth value (Float32)                                      |
| ffffff02 |  w  | Configuration flags for tile writing                             |
| ffffff04 |  w  | Vertex/Fragment Shader Base Address (Low)                        |
| ffffff05 |  w  | Vertex/Fragment Shader Base Address (High)                       |
| ffffff06 |  w  | Number of varying variables passed to fragment shader            |
| ffffff07 |  w  | X offset for the current tile                                    |
| ffffff08 |  w  | Y offset for the current tile                                    |
| ffffff09 |  w  | Tile Output Memory Address (Low)                                 |
| ffffff0a |  w  | Tile Output Memory Address (High)                                |
| ffffff0b |  w  | Tile output stride: bytes between the start of two adjacent rows |
| ffffff0c |  w  | Trigger register to begin writing tile                           |
| ffffff0d |  w  | Trigger register to begin clearing tile                          |

Direct memory access (DMA) allows the Command Processor to fetch command
packets and other data from host memory without stalling the CPU. To
initiate a DMA transfer, the firmware writes the 64-bit host memory
source address into the DmaAddrL and DmaAddrH registers, followed by
the transfer size in 32-bit words. Writing the DMA length starts the
transfer. Bit 0 of the DmaStatus register will be zero until the data is
available. Once they are, the processor can read them a word at a time.

It is illegal for the processor to start another transaction until it has
fully read all words that were requested.
