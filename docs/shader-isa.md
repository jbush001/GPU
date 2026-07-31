
# Functional Description

This is a description of the shader processing engine (SPE), a SIMD processor
responsible for programmable rendering operations. This design uses a unified
processor that handles multiple functions (e.g. vertex and pixel shading).

The number of SIMD lanes is configurable, but we are defaulting to 8.

## Terms/definitions

- **Job** Request from an external block to do some bounded chunk of work,
  which will be scheduled on the next available hardware thread and will
  run to completion. Consists of multiple independent `work items` that run
  in lockstep. Other architectures use the term 'wavefront' or 'warp'.
- **Work item** State associated with SIMD lane of a `job`. One
  independent logical instance of the program.
- **Kernel** A chunk of machine code that is scheduled as a `job`. Sometimes
  referred to as a shader program. The kernel is referenced by the address
  of its first instruction.
- **Quad** In many places, the smallest atom of work is a 2x2 grid of pixels.
  (four `work item`s), because this arrangement allows derivative computations.
  Jobs often consist of multiple independent quads to fully utilize the
  SIMD width of the processor.

## Operational Description

External fixed function hardware queues new jobs for the SPE, and the SPE
can subsequently dispatch work to other fixed function units.

Each `job` has some extrinsic state associated with it, which is passed in
from requesting functional units, not necessarily used by the programs, but
carried through to downstream fixed function units, for example:

 - Quad X/Y (for pixel shaders)
 - Interplated depth

There's also intrinsic state passed with requests, like the offset in local
parameter memory.

### Instruction format

Instructions use fixed width, 32-bit instructions, encoded as follows.

```
              31                                                            0
             +-------+-------------+-------------+-------------+-------------+
R: Arith     |       |   rs2 (7)   |    rs1 (7)  |    rd (7)   |  opcode (7) |
             +-------+-------------+-+-----------+-------------+-------------+
B: Branch    |       offset[18:7]    |  rs1 (6)  | offset[6:0] |  opcode (7) |
             +-----------------------+-----------+-------------+-------------+
X: Inherent  |                    unused (25)                  |  opcode (7) |
             +-------------------------------+---+-------------+-------------+
K: Constant  |            value (16)         |   |    rd (7)   |  opcode (7) |
             +-------------------------------+---+-------------+-------------+
```

`opcode`

The 'V' bit below indicates a vector instruction (format V), otherwise it is
scalar (format R).


| Code    | Mnemonic                 | Description                 | Format |
|---------|--------------------------|-----------------------------|--------|
| 0000000 | and rd, rs0, rs1         | Bitwise and                 |   R    |
| 0000001 | or rd, rs0, rs1          | Bitwise or                  |   R    |
| 0000010 | xor rd, rs0, rs1         | Bitwise xor                 |   R    |
| 0000011 | addi rd, rs0, rs1        | Integer add                 |   R    |
| 0000100 | subi rd, rs0, rs1        | Integer subtract            |   R    |
| 0000101 | muli rd, rs0, rs1        | Integer multiply            |   R    |
| 0000110 | lsl rd, rs0, rs1         | Logical shift left          |   R    |
| 0000111 | asr rd, rs0, rs1         | Arithmetic shift right      |   R    |
| 0001000 | lsr rd, rs0, rs1         | Logical shift right         |   R    |
| 0001001 | addf rd, rs0, rs1        | Floating point addition     |   R    |
| 0001010 | subf rd, rs0, rs1        | FP subtraction              |   R    |
| 0001011 | mulf rd, rs0, rs1        | FP multiplication           |   R    |
| 0001100 | recip rd, rs             | FP reciprocal estimate      |   R    |
| 0001101 | ftoi rd, rs              | Float to integer            |   R    |
| 0001110 | itof rd, rs              | Integer to float            |   R    |
| 0001111 | setgtf rd, rs0, rs1      | FP Compare greater          |   R    |
| 0010000 | setltf rd, rs0, rs1      | FP Compare less than        |   R    |
| 0010001 | setgei rd, rs0, rs1      | Greater or equal, signed int|   R    |
| 0010010 | setlti rd, rs0, rs1      | Less than, signed int       |   R    |
| 0010011 | setgeu rd, rs0, rs1      | Greater or equal, unsigned  |   R    |
| 0010100 | setltu rd, rs0, rs1      | Less than, unsigned         |   R    |
| 0010101 | seteq rd, rs0, rs1       | Equal                       |   R    |
| 0010110 | setne rd, rs0, rs1       | Not equal                   |   R    |
| 1000000 | bnz rs, target           | Branch if not zero          |   B    |
| 1000001 | bz rs, target            | Branch if zero              |   B    |
| 1000010 | j target                 | Unconditional jump          |   B    |
| 1000011 | loadlo rd, index         | Load constant low           |   K    |
| 1000100 | loadhi rd, index         | Load constant high          |   K    |
| 1111111 | halt                     | Finish execution of kernel  |   X    |

* `branch offset` relative signed offset to jump based on stack operation.
  When the branch is taken, this is multiplied by four and added to the
  address of the branch instruction plus four.
* `rd` Destination register. This is either a vector or scalar register,
  depending on the instruction.
* `rs1`, `rs2` Source registers. For scalar instructions, this is a 6 bit
  register index that indicates one of 64 registers. For vector operations,
  these are 7 bit values, where the most significant bit indicates if the
  operand is a vector registers (1) or scalar (0). It is legal to mix vector
  and scalar operands for a vector instruction. Scalar operands will be
  duplicated to all lanes.

Pseudo operations are instruction forms supported by the assembler, but actually
encoded as other instruction types.

| Pseudo op              | Encoding                      |
|------------------------|-------------------------------|
| nop                    | and r0, r0, r0 (0x00000000)   |
| setgti rd, rs0, rs1    | setlti rd, rs1, rs0           |
| setlei rd, rs0, rs1    | setgei rd, rs1, rs0           |
| move rd, rs            | and rd, rs, rs                |
| clr rd                 | xor rd, rd, rd                |
| loadf rd, value        | loadhi/loadlo                 |
| loadi rd, value        | loadhi/loadlo                 |

A setxx instruction compares two values and sets the destination register with
one bit per vector lane. Because vector instructions can take scalar operands,
these instructions can also do scalar compares.

**Registers**
Registers 0-63 are scalar registers, registers 64-127 are vector registers.
Registers 96-103 represent values passed in by the fixed function unit.
For a pixel shader, for example:

| Index | Meaning                                           |
|-------|---------------------------------------------------|
|   96  | Barycentric coordinate L1 (pixel shader only)     |
|   97  | Barycentric coordinate L2 (pixel shader only)     |
|   98  | Interpolated Z (pixel shader only)                |

Attempting to read a write-only register will not fault, but the result is
undefined. Attemping to write a read-only register will have no effect.

| Index | Meaning                                           | Access |
|-------|---------------------------------------------------|--------|
|  0-31 | Scalar general purpose registers                  |  r/w   |
|   32  | Exec mask                                         |  r/w   |
|   33  | LPM read address                                  |   w    |
|   34  | LPM write address                                 |   w    |
|   35  | Uniform read address                              |   w    |
|   36  | Uniform read value                                |   r    |
|   53  | Constant 0                                        |   r    |
|   54  | Constant 1                                        |   r    |
|   55  | Constant -1                                       |   r    |
|   56  | Constant 2                                        |   r    |
|   57  | Constant 4                                        |   r    |
|   58  | Constant 0.5f                                     |   r    |
|   59  | Constant -0.5f                                    |   r    |
|   60  | Constant 1.0f                                     |   r    |
|   61  | Constant -1.0f                                    |   r    |
|   62  | Constant 2.0f                                     |   r    |
|   63  | Constant -2.0f                                    |   r    |
| 64-95 | Vector general purpose registers                  |  r/w   |
| 96-103| Job parameters (from requestor)                   |   r    |
|  104  | High word of multiplication result                |   r    |
|  105  | Store pixel, red (10 bit)                         |   w    |
|  106  | Store pixel, green                                |   w    |
|  107  | Store pixel, blue                                 |   w    |
|  108  | Store pixel, alpha                                |   w    |
|  109  | LPM read value                                    |   r    |
|  110  | LPM write value                                   |   w    |
|  111  | Lane ID                                           |   r    |
|  112  | Texture S                                         |   w    |
|  113  | Texture T                                         |   w    |
|  114  | Texture R                                         |   r    |
|  115  | Texture G                                         |   r    |
|  116  | Texture B                                         |   r    |
|  117  | Texture A                                         |   r    |

Local parameter memory (LPM) access: whenever the registers LPM read/write
value are accessed, the current LPM address is incremented. If the register
LPM address is written it will reset this value. Note that the offset into
LPM for the current job is added automatically, so this is relative.

Some external fixed function units read/write directly into LPM (for example,
vertex fetch).

**Predicated execution**

All work-items in a job are executed in lock step. If the code has conditional
checks, we use a single 'exec' mask to determine which ones execute vs. which
ones are idle. For example:

```
    if (a > b) {
        a = a - b
    } else {
        b = b - a
    }
```

In this case, the processor will generally run both forks of the branch, first
setting the exec flag based on the comparison a>b, then inverting it for the
else clause to ensure each work item. Here is how that code might assemble:

```
    ; v0 = a, v1 = b
    setgt exec, v0, v1   ; Sets exec mask for all lanes that pass a > b
    subi v0, v0, v1      ; a = a - b
    xor exec, exec, -1   ; else, invert mask
    subi v1, v1, v0      ; b = b - a
    move exec, -1        ; reset execution mask
```
