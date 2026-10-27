
# Functional Description

This is a description of the shader processing engine (SPE), a SIMD processor
that performs programmable rendering operations. This design uses a unified
processor that handles multiple functions (e.g. vertex and pixel shading).

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

### Instruction format

Instructions use fixed width, 32-bit instructions, encoded as follows.

```
              31             23 22           15 14            7 6           0
             +-+---------------+---------------+---------------+-------------+
R: Arith     | |     rs2 (8)   |     rs1 (8)   |    rd (8)     |  opcode (7) |
             +-+---------------+-+-------------+---------------+-------------+
B: Branch    |    offset[17:8]   |    rs1 (7)  |   offset[7:0] |  opcode (7) |
             +-------------------+-------------+---------------+-------------+
X: Inherent  |                    unused (25)                  |  opcode (7) |
             +-------------------------------+-+---------------+-------------+
K: Constant  |            value (16)         | |    rd (8)     |  opcode (7) |
             +-------------------------------+-+---------------+-------------+
```

`opcode`


| Opcode  | Mnemonic                 | Description                 | Format |
|---------|--------------------------|-----------------------------|--------|
|    0    | halt                     | Finish execution of kernel  |   X    |
|    1    | and rd, rs0, rs1         | Bitwise and                 |   R    |
|    2    | or rd, rs0, rs1          | Bitwise or                  |   R    |
|    3    | xor rd, rs0, rs1         | Bitwise xor                 |   R    |
|    4    | addi rd, rs0, rs1        | Integer add                 |   R    |
|    5    | subi rd, rs0, rs1        | Integer subtract            |   R    |
|    6    | muli rd, rs0, rs1        | Integer multiply            |   R    |
|    7    | mulih rd, rs0, rs1       | Integer multiply (high word)|   R    |
|    8    | mulihu                   | Unsigned multiply upper     |   R    |
|    9    | shl rd, rs0, rs1         | Shift left                  |   R    |
|   10    | shr rd, rs0, rs1         | Signed shift right          |   R    |
|   11    | shru rd, rs0, rs1        | Unsigned shift right        |   R    |
|   12    | addf rd, rs0, rs1        | Floating point addition     |   R    |
|   13    | subf rd, rs0, rs1        | FP subtraction              |   R    |
|   14    | mulf rd, rs0, rs1        | FP multiplication           |   R    |
|   15    | fmin                     | Floating point minimum val  |   R    |
|   16    | fmax                     | Floating point max value    |   R    |
|   17    | fabs                     | Floating point absolute val |   R    |
|   18    | recip rd, rs             | FP reciprocal estimate      |   R    |
|   19    | ftoi rd, rs              | Float to integer            |   R    |
|   20    | itof rd, rs              | Integer to float            |   R    |
|   32    | setgtf rd, rs0, rs1      | FP Compare greater          |   R    |
|   33    | setgei rd, rs0, rs1      | Greater or equal, signed int|   R    |
|   34    | setlti rd, rs0, rs1      | Less than, signed int       |   R    |
|   35    | setgeu rd, rs0, rs1      | Greater or equal, unsigned  |   R    |
|   36    | setltu rd, rs0, rs1      | Less than, unsigned         |   R    |
|   37    | seteq rd, rs0, rs1       | Equal                       |   R    |
|   38    | setne rd, rs0, rs1       | Not equal                   |   R    |
|   48    | bnz rs, target           | Branch if not zero          |   B    |
|   49    | bz rs, target            | Branch if zero              |   B    |
|   50    | j target                 | Unconditional jump          |   B    |
|   56    | loadlo rd, index         | Load constant low           |   K    |
|   57    | loadhi rd, index         | Load constant high          |   K    |

*These probably need a renumbering*

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
| nop                    | and r0, r0, r0 (0x00000001)   |
| setgti rd, rs0, rs1    | setlti rd, rs1, rs0           |
| setlei rd, rs0, rs1    | setgei rd, rs1, rs0           |
| setltf rd, rs0, rs1    | setgtf rd, rs1, rs0           |
| move rd, rs            | or rd, rs, zero               |
| clr rd                 | xor rd, rd, rd                |
| loadf rd, value        | loadhi/loadlo                 |
| loadi rd, value        | loadhi/loadlo                 |

A setxx instruction compares two values and sets the destination register with
one bit per vector lane. Because vector instructions can take scalar operands,
these instructions can also do scalar compares.

**Registers**
Registers 0-63 are scalar registers, registers 64-127 are vector registers.

Attempting to read a write-only register will not fault, but the result is
undefined. Attemping to write a read-only register will have no effect.

|  Index  | Meaning                                           | Access |
|---------|---------------------------------------------------|--------|
|  0-63   | Scalar general purpose registers<sup>1</sup>      |  r/w   |
|   116   | Constant 0                                        |   r    |
|   117   | Constant 1                                        |   r    |
|   118   | Constant -1                                       |   r    |
|   119   | Constant 2                                        |   r    |
|   120   | Constant 4                                        |   r    |
|   121   | Constant 0.5f                                     |   r    |
|   122   | Constant -0.5f                                    |   r    |
|   123   | Constant 1.0f                                     |   r    |
|   124   | Constant -1.0f                                    |   r    |
|   125   | Constant 2.0f                                     |   r    |
|   126   | Constant -2.0f                                    |   r    |
|   127   | Exec mask                                         |  r/w   |
| 128-191 | Vector general purpose registers                  |  r/w   |
|   223   | Lane ID                                           |   r    |
|   224   | Lambda0 (barycentric coordinate)                  |   r    |
|   225   | Lambda1 (barycentric coordinate)                  |   r    |
|   226   | Read varying coefficient<sup>2</sup>              |   r    |
|   227   | Texture fetch red <sup>3</sup>                    |   r    |
|   228   | Texture fetch green                               |   r    |
|   229   | Texture fetch blue                                |   r    |
|   230   | Texture fetch alpha                               |   r    |
|   240   | Output red<sup>4</sup>                            |   w    |
|   241   | Output green                                      |   w    |
|   242   | Output blue                                       |   w    |
|   243   | Output alpha                                      |   w    |
|   244   | Texture fetch S coordinate<sup>4</sup>            |   w    |
|   245   | Texture fetch T coordinate<sup>5</sup>            |   w    |

*Note that register 192-207 can only be used in the first register operand*

1. Note that although there are 64 register indices reserved, there may
   not be that many physical registers configured.
2. This will read varying coefficients in order and will increment to
   the next varying automatically as a side effect.
3. These will block the current thread to wait for the texture fetch
   to finish if it is not available yet.
4. These are floating point values 0.0-1.0.
5. Writing to the T coordinate will initiate the texture fetch as a side
   effect, so this register should be written last.

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
setting the `exec mask` (Register 32) based on the comparison a>b, then
inverting it for the else clause to ensure each work item. Here is how that
code might assemble:

```
    ; v0 = a, v1 = b
    setgt exec, v0, v1   ; Sets exec mask for all lanes that pass a > b
    subi v0, v0, v1      ; a = a - b
    xor exec, exec, -1   ; else, invert mask
    subi v1, v1, v0      ; b = b - a
    move exec, -1        ; reset execution mask
```
