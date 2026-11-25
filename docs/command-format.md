# Command Format

Drawing commands for the GPU are written to shared memory by the host and read by the
CommandProcessor [[../hardware/main/CommandProcessor.scala]]. Packets are variable
length and always start with an opcode byte. Packets are 8-byte aligned. Values
are little endian.

**In Progress**

 | opcode| payload                                                                              |
 |-------|--------------------------------------------------------------------------------------|
 |   0   | No-operation, do nothing, load the next packet                                       |
 |   2   | FLUSH_TILE. Bytes 1-3: destination stride in bytes. Bytes 4-7: destination address   |
 |   3   | LOAD_SHADER_STATE. Bytes 1-3: number of varyings. Bytes 4-7: fragment shader address (low 32 bits; high 32 bits are set to zero) |

The host places packets in a command buffer in memory and tells the command
processor about them using two scratchpad registers, both byte addresses:
Scratchpad0 is the address of the next packet to process (advanced by the
command processor) and Scratchpad1 is the end of valid data (advanced by the
host). The command processor busy waits while they are equal.
