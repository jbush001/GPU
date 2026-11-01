# Command Format

Drawing commands for the GPU are written to shared memory by the host and read by the
CommandProcessor [[../hardware/main/CommandProcessor.scala]]. Packets are variable
length, but always start with an opcode byte:

 | opcode| payload                                        |
 |-------|------------------------------------------------|
 |   0   | No-operation, do nothing, load the next packet |
 |   1   | HALT. End execution of command stream          |
