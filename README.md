
## Setup

This uses the Chisel Hardware Description Language

https://www.chisel-lang.org/docs/installation

## Building and Running

**To run all automated tests:**

    ./run test

**To run a specific test:**

    ./run test "my test name"

**To run a test and dumping waveform files:**

    ./run test-wave "my test name"

The output waveform will be written to build/chiselsim/.../workdir-verilator/trace.vcd

**To run the full design in simulation**

    ./run sim

This will write the rendered framebuffer into "output.png"

**To generate synthesizable design**

    ./run syn

(currently not implemented)

**Generating API documentation**

    ./run doc
