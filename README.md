
## Setup

This uses the Chisel Hardware Description Language

https://www.chisel-lang.org/docs/installation

## Building and Running

**To run all automated tests:**

    scala-cli test hardware

**To run a specific test:**

    scala-cli test hardware -- -z "<name, as passed to test()>"

**To enable dumping waveform files:**

    scala-cli test hardware -- -DemitVcd=1 -DfirtoolOpts="-g"

The output waveform will be written to build/chiselsim/.../workdir-verilator/trace.vcd

**To run the full design in simulation**

    scala-cli run hardware -- --sim

This will write the rendered framebuffer into "output.png"

**To generate synthesizable code**

    scala-cli run hardware -- --syn

The resulting RTL will be in hardware/gen/synthesis
