This project is an experimental open hardware GPU written in Chisel HDL,
targeting a modern, mobile-class, ASIC-optimized design. Because
commercial GPU implementations are largely undocumented, much of the work is
exploratory. The initial goal is a working end-to-end reference pipeline,
likely suboptimal, that provides a foundation for deep performance
characterization. Major features:

* Tile-based, sort-middle architecture
* Unified shader
* Unified memory shared with host CPU


## Setup

This uses the Chisel Hardware Description Language and Scala-CLI, which can
be downloaded from here:

https://www.chisel-lang.org/docs/installation

## Building and Running

**To run all automated tests:**

    ./run test

**To run a specific test:**

    ./run test "my test name"

**To run a test and dumping waveform files:**

    ./run test-wave "my test name"

The output waveform will be written to build/chiselsim/.../workdir-verilator/trace.vcd

**Generating API documentation**

    ./run doc

**Running Assembler**

    python3 tools/assemble.py sw/test.s

Generates a .hex and .list file

**Test Tools**

    python3 -m unittest discover tools
