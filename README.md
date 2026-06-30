
## Setup

This uses the Spinal Hardware Description Language
<https://spinalhdl.github.io/SpinalDoc-RTD/master/index.html>,
which compiles into Verilog. Simulation is done using Verilator.

Install ScalaCLI: <https://scala-cli.virtuslab.org/install/>\
Install Verilator: <https://verilator.org/guide/latest/install.html>
Install Icarus Verilog: <https://steveicarus.github.io/iverilog/>

## Building and Running

**To run all automated tests:**

    scala-cli test hardware

**To run a specific test:**

    scala-cli test hardware -- -z "<name, as passed to test()>"

**To enable dumping waveform files:**

    scala-cli test hardware --java-prop trace=true

When the simulation begins running, it will print the following line:

    [Progress] Simulation workspace in <some path>

The output waveform will be written to <some path>/test/wave.fst

**To run the full design in simulation (currently this doesn't do anything)**

    scala-cli run hardware -- --sim

**To generate synthesizable code**

    scala-cli run hardware -- --syn

The resulting RTL will be in hardware/gen/synthesis
