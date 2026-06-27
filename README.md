
## Setup

Install ScalaCLI: <https://scala-cli.virtuslab.org/install/> \\
Install Verilator: <https://verilator.org/guide/latest/install.html>

## Building

To run tests:

    scala-cli test hardware

To run the full design in simulation

    scala-cli run hardware -- --sim

The output waveform dump is in hardware/gen/simulation/Counter/test/wave.fst

To generate synthesizable code

    scala-cli run hardware -- --syn

The resulting RTL will be in hardware/gen/synthesis
