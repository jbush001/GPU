//
//   Copyright 2026 Jeff Bush
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.
//

package gpu

import spinal.core._
import spinal.core.sim._

object Main {
  def main(args: Array[String]): Unit = {

    val hasSyn = args.contains("--syn")
    val hasSim = args.contains("--sim")

    // Check if the user passed "sim" as an argument
    if (hasSim && !hasSyn) {
      println("Running Simulation")
      SimConfig
        .withFstWave
        .workspacePath("hardware/gen/simulation")
        .compile(new Rasterizer())
        .doSim { dut =>
          dut.clockDomain.forkStimulus(period = 10)
          dut.clockDomain.waitSampling(100)
          println("Simulation complete.")
        }
    } else if (hasSyn && !hasSim) {
      println("Generating Synthesizable RTL")

      val config = SpinalConfig(targetDirectory = "hardware/gen/synthesis")
      config.generateVerilog(new Rasterizer())
      println("Verilog generation complete.")
    } else {
      println("Error, must specify either --syn or --sim");
      sys.exit(1);
    }
  }
}
