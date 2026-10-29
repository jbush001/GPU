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

import chisel3._
import chisel3.util._

/**
 * Fixed-point reciprocal module.
 * Computes 1 / input.
 * Pipelined with 5 cycles of latency. Accepts a new operation every clock cycle.
 * XXX this is a stub implementation using a single-stage division.
 * It will be replaced with a pipelined implementation with estimate/
 * Newton-Raphson refinement for better latency and area.
 */
class FixedReciprocal(implicit cfg: GpuConfig) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(cfg.edgeFunctionBits.W))
    val result = Output(UInt(48.W))
  })

  io.result := ShiftRegister(0xffffff000000L.U / io.in, 5)
}

