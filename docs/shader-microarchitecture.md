# Shader Microarchitecture


```
                                                  ┌──────────────────────────────┐
                                                  ↓                              │
┌─────────────┐   ┌──────────────────┐   ┌──────────────────┐   ┌─────────────┐  │
│             │   │                  │   │                  │   │             │  │
│ FetchSelect │──►│ InstructionFetch │──►│InstructionDecode │──►│   Execute   │──┘
│             │   │                  │   │                  │   │             │
└─────────────┘   └──────────────────┘   └──────────────────┘   └─────────────┘
                            ↑
                            ↓
                  ┌──────────────────┐
                  │                  │
                  │    ICacheLoad    │
                  │                  │
                  └──────────────────┘
```

The Shader Processing Engine (SPE) is a 4-stage RISC SIMD vector
execution unit supporting up to 8 concurrent execution threads
(warps/wavefronts).

Rather than utilizing global pipeline stalls or complex forwarding networks
to resolve data hazards or cache misses, the SPE employs a thread rollback/
restart mechanism. If a thread encounters a long-latency operation,
cache miss, or branch, its instruction is dropped, and the thread is rolled
back to be re-issued once the dependency resolves.

Stages

* **FetchSelect**: Each cycle, selects a thread from among any of the 8 that
  are ready to execute and issues to the InstructionFetch stage. To avoid any
  pipeline hazards, it will not issue more than once every 5 cycles.
* **InstructionFetch**: Contains the direct mapped instruction cache. Has two
  sub stages: a tag check and the data read.
* **ICacheLoad**: If there is an instruction cache miss, this stage fetches the
  data and writes into the instruction cache. It connects to the memory arbiter.
* **InstructionDecode**: Has logic to determine instruction types. It also
  contains the vector and scalar register files, as well as the exec mask.
* **Execute**: Has 3 cycles of latency. It performs floating point and integer
  operations and writes back to the register files in the InstructionDecode stage.
