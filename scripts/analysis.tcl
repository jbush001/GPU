read_liberty /OpenROAD-flow-scripts/flow/platforms/nangate45/lib/NangateOpenCellLibrary_typical.lib
read_verilog build/Gpu_synth.v
link_design Gpu
read_sdc scripts/constraint.sdc
report_checks -path_delay max
report_power
