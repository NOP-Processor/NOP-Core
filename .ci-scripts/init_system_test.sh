#! /usr/bin/env bash

set -ex

mkdir -p ${SYSTEM_RESULT}

cp ./original_build/mycpu_top.v /linux_run/nop_cpu/mycpu_top.v

# generate bitstream
SKIP_TIMING_CHECK=1 ${VIVADO} -mode batch -source ${SCRIPT_DIR}/synth_impl.tcl /linux_run/fpga/loongson_mycpu/system_run/system_run.xpr

# copy bitstream
cp /linux_run/fpga/loongson_mycpu/system_run/system_run.runs/impl_1/*.bit ${SYSTEM_RESULT}