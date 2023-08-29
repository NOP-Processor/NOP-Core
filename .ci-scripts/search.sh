#! /usr/bin/env bash

set -x

# build args
# export PLL_FREQ=100
# export JOBS_NUMBER=8

# executables
# VIVADO=/opt/Xilinx/Vivado/2019.2/bin/vivado

# sources
# CPU_DIR=/home/scc23nop1/lambda/team-latest/cpu-src
# IP_DIR=/home/scc23nop1/lambda/team-latest/cpu-src
# SCRIPT_DIR=/home/scc23nop1/lambda/team-latest/.ci-scripts

# destination
# TEAM_ROOT=~/lambda/team-latest

# assert environment variables
if [ -z "${VIVADO}" ]; then
    echo "VIVADO is not set"
    exit 1
fi

if [ -z "${CPU_DIR}" ]; then
    echo "CPU_DIR is not set"
    exit 1
fi

if [ -z "${IP_DIR}" ]; then
    echo "IP_DIR is not set"
    exit 1
fi

if [ -z "${SCRIPT_DIR}" ]; then
    echo "SCRIPT_DIR is not set"
    exit 1
fi

if [ -z "${PERF_ROOT}" ]; then
    echo "PERF_ROOT is not set"
    exit 1
fi

IP_ROOT=${PERF_ROOT}/soc_axi_perf/rtl/xilinx_ip_extra
SRC_ROOT=${PERF_ROOT}/soc_axi_perf/rtl/myCPU
PROJ_ROOT=${PERF_ROOT}/soc_axi_perf/run_vivado/mycpu_prj1

mkdir ${SRC_ROOT}
mkdir ${IP_ROOT}

cp ${CPU_DIR}/*.v ${SRC_ROOT}
for ip in `ls ${IP_DIR}/*.xci`; do
    ip_name=`basename $ip`
    ip_name=${ip_name%.*}
    ip_dir=${IP_ROOT}/${ip_name}
    mkdir ${ip_dir}
    cp ${ip} ${ip_dir}
done

# add source to project
${VIVADO} -mode batch -source ${SCRIPT_DIR}/perf_add_source.tcl ${PROJ_ROOT}/mycpu.xpr

# copy project
cp -r ${PERF_ROOT} ${PERF_RESULT}

# binary search for a valid pll frequency
min=${SEARCH_LOWER}
max=${SEARCH_UPPER}
latest_freq=0.0

SEARCH_REPORT=${PERF_RESULT}/search_report.txt

echo "Initial Range [${min}, ${max}]" | tee -a ${SEARCH_REPORT}

for i in `seq ${SEARCH_ITERATION}` 
do
    echo "Iteration ${i}" | tee -a ${SEARCH_REPORT}
    # calculate mid
    mid=`perl -e "print ((${min}+${max})/2)"`
    datestr=`date +%Y%m%d%H%M%S`
    run_root=${PERF_RESULT}/search/${datestr}_${mid}
    mkdir -p ${run_root}

    echo "[${datestr}] mid: ${mid}" 

    # config pll frequency
    PLL_FREQ=${mid} ${VIVADO} -mode batch -source ${SCRIPT_DIR}/pll.tcl ${PROJ_ROOT}/mycpu.xpr
    if [ $? -ne 0 ]; then
        echo "Failed to config pll frequency"
        exit 1
    fi

    SKIP_BITSTREAM_GEN=1 ${VIVADO} -mode batch -source ${SCRIPT_DIR}/synth_impl.tcl ${PROJ_ROOT}/mycpu.xpr
    if [ $? -eq 0 ]; then
        min=${mid}
        latest_freq=${mid}
        echo "- Frequecy ${mid} OK" | tee -a ${SEARCH_REPORT}
    else
        echo "- Frequecy ${mid} Failed" | tee -a ${SEARCH_REPORT}
        max=${mid}
    fi

    # save synth & impl log
    cp ${PROJ_ROOT}/mycpu.runs/synth_1/runme.log ${run_root}/synth_runme.log
    cp ${PROJ_ROOT}/mycpu.runs/impl_1/runme.log ${run_root}/impl_runme.log

    # set -e
    echo "Updated Range [${min}, ${max}]" | tee -a ${SEARCH_REPORT}
done

if [ ${latest_freq} -eq 0.0 ]; then
    echo "No valid frequency found"
    exit 1
fi

set -e

export PLL_FREQ=${latest_freq}

# config pll frequency
${VIVADO} -mode batch -source ${SCRIPT_DIR}/pll.tcl ${PROJ_ROOT}/mycpu.xpr

# generate bitstream
${VIVADO} -mode batch -source ${SCRIPT_DIR}/synth_impl.tcl ${PROJ_ROOT}/mycpu.xpr

# copy bitstream
cp ${PROJ_ROOT}/mycpu.runs/impl_1/*.bit ${PERF_RESULT}

# copy clock
cp ${PERF_ROOT}/soc_axi_perf/rtl/xilinx_ip/clk_pll/clk_pll.xci ${PERF_RESULT}/perf_clk_pll.xci
