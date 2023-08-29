#! /usr/bin/env bash

set -ex

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

if [ -z "${FUNC_ROOT}" ]; then
    echo "FUNC_ROOT is not set"
    exit 1
fi

if [ -z "${FUNC_RESULT}" ]; then
    echo "FUNC_RESULT is not set"
    exit 1
fi

IP_ROOT=${FUNC_ROOT}/soc_verify/soc_axi/rtl/xilinx_ip
SRC_ROOT=${FUNC_ROOT}/myCPU
RUN_VIVADO_ROOT=${FUNC_ROOT}/soc_verify/soc_axi/run_vivado

mkdir ${SRC_ROOT}

cp ${CPU_DIR}/*.v ${SRC_ROOT}
for ip in `ls ${IP_DIR}/*.xci`; do
    ip_name=`basename $ip`
    ip_name=${ip_name%.*}
    ip_dir=${IP_ROOT}/${ip_name}
    mkdir ${ip_dir}
    cp ${ip} ${ip_dir}
done

pushd ${RUN_VIVADO_ROOT}

${VIVADO} -mode batch -source create_project.tcl

popd

# copy project
cp -r ${FUNC_ROOT} ${FUNC_RESULT}

# generate bitstream
${VIVADO} -mode batch -source ${SCRIPT_DIR}/synth_impl.tcl ${RUN_VIVADO_ROOT}/project/loongson.xpr

# copy bitstream
cp ${RUN_VIVADO_ROOT}/project/loongson.runs/impl_1/*.bit ${FUNC_RESULT}
