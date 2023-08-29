#! /usr/bin/env bash

set -ex

source ~/.bashrc 

# build test scripts
make -C ${FUNC_ROOT}/func clean
make -C ${FUNC_ROOT}/func
make -C ${PERF_ROOT}/soft/perf_func clean
make -C ${PERF_ROOT}/soft/perf_func

# relax permissions
chmod -R 777 ${TEAM_ROOT}

# fix mis-comment in CDP_EDE_local @ introduced by c4f09a3d30917cd90a1672b6297022578512404c
sed -i 's/# add_files -scan_for_includes ..\/..\/..\/myCPU/add_files -scan_for_includes ..\/..\/..\/myCPU/g' ${FUNC_ROOT}/soc_verify/soc_axi/run_vivado/create_project.tcl
