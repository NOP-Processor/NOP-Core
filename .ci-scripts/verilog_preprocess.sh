#! /use/bin/env bash

if [ -z "${SRC_ROOT}" ]; then
    echo "SRC_ROOT is not set"
    exit 1
fi

sed -i 's/intrpt/ext_int/g' ${SRC_ROOT}/*.v
sed -i 's/debug0_wb_pc/debug_wb_pc/g' ${SRC_ROOT}/*.v
sed -i 's/debug0_wb_rf_wen/debug_wb_rf_we/g' ${SRC_ROOT}/*.v
sed -i 's/debug0_wb_rf_wnum/debug_wb_rf_wnum/g' ${SRC_ROOT}/*.v
sed -i 's/debug0_wb_rf_wdata/debug_wb_rf_wdata/g' ${SRC_ROOT}/*.v