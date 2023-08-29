# WARNING: hard coded paths!!!

# add cpu code
add_files -scan_for_includes nscscc-team-la32r/perf_test/soc_axi_perf/rtl/myCPU

# add ip
add_files -quiet [glob -nocomplain nscscc-team-la32r/perf_test/soc_axi_perf/rtl/xilinx_ip_extra/*/*.xci]