# Based on LLCL-MIPS project(https://github.com/huang-jl/LLCL-MIPS)
update_compile_order -fileset sources_1

# Check jobs number
if {[info exists env(JOBS_NUMBER)]} {
    set jobs_number $env(JOBS_NUMBER)
} else {
    error "JOBS_NUMBER is not set"
}

puts "JOBS NUMBER is $jobs_number"

reset_run impl_1
reset_run synth_1
launch_runs -jobs $jobs_number impl_1
wait_on_run impl_1

if {[info exists env(SKIP_TIMING_CHECK)]} {
    # do nothing
} else {
    set wns_number [get_property STATS.WNS [get_runs impl_1]]
    set whs_number [get_property STATS.WHS [get_runs impl_1]]
    set pll_freq [get_property CONFIG.CLKOUT1_REQUESTED_OUT_FREQ [get_ips clk_pll]]
    if [expr {$wns_number < 0 || $whs_number < 0}] {
        puts "PLL FREQ=$pll_freq, WNS=$wns_number, WHS=$whs_number"
        error "ERROR: Timing failed"
    }
    puts "PLL FREQ=$pll_freq, WNS=$wns_number, WHS=$whs_number"
}

if {[info exists env(SKIP_BITSTREAM_GEN)]} {
    # do nothing
} else {
    launch_runs -jobs $jobs_number impl_1 -to_step write_bitstream
    wait_on_run impl_1
}