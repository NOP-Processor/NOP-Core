# # upgrade IP
# upgrade_ip -vlnv xilinx.com:ip:clk_wiz:6.0 [get_ips  clk_pll] -log ip_upgrade.log
# export_ip_user_files -of_objects [get_ips clk_pll] -no_script -sync -force -quiet
# update_compile_order -fileset sources_1

# Set PLL frequency for cpu_clk
if {([info exists env(PLL_FREQ)])} {
    set pll_freq $env(PLL_FREQ)
} else {
    error "PLL_FREQ is not set"
}
puts "frequency set to $pll_freq"
set_property -dict [list CONFIG.CLKOUT1_REQUESTED_OUT_FREQ $pll_freq] [get_ips clk_pll]
reset_target all [get_ips clk_pll]

update_compile_order -fileset sources_1
generate_target all [get_ips clk_pll]
create_ip_run [get_ips clk_pll]
launch_runs -jobs 8 clk_pll_synth_1
wait_on_run clk_pll_synth_1