###############################################################################
# Add clocks and resets
add wave -noupdate -divider "Clock & Reset"
add wave -noupdate -format Logic /tb_fir_filter_comprehensive/clk
add wave -noupdate -format Logic /tb_fir_filter_comprehensive/reset

# Add input signals
add wave -noupdate -divider "Input Signals"
add wave -noupdate -format Logic /tb_fir_filter_comprehensive/din_valid
add wave -noupdate -format Literal -radix decimal /tb_fir_filter_comprehensive/din

# Add output signals
add wave -noupdate -divider "Output Signals"
add wave -noupdate -format Logic /tb_fir_filter_comprehensive/dout_valid
add wave -noupdate -format Literal -radix decimal /tb_fir_filter_comprehensive/dout

# Add internal signals from DUT
add wave -noupdate -divider "Internal Signals"
add wave -noupdate -format Literal -radix unsigned /tb_fir_filter_comprehensive/dut/x_reg(0)
add wave -noupdate -format Literal -radix unsigned /tb_fir_filter_comprehensive/dut/x_reg(1)
add wave -noupdate -format Literal -radix decimal /tb_fir_filter_comprehensive/dut/mac_pipe(0)
add wave -noupdate -format Literal -radix decimal /tb_fir_filter_comprehensive/dut/mac_pipe(1)
add wave -noupdate -format Literal -radix decimal /tb_fir_filter_comprehensive/dut/mac_pipe(2)
add wave -noupdate -format Literal -radix decimal /tb_fir_filter_comprehensive/dut/mac_pipe(3)

# Add coefficient reload signals (if enabled)
add wave -noupdate -divider "Coefficient Reload"
add wave -noupdate -format Logic /tb_fir_filter_comprehensive/coeff_wr_en
add wave -noupdate -format Literal -radix unsigned /tb_fir_filter_comprehensive/coeff_addr
add wave -noupdate -format Literal -radix decimal /tb_fir_filter_comprehensive/coeff_data

# Add virtual signals for analysis
add wave -noupdate -divider "Analysis"
# Input as analog
add wave -noupdate -format Analog-step -min -32768 -max 32767 -height 100 /tb_fir_filter_comprehensive/din
# Output as analog
add wave -noupdate -format Analog-step -min -32768 -max 32767 -height 100 /tb_fir_filter_comprehensive/dout

# Configure wave window properties
configure wave -namecolwidth 250
configure wave -valuecolwidth 100
configure wave -justifyvalue left
configure wave -signalnamewidth 1
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits ns

# Run simulation
echo "Running simulation..."
echo "This will take several seconds..."

# Run for full test duration (adjust as needed)
run 150 us

# Check if simulation completed
if {[examine -value test_done] == "true"} {
    echo "✓ Simulation completed successfully!"
} else {
    echo "⚠ Simulation may not have completed. Extending runtime..."
    run 50 us
}

# Zoom to fit
wave zoom full

# Save waveform configuration
echo "Saving waveform configuration..."
wave save "fir_filter.wave"

# Generate coverage report (if coverage enabled)
if {[info exists env(ENABLE_COVERAGE)]} {
    coverage report -detail -file coverage_report.txt
    coverage report -html -output coverage_html
    echo "Coverage reports generated"
}

# Print summary information
echo ""
echo "================================================="
echo "          SIMULATION SUMMARY"
echo "================================================="
echo "Check transcript for test results"
echo "Output data saved to: fir_output.txt"
echo "Waveform saved to: fir_filter.wave"
echo ""
echo "To view results:"
echo "  - Check waveform window for signal behavior"
echo "  - Review fir_output.txt for numerical results"
echo "  - Run Python verification script for comparison"
echo ""
echo "Commands:"
echo "  - 'run <time>'  : Continue simulation"
echo "  - 'restart -f'  : Restart simulation"
echo "  - 'quit'        : Exit simulator"
echo "================================================="


