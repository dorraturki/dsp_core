# Add signals for waveform viewing
add wave -divider {===== CLOCK & RESET =====}
add wave clk
add wave reset

add wave -divider {===== INPUT SIGNALS =====}
add wave din
add wave din_valid

add wave -divider {===== FILTER OUTPUT =====}
add wave dout
add wave dout_valid

add wave -divider {===== STATUS =====}
add wave overflow_detect
add wave underflow_detect

# Zoom out for overview, optionally zoom and run for full view
run 0 ns
run 10 us

# Optional: set up color/format for better visualization
wave color din yellow
wave color dout cyan
wave color overflow_detect red
wave color underflow_detect orange

wave format din analog
wave format dout analog

# Print useful info to transcript
echo "==== Simulation complete: View din vs dout for filter response ===="