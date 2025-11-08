
    # Add signals
    add wave -noupdate -divider "Clock and Reset"
    add wave -format Logic /tb_fft_comprehensive/clk
    add wave -format Logic /tb_fft_comprehensive/reset
    
    add wave -noupdate -divider "Input Interface"
    add wave -format Logic /tb_fft_comprehensive/din_valid
    add wave -format Logic /tb_fft_comprehensive/din_last
    add wave -format Analog-Step -height 60 -max 20000 -min -20000 /tb_fft_comprehensive/din_re
    add wave -format Analog-Step -height 60 -max 20000 -min -20000 /tb_fft_comprehensive/din_im
    
    add wave -noupdate -divider "Output Interface"
    add wave -format Logic /tb_fft_comprehensive/dout_valid
    add wave -format Logic /tb_fft_comprehensive/dout_last
    add wave -format Analog-Step -height 60 -max 20000 -min -20000 /tb_fft_comprehensive/dout_re
    add wave -format Analog-Step -height 60 -max 20000 -min -20000 /tb_fft_comprehensive/dout_im
    
    add wave -noupdate -divider "Status and Control"
    add wave -format Logic /tb_fft_comprehensive/ready
    add wave -format Logic /tb_fft_comprehensive/overflow
    
    add wave -noupdate -divider "FFT Internal State"
    add wave -format Literal /tb_fft_comprehensive/dut/state
    add wave -format Literal -radix unsigned /tb_fft_comprehensive/dut/stage_counter
    add wave -format Literal -radix unsigned /tb_fft_comprehensive/dut/butterfly_counter
    add wave -format Literal -radix unsigned /tb_fft_comprehensive/dut/write_addr
    add wave -format Literal -radix unsigned /tb_fft_comprehensive/dut/read_addr
    add wave -format Logic /tb_fft_comprehensive/dut/current_buffer
    
    add wave -noupdate -divider "Butterfly Signals"
    add wave -format Analog-Step -height 50 -max 20000 -min -20000 /tb_fft_comprehensive/dut/bf_x0_re
    add wave -format Analog-Step -height 50 -max 20000 -min -20000 /tb_fft_comprehensive/dut/bf_x0_im
    add wave -format Analog-Step -height 50 -max 20000 -min -20000 /tb_fft_comprehensive/dut/bf_x1_re
    add wave -format Analog-Step -height 50 -max 20000 -min -20000 /tb_fft_comprehensive/dut/bf_x1_im
    add wave -format Analog-Step -height 50 -max 20000 -min -20000 /tb_fft_comprehensive/dut/bf_y0_re
    add wave -format Analog-Step -height 50 -max 20000 -min -20000 /tb_fft_comprehensive/dut/bf_y0_im
    add wave -format Analog-Step -height 50 -max 20000 -min -20000 /tb_fft_comprehensive/dut/bf_y1_re
    add wave -format Analog-Step -height 50 -max 20000 -min -20000 /tb_fft_comprehensive/dut/bf_y1_im
    
    add wave -noupdate -divider "Twiddle Factors"
    add wave -format Literal /tb_fft_comprehensive/dut/bf_tw_re
    add wave -format Literal /tb_fft_comprehensive/dut/bf_tw_im
    
    add wave -noupdate -divider "Test Control"
    add wave -format Logic /tb_fft_comprehensive/test_done
    add wave -format Literal -radix unsigned /tb_fft_comprehensive/output_count
    
    add wave -noupdate -divider "Performance Metrics"
    add wave -format Literal /tb_fft_comprehensive/max_magnitude
    
    # Configure wave window properties
    configure wave -namecolwidth 300
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
    run 100us
    
    puts "FFT simulation complete."
    puts "Output saved to: fft_output_re.txt, fft_output_im.txt, fft_output_magnitude.txt"
    
    if {$sim_choice != 4} {
        zoom full
        wave zoomfull
    }
}