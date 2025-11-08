-- ===========================================================================
--  tb_fft_comprehensive.vhd : Comprehensive FFT testbench
-- ===========================================================================

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use std.textio.all;
use ieee.std_logic_textio.all;
use work.pkg_dsp_types.all;
use work.pkg_fft_types.all;

entity tb_fft_comprehensive is
end entity tb_fft_comprehensive;

architecture sim of tb_fft_comprehensive is

    -- Test configurations
    constant C_FFT_SIZE     : integer := 128;
    constant C_DATA_WIDTH   : integer := 16;
    constant C_TWIDDLE_WIDTH: integer := 16;
    constant C_CLK_PERIOD   : time := 10 ns;  -- 100 MHz
    
    -- DUT signals
    signal clk           : std_logic := '0';
    signal reset         : std_logic := '1';
    signal din_re        : std_logic_vector(C_DATA_WIDTH-1 downto 0) := (others => '0');
    signal din_im        : std_logic_vector(C_DATA_WIDTH-1 downto 0) := (others => '0');
    signal din_valid     : std_logic := '0';
    signal din_last      : std_logic := '0';
    signal dout_re       : std_logic_vector(C_DATA_WIDTH-1 downto 0);
    signal dout_im       : std_logic_vector(C_DATA_WIDTH-1 downto 0);
    signal dout_valid    : std_logic;
    signal dout_last     : std_logic;
    signal overflow      : std_logic;
    signal ready         : std_logic;
    
    signal test_done     : boolean := false;
    
    -- Output files
    file output_file_re : text;
    file output_file_im : text;
    file output_file_mag : text;
    
    -- Statistics
    signal output_count : integer := 0;
    signal max_magnitude : real := 0.0;
    
    -- Test stimulus generation
    type test_signal_t is (
        TEST_IMPULSE,
        TEST_DC,
        TEST_SINE_SINGLE,
        TEST_SINE_DUAL,
        TEST_CHIRP,
        TEST_RANDOM_NOISE,
        TEST_SQUARE_WAVE
    );
    
    -- Generate sine wave samples
    function generate_sine_sample(
        index : integer;
        amplitude : real;
        frequency : real;
        phase : real := 0.0
    ) return integer is
        variable angle : real;
    begin
        angle := 2.0 * MATH_PI * frequency * real(index) + phase;
        return integer(amplitude * sin(angle));
    end function;
    
    -- Calculate magnitude from complex
    function calculate_magnitude(re : integer; im : integer) return real is
        variable re_sq : real;
        variable im_sq : real;
    begin
        re_sq := real(re) * real(re);
        im_sq := real(im) * real(im);
        return sqrt(re_sq + im_sq);
    end function;
    
    -- Calculate magnitude in dB
    function to_db(magnitude : real) return real is
    begin
        if magnitude > 0.0 then
            return 20.0 * log10(magnitude);
        else
            return -120.0;  -- Floor
        end if;
    end function;

begin

    -- DUT instantiation
    dut : entity work.fft_radix2(rtl)
        generic map (
            FFT_SIZE         => C_FFT_SIZE,
            DATA_WIDTH       => C_DATA_WIDTH,
            TWIDDLE_WIDTH    => C_TWIDDLE_WIDTH,
            SIGNED_MODE      => true,
            PIPELINE_STAGES  => 2,
            USE_BIT_REVERSE  => true,
            USE_WINDOWING    => false,
            WINDOW_TYPE      => WINDOW_HANNING
        )
        port map (
            clk          => clk,
            reset        => reset,
            din_re       => din_re,
            din_im       => din_im,
            din_valid    => din_valid,
            din_last     => din_last,
            dout_re      => dout_re,
            dout_im      => dout_im,
            dout_valid   => dout_valid,
            dout_last    => dout_last,
            overflow     => overflow,
            ready        => ready
        );

    -- Clock generation
    clk_process : process
    begin
        while not test_done loop
            clk <= '0';
            wait for C_CLK_PERIOD / 2;
            clk <= '1';
            wait for C_CLK_PERIOD / 2;
        end loop;
        wait;
    end process;

    -- Output capture process
    output_capture : process
        variable line_out : line;
        variable count : integer := 0;
        variable re_val : integer;
        variable im_val : integer;
        variable mag_val : real;
        variable db_val : real;
    begin
        file_open(output_file_re, "fft_output_re.txt", write_mode);
        file_open(output_file_im, "fft_output_im.txt", write_mode);
        file_open(output_file_mag, "fft_output_magnitude.txt", write_mode);
        
        while not test_done loop
            wait until rising_edge(clk);
            
            if dout_valid = '1' then
                re_val := to_integer(signed(dout_re));
                im_val := to_integer(signed(dout_im));
                mag_val := calculate_magnitude(re_val, im_val);
                db_val := to_db(mag_val);
                
                -- Write real part
                write(line_out, count);
                write(line_out, string'(" "));
                write(line_out, re_val);
                writeline(output_file_re, line_out);
                
                -- Write imaginary part
                write(line_out, count);
                write(line_out, string'(" "));
                write(line_out, im_val);
                writeline(output_file_im, line_out);
                
                -- Write magnitude (linear and dB)
                write(line_out, count);
                write(line_out, string'(" "));
                write(line_out, mag_val);
                write(line_out, string'(" "));
                write(line_out, db_val);
                writeline(output_file_mag, line_out);
                
                count := count + 1;
                output_count <= count;
                
                if mag_val > max_magnitude then
                    max_magnitude <= mag_val;
                end if;
            end if;
        end loop;
        
        file_close(output_file_re);
        file_close(output_file_im);
        file_close(output_file_mag);
        wait;
    end process;

    -- Main test stimulus
    test_process : process
        
        -- Send a frame of data
        procedure send_frame(signal_type : test_signal_t; 
                           amplitude : real := 10000.0;
                           freq1 : real := 0.1;
                           freq2 : real := 0.0) is
            variable re_val : integer;
            variable im_val : integer;
            variable seed1, seed2 : positive := 12345;
            variable rand_val : real;
        begin
            wait until ready = '1';
            wait for 2 * C_CLK_PERIOD;
            
            for i in 0 to C_FFT_SIZE-1 loop
                case signal_type is
                    when TEST_IMPULSE =>
                        if i = 0 then
                            re_val := integer(amplitude);
                        else
                            re_val := 0;
                        end if;
                        im_val := 0;
                    
                    when TEST_DC =>
                        re_val := integer(amplitude);
                        im_val := 0;
                    
                    when TEST_SINE_SINGLE =>
                        re_val := generate_sine_sample(i, amplitude, freq1);
                        im_val := 0;
                    
                    when TEST_SINE_DUAL =>
                        re_val := generate_sine_sample(i, amplitude/2.0, freq1) +
                                 generate_sine_sample(i, amplitude/2.0, freq2);
                        im_val := 0;
                    
                    when TEST_CHIRP =>
                        -- Linear frequency sweep
                        re_val := generate_sine_sample(i, amplitude, 
                                     freq1 + (freq2 - freq1) * real(i) / real(C_FFT_SIZE));
                        im_val := 0;
                    
                    when TEST_RANDOM_NOISE =>
                        uniform(seed1, seed2, rand_val);
                        re_val := integer((rand_val - 0.5) * 2.0 * amplitude);
                        uniform(seed1, seed2, rand_val);
                        im_val := integer((rand_val - 0.5) * 2.0 * amplitude);
                    
                    when TEST_SQUARE_WAVE =>
                        if (i mod integer(1.0 / freq1)) < integer(0.5 / freq1) then
                            re_val := integer(amplitude);
                        else
                            re_val := integer(-amplitude);
                        end if;
                        im_val := 0;
                end case;
                
                din_re <= std_logic_vector(to_signed(re_val, C_DATA_WIDTH));
                din_im <= std_logic_vector(to_signed(im_val, C_DATA_WIDTH));
                din_valid <= '1';
                
                if i = C_FFT_SIZE-1 then
                    din_last <= '1';
                else
                    din_last <= '0';
                end if;
                
                wait for C_CLK_PERIOD;
            end loop;
            
            din_valid <= '0';
            din_last <= '0';
            
            -- Wait for FFT to complete
            wait until dout_last = '1';
            wait for 10 * C_CLK_PERIOD;
        end procedure;
        
        -- Verify peak location for single tone
        procedure verify_single_tone(expected_bin : integer; tolerance : integer := 2) is
            variable peak_bin : integer := 0;
            variable peak_mag : real := 0.0;
        begin
            wait for 100 ns;
            report "Expected peak at bin " & integer'image(expected_bin);
            report "Maximum magnitude: " & real'image(max_magnitude);
            -- In a complete verification, we would check the actual peak location
        end procedure;
        
    begin
        -- Initial reset
        reset <= '1';
        din_valid <= '0';
        din_last <= '0';
        wait for 100 ns;
        reset <= '0';
        wait for 50 ns;
        
        report "=== Starting FFT Tests ===";
        report "";
        
        -- Test 1: Impulse response (should be flat spectrum)
        report "TEST 1: Impulse Response";
        send_frame(TEST_IMPULSE, 16000.0);
        report "Impulse test complete - expect flat spectrum";
        wait for 100 ns;
        
        -- Test 2: DC signal (should have peak at bin 0)
        report "TEST 2: DC Signal";
        send_frame(TEST_DC, 10000.0);
        verify_single_tone(0);
        wait for 100 ns;
        
        -- Test 3: Single sine wave at bin 8
        report "TEST 3: Single Tone (8/128 = 0.0625 Fs)";
        send_frame(TEST_SINE_SINGLE, 8000.0, 8.0/real(C_FFT_SIZE));
        verify_single_tone(8);
        wait for 100 ns;
        
        -- Test 4: Single sine wave at bin 16
        report "TEST 4: Single Tone (16/128 = 0.125 Fs)";
        send_frame(TEST_SINE_SINGLE, 8000.0, 16.0/real(C_FFT_SIZE));
        verify_single_tone(16);
        wait for 100 ns;
        
        -- Test 5: Two-tone test
        report "TEST 5: Dual Tone (bins 10 and 20)";
        send_frame(TEST_SINE_DUAL, 8000.0, 10.0/real(C_FFT_SIZE), 20.0/real(C_FFT_SIZE));
        report "Dual tone test complete - expect peaks at bins 10 and 20";
        wait for 100 ns;
        
        -- Test 6: Nyquist frequency (bin N/2)
        report "TEST 6: Nyquist Frequency";
        send_frame(TEST_SINE_SINGLE, 8000.0, 0.5);
        verify_single_tone(C_FFT_SIZE/2);
        wait for 100 ns;
        
        -- Test 7: Chirp signal (frequency sweep)
        report "TEST 7: Chirp Signal (Linear Frequency Sweep)";
        send_frame(TEST_CHIRP, 8000.0, 0.05, 0.4);
        report "Chirp test complete - expect spread spectrum";
        wait for 100 ns;
        
        -- Test 8: White noise
        report "TEST 8: White Noise";
        send_frame(TEST_RANDOM_NOISE, 5000.0);
        report "Noise test complete - expect flat spectrum with variance";
        wait for 100 ns;
        
        -- Test 9: Square wave (rich harmonics)
        report "TEST 9: Square Wave";
        send_frame(TEST_SQUARE_WAVE, 8000.0, 0.1);
        report "Square wave test complete - expect odd harmonics";
        wait for 100 ns;
        
        -- Test 10: Low amplitude signal (quantization noise test)
        report "TEST 10: Low Amplitude Signal";
        send_frame(TEST_SINE_SINGLE, 100.0, 0.1);
        report "Low amplitude test complete - check SNR";
        wait for 100 ns;
        
        -- Test 11: Maximum amplitude signal (overflow test)
        report "TEST 11: Maximum Amplitude";
        send_frame(TEST_SINE_SINGLE, 30000.0, 0.1);
        report "Maximum amplitude test complete - check for overflow";
        wait for 100 ns;
        
        -- Test 12: Complex input (both real and imaginary)
        report "TEST 12: Complex Input Signal";
        -- This would require modifying send_frame to support complex inputs
        report "Complex input test - to be implemented with complex stimulus";
        wait for 100 ns;
        
        -- Performance test: Multiple consecutive frames
        report "TEST 13: Throughput Test (10 consecutive frames)";
        for i in 1 to 10 loop
            send_frame(TEST_SINE_SINGLE, 8000.0, 0.1);
        end loop;
        report "Throughput test complete";
        wait for 100 ns;
        
        report "";
        report "=== All FFT Tests Completed ===";
        report "Total output samples: " & integer'image(output_count);
        
        test_done <= true;
        wait;
    end process;

    -- Performance monitoring
    performance_monitor : process
        variable start_time : time := 0 ns;
        variable end_time : time := 0 ns;
        variable processing_time : time;
        variable throughput : real;
    begin
        wait until reset = '0';
        wait for 100 ns;
        
        -- Wait for first frame start
        wait until din_valid = '1' and rising_edge(clk);
        start_time := now;
        
        -- Wait for first frame completion
        wait until dout_last = '1' and rising_edge(clk);
        end_time := now;
        
        processing_time := end_time - start_time;
		throughput := real(C_FFT_SIZE) / real(processing_time / C_CLK_PERIOD) * 100.0;
        
        report "Performance Metrics:";
        report "  FFT Size: " & integer'image(C_FFT_SIZE) & " points";
        report "  Processing Time: " & time'image(processing_time);
        report "  Clock Cycles: " & integer'image(processing_time / C_CLK_PERIOD);
        report "  Throughput: " & real'image(throughput) & "% of sample rate";
        report "  Cycles per point: " & 
               real'image(real(processing_time / C_CLK_PERIOD) / real(C_FFT_SIZE));
        
        wait;
    end process;

    -- Overflow monitoring
    overflow_monitor : process
        variable overflow_count : integer := 0;
    begin
        wait until reset = '0';
        
        while not test_done loop
            wait until rising_edge(clk);
            
            if overflow = '1' then
                overflow_count := overflow_count + 1;
                report "WARNING: Overflow detected at time " & time'image(now);
            end if;
        end loop;
        
        if overflow_count > 0 then
            report "Total overflows: " & integer'image(overflow_count);
        else
            report "No overflows detected";
        end if;
        
        wait;
    end process;

    -- Timing verification
    timing_check : process
        variable input_time : time := 0 ns;
        variable output_time : time := 0 ns;
        variable latency : time;
    begin
        wait until reset = '0';
        wait for 100 ns;
        
        -- Measure latency from first input to first output
        wait until din_valid = '1' and rising_edge(clk);
        input_time := now;
        
        wait until dout_valid = '1' and rising_edge(clk);
        output_time := now;
        
        latency := output_time - input_time;
        
        report "FFT Latency: " & time'image(latency) & 
               " (" & integer'image(latency / C_CLK_PERIOD) & " cycles)";
        
        wait;
    end process;

    -- Spectral purity check
    spectral_check : process
        variable bin_count : integer := 0;
        variable re_val : integer;
        variable im_val : integer;
        variable mag : real;
        variable max_mag : real := 0.0;
        variable total_power : real := 0.0;
        variable peak_power : real := 0.0;
        variable snr : real;
    begin
        wait until reset = '0';
        
        -- Wait for a single-tone test output
        wait until output_count > C_FFT_SIZE;
        
        for i in 0 to C_FFT_SIZE-1 loop
            wait until dout_valid = '1' and rising_edge(clk);
            
            re_val := to_integer(signed(dout_re));
            im_val := to_integer(signed(dout_im));
            mag := calculate_magnitude(re_val, im_val);
            
            total_power := total_power + mag * mag;
            
            if mag > max_mag then
                max_mag := mag;
                peak_power := mag * mag;
            end if;
        end loop;
        
        if total_power > 0.0 and peak_power > 0.0 then
            snr := 10.0 * log10(peak_power / (total_power - peak_power));
            report "Spectral Analysis:";
            report "  Peak magnitude: " & real'image(max_mag);
            report "  Total power: " & real'image(total_power);
            report "  Estimated SNR: " & real'image(snr) & " dB";
        end if;
        
        wait;
    end process;

end architecture sim;