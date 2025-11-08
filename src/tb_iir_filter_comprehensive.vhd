-- ===========================================================================
--  tb_iir_filter_comprehensive.vhd : Complete IIR filter testbench
-- ===========================================================================

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use std.textio.all;
use ieee.std_logic_textio.all;
use work.pkg_dsp_types.all;
use work.pkg_iir_types.all;

entity tb_iir_filter_comprehensive is
end entity tb_iir_filter_comprehensive;

architecture sim of tb_iir_filter_comprehensive is

    constant C_ORDER       : integer := 2;
    constant C_DATA_WIDTH  : integer := 16;
    constant C_COEFF_WIDTH : integer := 16;
    constant C_INT_WIDTH   : integer := 32;
    constant C_CLK_PERIOD  : time := 10 ns;  -- 100 MHz
    constant C_NUM_BIQUADS : integer := 1;
    
    signal clk           : std_logic := '0';
    signal reset         : std_logic := '1';
    signal din           : std_logic_vector(C_DATA_WIDTH-1 downto 0) := (others => '0');
    signal din_valid     : std_logic := '0';
    signal dout          : std_logic_vector(C_DATA_WIDTH-1 downto 0);
    signal dout_valid    : std_logic;
    signal overflow      : std_logic;
    signal underflow     : std_logic;
    
    signal coeff_wr_en   : std_logic := '0';
    signal coeff_section : std_logic_vector(3 downto 0) := (others => '0');
    signal coeff_addr    : std_logic_vector(2 downto 0) := (others => '0');
    signal coeff_data    : std_logic_vector(C_COEFF_WIDTH-1 downto 0) := (others => '0');
    
    signal test_done     : boolean := false;
    
    -- Output file
    file output_file : text;
    
    -- Statistics
    signal overflow_count : integer := 0;
    signal sample_count   : integer := 0;

    -- Generate sine LUT
    type sine_lut_t is array (0 to 255) of integer;
    function generate_sine_lut(amplitude : integer; frequency : real) return sine_lut_t is
        variable lut : sine_lut_t;
        variable phase : real;
    begin
        for i in 0 to 255 loop
            phase := 2.0 * MATH_PI * frequency * real(i) / 256.0;
            lut(i) := integer(real(amplitude) * sin(phase));
        end loop;
        return lut;
    end function;
    
    constant SINE_LUT_LOW  : sine_lut_t := generate_sine_lut(8000, 1.0);   -- Low freq
    constant SINE_LUT_MID  : sine_lut_t := generate_sine_lut(8000, 8.0);   -- Mid freq
    constant SINE_LUT_HIGH : sine_lut_t := generate_sine_lut(8000, 32.0);  -- High freq

begin

    -- DUT: Direct Form II (more efficient)
    dut_df2 : entity work.iir_filter_generic(rtl)
        generic map (
            ORDER          => C_ORDER,
            DATA_WIDTH     => C_DATA_WIDTH,
            COEFF_WIDTH    => C_COEFF_WIDTH,
            INTERNAL_WIDTH => C_INT_WIDTH,
            STRUCTURE      => DIRECT_FORM_II,
            SIGNED_MODE    => true,
            COEFF_ROM      => true,
            NUM_BIQUADS    => C_NUM_BIQUADS
        )
        port map (
            clk            => clk,
            reset          => reset,
            din            => din,
            din_valid      => din_valid,
            dout           => dout,
            dout_valid     => dout_valid,
            coeff_wr_en    => coeff_wr_en,
            coeff_section  => coeff_section,
            coeff_addr     => coeff_addr,
            coeff_data     => coeff_data,
            overflow       => overflow,
            underflow      => underflow
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

    -- Output capture
    output_capture : process
        variable line_out : line;
        variable count : integer := 0;
    begin
        file_open(output_file, "iir_output.txt", write_mode);
        
        while not test_done loop
            wait until rising_edge(clk);
            
            if dout_valid = '1' then
                write(line_out, count);
                write(line_out, string'(" "));
                write(line_out, to_integer(signed(dout)));
                writeline(output_file, line_out);
                count := count + 1;
            end if;
        end loop;
        
        file_close(output_file);
        wait;
    end process;

    -- Overflow monitoring
    overflow_monitor : process(clk)
    begin
        if rising_edge(clk) then
            if overflow = '1' or underflow = '1' then
                overflow_count <= overflow_count + 1;
            end if;
            if dout_valid = '1' then
                sample_count <= sample_count + 1;
            end if;
        end if;
    end process;

    -- Main test stimulus
    test_process : process
    
        -- Load coefficients for a specific filter type
        procedure load_coefficients(coeffs : biquad_coeffs_t; section : integer := 0) is
        begin
            report "Loading coefficients for section " & integer'image(section);
            
            coeff_section <= std_logic_vector(to_unsigned(section, 4));
            
            -- b0
            coeff_addr <= "000";
            coeff_data <= std_logic_vector(coeffs.b0);
            coeff_wr_en <= '1';
            wait for C_CLK_PERIOD;
            
            -- b1
            coeff_addr <= "001";
            coeff_data <= std_logic_vector(coeffs.b1);
            wait for C_CLK_PERIOD;
            
            -- b2
            coeff_addr <= "010";
            coeff_data <= std_logic_vector(coeffs.b2);
            wait for C_CLK_PERIOD;
            
            -- a1
            coeff_addr <= "011";
            coeff_data <= std_logic_vector(coeffs.a1);
            wait for C_CLK_PERIOD;
            
            -- a2
            coeff_addr <= "100";
            coeff_data <= std_logic_vector(coeffs.a2);
            wait for C_CLK_PERIOD;
            
            coeff_wr_en <= '0';
            wait for 5 * C_CLK_PERIOD;
        end procedure;
        
        -- Apply step input
        procedure apply_step(amplitude : integer; num_samples : integer) is
        begin
            report "TEST: Step Response (amplitude=" & integer'image(amplitude) & ")";
            din_valid <= '1';
            din <= std_logic_vector(to_signed(amplitude, C_DATA_WIDTH));
            
            for i in 0 to num_samples-1 loop
                wait for C_CLK_PERIOD;
            end loop;
            
            din <= (others => '0');
            for i in 0 to 63 loop
                wait for C_CLK_PERIOD;
            end loop;
            
            din_valid <= '0';
            wait for 10 * C_CLK_PERIOD;
        end procedure;
        
        -- Apply impulse
        procedure apply_impulse is
        begin
            report "TEST: Impulse Response";
            din_valid <= '1';
            din <= std_logic_vector(to_signed(16384, C_DATA_WIDTH));
            wait for C_CLK_PERIOD;
            
            din <= (others => '0');
            for i in 0 to 127 loop
                wait for C_CLK_PERIOD;
            end loop;
            
            din_valid <= '0';
            wait for 10 * C_CLK_PERIOD;
        end procedure;
        
        -- Apply sine wave
		procedure apply_sine(constant lut : in sine_lut_t; num_cycles : integer) is
		begin
			din_valid <= '1';
			for cycle in 0 to num_cycles-1 loop
				for i in 0 to 255 loop
					din <= std_logic_vector(to_signed(lut(i), C_DATA_WIDTH));
					wait for C_CLK_PERIOD;
				end loop;
			end loop;
			din_valid <= '0';
			wait for 10 * C_CLK_PERIOD;
		end procedure;

        
        -- Apply random noise
        procedure apply_random(num_samples : integer) is
            variable seed1, seed2 : positive := 42;
            variable rand_val : real;
            variable int_val : integer;
        begin
            report "TEST: Random Noise";
            din_valid <= '1';
            
            for i in 0 to num_samples-1 loop
                uniform(seed1, seed2, rand_val);
                int_val := integer((rand_val - 0.5) * 16000.0);
                din <= std_logic_vector(to_signed(int_val, C_DATA_WIDTH));
                wait for C_CLK_PERIOD;
            end loop;
            
            din_valid <= '0';
            wait for 10 * C_CLK_PERIOD;
        end procedure;
        
        -- Stability test with maximum input
        procedure stability_test is
            constant MAX_VAL : integer := 2**(C_DATA_WIDTH-1) - 1;
        begin
            report "TEST: Stability Test (Maximum Input)";
            din_valid <= '1';
            
            -- Alternate between max positive and negative
            for i in 0 to 511 loop
                if (i mod 2) = 0 then
                    din <= std_logic_vector(to_signed(MAX_VAL, C_DATA_WIDTH));
                else
                    din <= std_logic_vector(to_signed(-MAX_VAL, C_DATA_WIDTH));
                end if;
                wait for C_CLK_PERIOD;
            end loop;
            
            din <= (others => '0');
            for i in 0 to 127 loop
                wait for C_CLK_PERIOD;
            end loop;
            
            din_valid <= '0';
            wait for 10 * C_CLK_PERIOD;
        end procedure;
        
        -- Sweep frequency test
        procedure frequency_sweep is
            variable phase : real := 0.0;
            variable phase_inc : real := 0.0;
            variable amplitude : real := 8000.0;
            variable sample_val : integer;
        begin
            report "TEST: Frequency Sweep";
            din_valid <= '1';
            
            -- Sweep from low to high frequency
            for i in 0 to 2047 loop
                phase_inc := real(i) / 2048.0 * MATH_PI / 4.0;  -- 0 to pi/4
                sample_val := integer(amplitude * sin(phase));
                din <= std_logic_vector(to_signed(sample_val, C_DATA_WIDTH));
                phase := phase + phase_inc;
                wait for C_CLK_PERIOD;
            end loop;
            
            din_valid <= '0';
            wait for 10 * C_CLK_PERIOD;
        end procedure;
        
    begin
        -- Initial reset
        reset <= '1';
        din_valid <= '0';
        wait for 100 ns;
        reset <= '0';
        wait for 50 ns;
        
        report "=== Starting IIR Filter Tests ===";
        report "";
        
        -- Test 1: Low-pass filter tests
        report "=== Test Suite 1: LOW-PASS FILTER ===";
        load_coefficients(BIQUAD_LOWPASS_01);
        
        apply_impulse;
        wait for 100 ns;
        
        apply_step(10000, 256);
        wait for 100 ns;
        
        report "TEST: Low Frequency Sine (should pass)";
        apply_sine(SINE_LUT_LOW, 4);
        wait for 100 ns;
        
        report "TEST: High Frequency Sine (should attenuate)";
        apply_sine(SINE_LUT_HIGH, 4);
        wait for 100 ns;
        
        -- Test 2: High-pass filter tests
        report "=== Test Suite 2: HIGH-PASS FILTER ===";
        load_coefficients(BIQUAD_HIGHPASS_01);
        
        apply_impulse;
        wait for 100 ns;
        
        report "TEST: Low Frequency Sine (should attenuate)";
        apply_sine(SINE_LUT_LOW, 4);
        wait for 100 ns;
        
        report "TEST: High Frequency Sine (should pass)";
        apply_sine(SINE_LUT_HIGH, 4);
        wait for 100 ns;
        
        -- Test 3: Band-pass filter tests
        report "=== Test Suite 3: BAND-PASS FILTER ===";
        load_coefficients(BIQUAD_BANDPASS);
        
        report "TEST: Mid Frequency Sine (should pass)";
        apply_sine(SINE_LUT_MID, 4);
        wait for 100 ns;
        
        frequency_sweep;
        wait for 100 ns;
        
        -- Test 4: Notch filter
        report "=== Test Suite 4: NOTCH FILTER ===";
        load_coefficients(BIQUAD_NOTCH);
        
        apply_sine(SINE_LUT_MID, 4);
        wait for 100 ns;
        
        -- Test 5: Stability tests
        report "=== Test Suite 5: STABILITY & EDGE CASES ===";
        load_coefficients(BIQUAD_LOWPASS_01);
        
        stability_test;
        wait for 100 ns;
        
        apply_random(512);
        wait for 100 ns;
        
        -- Test 6: DC blocking (high-pass with step)
        report "=== Test Suite 6: DC BLOCKING ===";
        load_coefficients(BIQUAD_HIGHPASS_01);
        apply_step(15000, 512);
        wait for 200 ns;
        
        report "";
        report "=== All Tests Completed ===";
        report "Overflow events: " & integer'image(overflow_count);
        report "Total samples: " & integer'image(sample_count);
        if overflow_count > 0 then
            report "Overflow rate: " & 
                   real'image(real(overflow_count) / real(sample_count) * 100.0) & "%";
        end if;
        
        test_done <= true;
        wait;
    end process;

    -- Performance monitoring
    performance_monitor : process
        variable first_valid : time := 0 ns;
        variable last_valid  : time := 0 ns;
        variable sample_cnt  : integer := 0;
    begin
        wait until reset = '0';
        wait for 100 ns;
        
        -- Wait for first output
        wait until dout_valid = '1' and rising_edge(clk);
        first_valid := now;
        
        -- Count samples for 10000 cycles
        for i in 0 to 9999 loop
            wait until rising_edge(clk);
            if dout_valid = '1' then
                sample_cnt := sample_cnt + 1;
                last_valid := now;
            end if;
        end loop;
        
        report "Performance Analysis:";
        report "  Valid samples: " & integer'image(sample_cnt) & " / 10000";
        report "  Throughput: " & real'image(real(sample_cnt) / 10000.0 * 100.0) & "%";
        
        wait;
    end process;

    -- Latency measurement
    latency_check : process
        variable input_time : time := 0 ns;
        variable output_time : time := 0 ns;
        variable latency : time;
    begin
        wait until reset = '0';
        wait for 100 ns;
        
        -- Wait for valid input
        wait until din_valid = '1' and rising_edge(clk);
        input_time := now;
        
        -- Wait for first valid output after this input
        wait until dout_valid = '1' and rising_edge(clk);
        output_time := now;
        
        latency := output_time - input_time;
        
        report "Filter Latency: " & time'image(latency) & 
               " (" & integer'image(latency / C_CLK_PERIOD) & " cycles)";
        
        wait;
    end process;

end architecture sim;