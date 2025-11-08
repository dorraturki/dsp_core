library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use std.textio.all;
use ieee.std_logic_textio.all;
use work.pkg_dsp_types.all;
use work.pkg_coeffs.all;

entity tb_fir_filter_comprehensive is
end entity tb_fir_filter_comprehensive;

architecture sim of tb_fir_filter_comprehensive is

    constant C_TAPS        : integer := 32;
    constant C_DATA_WIDTH  : integer := 16;
    constant C_COEFF_WIDTH : integer := 16;
    constant C_CLK_PERIOD  : time := 10 ns;  -- 100 MHz
    
    signal clk         : std_logic := '0';
    signal reset       : std_logic := '1';
    signal din         : std_logic_vector(C_DATA_WIDTH-1 downto 0) := (others => '0');
    signal din_valid   : std_logic := '0';
    signal dout        : std_logic_vector(C_DATA_WIDTH-1 downto 0);
    signal dout_valid  : std_logic;
    signal coeff_wr_en : std_logic := '0';
    signal coeff_addr  : std_logic_vector(7 downto 0) := (others => '0');
    signal coeff_data  : std_logic_vector(C_COEFF_WIDTH-1 downto 0) := (others => '0');
    
    signal test_done   : boolean := false;
    
    -- Output file handling
    file output_file : text;

    -- Test stimulus types
    type test_type is (TEST_IMPULSE, TEST_SINE, TEST_MULTITONE, TEST_RANDOM, TEST_STEP);
    
    -- LUT for sine wave (256 samples, multiple frequencies)
    type sine_lut_t is array (0 to 255) of integer;
    
    -- Generate sine LUT function
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
    
    constant SINE_LUT_LOW  : sine_lut_t := generate_sine_lut(6000, 1.0);  -- 1 cycle per 256 samples
    constant SINE_LUT_MID  : sine_lut_t := generate_sine_lut(6000, 4.0);  -- 4 cycles
    constant SINE_LUT_HIGH : sine_lut_t := generate_sine_lut(6000, 16.0); -- 16 cycles (high freq)

begin

    -- DUT instantiation
    dut : entity work.fir_filter_enhanced(rtl)
        generic map (
            TAPS             => C_TAPS,
            DATA_WIDTH       => C_DATA_WIDTH,
            COEFF_WIDTH      => C_COEFF_WIDTH,
            PIPELINE_STAGES  => 3,
            COEFF_RELOADABLE => true
        )
        port map (
            clk          => clk,
            reset        => reset,
            din          => din,
            din_valid    => din_valid,
            dout         => dout,
            dout_valid   => dout_valid,
            coeff_wr_en  => coeff_wr_en,
            coeff_addr   => coeff_addr,
            coeff_data   => coeff_data
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
        variable sample_count : integer := 0;
    begin
        file_open(output_file, "fir_output.txt", write_mode);
        
        while not test_done loop
            wait until rising_edge(clk);
            
            if dout_valid = '1' then
                write(line_out, sample_count);
                write(line_out, string'(" "));
                write(line_out, to_integer(signed(dout)));
                writeline(output_file, line_out);
                sample_count := sample_count + 1;
            end if;
        end loop;
        
        file_close(output_file);
        wait;
    end process;

    -- Main test stimulus
    test_process : process
        
        -- Procedure to apply impulse
        procedure apply_impulse is
        begin
            report "TEST 1: Impulse Response";
            
            -- Apply impulse
            din <= std_logic_vector(to_signed(16384, C_DATA_WIDTH));
            din_valid <= '1';
            wait for C_CLK_PERIOD;
            
            -- Zero samples
            din <= (others => '0');
            for i in 0 to 127 loop
                wait for C_CLK_PERIOD;
            end loop;
            din_valid <= '0';
            wait for 10 * C_CLK_PERIOD;
        end procedure;
        
        -- Procedure to apply sine wave
        procedure apply_sine(lut : in sine_lut_t; num_cycles : integer) is
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
        
        -- Procedure to apply random signal
        procedure apply_random(num_samples : integer) is
            variable seed1, seed2 : positive := 1;
            variable rand_val : real;
            variable int_val : integer;
        begin
            report "TEST 4: Random Signal";
            din_valid <= '1';
            
            for i in 0 to num_samples-1 loop
                uniform(seed1, seed2, rand_val);
                int_val := integer((rand_val - 0.5) * 20000.0);
                din <= std_logic_vector(to_signed(int_val, C_DATA_WIDTH));
                wait for C_CLK_PERIOD;
            end loop;
            
            din_valid <= '0';
            wait for 10 * C_CLK_PERIOD;
        end procedure;
        
        -- Procedure to apply step input
        procedure apply_step(amplitude : integer; num_samples : integer) is
        begin
            report "TEST 5: Step Response";
            din_valid <= '1';
            din <= std_logic_vector(to_signed(amplitude, C_DATA_WIDTH));
            
            for i in 0 to num_samples-1 loop
                wait for C_CLK_PERIOD;
            end loop;
            
            din <= (others => '0');
            for i in 0 to 127 loop
                wait for C_CLK_PERIOD;
            end loop;
            
            din_valid <= '0';
            wait for 10 * C_CLK_PERIOD;
        end procedure;
        
        -- Procedure to test coefficient reload
        procedure test_coeff_reload is
        begin
            report "TEST 6: Coefficient Reload";
            din_valid <= '0';
            wait for 5 * C_CLK_PERIOD;
            
            -- Load unity coefficients (all-pass)
            for i in 0 to C_TAPS-1 loop
                coeff_wr_en <= '1';
                coeff_addr <= std_logic_vector(to_unsigned(i, 8));
                if i = C_TAPS/2 then
                    coeff_data <= std_logic_vector(to_signed(8192, C_COEFF_WIDTH));
                else
                    coeff_data <= (others => '0');
                end if;
                wait for C_CLK_PERIOD;
            end loop;
            coeff_wr_en <= '0';
            
            wait for 5 * C_CLK_PERIOD;
            
            -- Test with impulse
            din <= std_logic_vector(to_signed(16384, C_DATA_WIDTH));
            din_valid <= '1';
            wait for C_CLK_PERIOD;
            
            din <= (others => '0');
            for i in 0 to 63 loop
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
        
        -- Run test sequence
        apply_impulse;
        wait for 100 ns;
        
        report "TEST 2: Low Frequency Sine (passband)";
        apply_sine(SINE_LUT_LOW, 4);
        wait for 100 ns;
        
        report "TEST 3: High Frequency Sine (stopband)";
        apply_sine(SINE_LUT_HIGH, 4);
        wait for 100 ns;
        
        apply_random(512);
        wait for 100 ns;
        
        apply_step(10000, 256);
        wait for 100 ns;
        
        test_coeff_reload;
        wait for 100 ns;
        
        -- Performance test: continuous stream
        report "TEST 7: Continuous Stream (Throughput Test)";
        din_valid <= '1';
        for i in 0 to 2047 loop
            din <= std_logic_vector(to_signed(SINE_LUT_MID(i mod 256), C_DATA_WIDTH));
            wait for C_CLK_PERIOD;
        end loop;
        din_valid <= '0';
        
        wait for 1 us;
        
        report "All tests completed successfully!";
        test_done <= true;
        wait;
    end process;

    -- Performance monitoring
    performance_monitor : process
        variable cycle_count : integer := 0;
        variable valid_count : integer := 0;
        variable throughput : real;
    begin
        wait until reset = '0';
        wait for 100 ns;
        
        cycle_count := 0;
        valid_count := 0;
        
        -- Monitor for 10000 cycles
        for i in 0 to 9999 loop
            wait until rising_edge(clk);
            cycle_count := cycle_count + 1;
            if dout_valid = '1' then
                valid_count := valid_count + 1;
            end if;
        end loop;
        
        throughput := real(valid_count) / real(cycle_count) * 100.0;
        report "Throughput: " & real'image(throughput) & "% (" & 
               integer'image(valid_count) & "/" & integer'image(cycle_count) & ")";
        
        wait;
    end process;

    -- Timing verification
    timing_check : process
        variable last_valid_time : time := 0 ns;
        variable latency : time;
    begin
        wait until reset = '0';
        
        -- Wait for first valid input
        wait until din_valid = '1' and rising_edge(clk);
        last_valid_time := now;
        
        -- Wait for first valid output
        wait until dout_valid = '1' and rising_edge(clk);
        latency := now - last_valid_time;
        
        report "Filter Latency: " & time'image(latency) & 
               " (" & integer'image(latency / C_CLK_PERIOD) & " cycles)";
        
        wait;
    end process;

end architecture sim;