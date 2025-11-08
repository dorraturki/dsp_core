-- ===========================================================================
--  iir_filter_generic.vhd : Configurable IIR Filter with Direct Forms I/II
-- ===========================================================================

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.pkg_dsp_types.all;
use work.pkg_iir_types.all;

entity iir_filter_generic is
    generic (
        ORDER           : integer := 2;           -- Filter order (sections)
        DATA_WIDTH      : integer := 16;          -- I/O data width
        COEFF_WIDTH     : integer := 16;          -- Coefficient width
        INTERNAL_WIDTH  : integer := 32;          -- Internal accumulator width
        STRUCTURE       : iir_structure_t := DIRECT_FORM_II;  -- DirectFormI or II
        SIGNED_MODE     : boolean := true;        -- Signed/unsigned
        COEFF_ROM       : boolean := false;       -- Enable dynamic coeff loading
        NUM_BIQUADS     : integer := 1            -- Number of cascaded biquads
    );
    port (
        clk             : in  std_logic;
        reset           : in  std_logic;
        
        -- Data interface
        din             : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        din_valid       : in  std_logic;
        dout            : out std_logic_vector(DATA_WIDTH-1 downto 0);
        dout_valid      : out std_logic;
        
        -- Coefficient loading interface (optional)
        coeff_wr_en     : in  std_logic := '0';
        coeff_section   : in  std_logic_vector(3 downto 0) := (others => '0');
        coeff_addr      : in  std_logic_vector(2 downto 0) := (others => '0');
        coeff_data      : in  std_logic_vector(COEFF_WIDTH-1 downto 0) := (others => '0');
        
        -- Status outputs
        overflow        : out std_logic;
        underflow       : out std_logic
    );
end entity iir_filter_generic;

architecture rtl of iir_filter_generic is

    -- Coefficient storage for biquad sections
    -- Each biquad has: b0, b1, b2, a1, a2 (a0 normalized to 1)
    type coeff_section_t is array (0 to 4) of signed(COEFF_WIDTH-1 downto 0);
    type coeff_array_t is array (0 to NUM_BIQUADS-1) of coeff_section_t;
    
    -- Initialize with default coefficients (unity gain, no filtering)
    signal coeffs : coeff_array_t := (others => (
        to_signed(2**(COEFF_WIDTH-2), COEFF_WIDTH),  -- b0 = 0.25
        to_signed(0, COEFF_WIDTH),                    -- b1 = 0
        to_signed(0, COEFF_WIDTH),                    -- b2 = 0
        to_signed(0, COEFF_WIDTH),                    -- a1 = 0
        to_signed(0, COEFF_WIDTH)                     -- a2 = 0
    ));
    
    -- State storage for Direct Form I
    type state_df1_t is record
        x1, x2 : signed(INTERNAL_WIDTH-1 downto 0);  -- Input delays
        y1, y2 : signed(INTERNAL_WIDTH-1 downto 0);  -- Output delays
    end record;
    
    -- State storage for Direct Form II (more efficient)
    type state_df2_t is record
        w1, w2 : signed(INTERNAL_WIDTH-1 downto 0);  -- State variables
    end record;
    
    type state_df1_array_t is array (0 to NUM_BIQUADS-1) of state_df1_t;
    type state_df2_array_t is array (0 to NUM_BIQUADS-1) of state_df2_t;
    
    signal states_df1 : state_df1_array_t;
    signal states_df2 : state_df2_array_t;
    
    -- Pipeline registers between sections
    type inter_section_t is array (0 to NUM_BIQUADS) of signed(INTERNAL_WIDTH-1 downto 0);
    signal section_data : inter_section_t;
    
    -- Valid signal pipeline
    signal valid_pipe : std_logic_vector(0 to NUM_BIQUADS) := (others => '0');
    
    -- Overflow detection
    signal overflow_detect  : std_logic := '0';
    signal underflow_detect : std_logic := '0';
    
    -- Output register
    signal dout_reg       : signed(DATA_WIDTH-1 downto 0) := (others => '0');
    signal dout_valid_reg : std_logic := '0';

		-- Saturation with overflow detection (PROCEDURE)
		procedure saturate_detect(
			val : in signed; 
			width : in integer;
			variable result : out signed;
			variable overflow_flag : out std_logic;
			variable underflow_flag : out std_logic
		) is
			variable max_value : signed(width-1 downto 0);
			variable min_value : signed(width-1 downto 0);
		begin
			max_value := (others => '1');
			max_value(width-1) := '0';
			min_value := (others => '0');
			min_value(width-1) := '1';
			
			overflow_flag := '0';
			underflow_flag := '0';
			
			if val > max_value then
				result := max_value;
				overflow_flag := '1';
			elsif val < min_value then
				result := min_value;
				underflow_flag := '1';
			else
				result := resize(val, width);
			end if;
		end procedure;



begin

    -- Coefficient loading process
    coeff_load_proc : process(clk)
        variable section_idx : integer;
        variable coeff_idx   : integer;
    begin
        if rising_edge(clk) then
            if COEFF_ROM and coeff_wr_en = '1' then
                section_idx := to_integer(unsigned(coeff_section));
                coeff_idx   := to_integer(unsigned(coeff_addr));
                
                if section_idx < NUM_BIQUADS and coeff_idx < 5 then
                    coeffs(section_idx)(coeff_idx) <= signed(coeff_data);
                end if;
            end if;
        end if;
    end process;

    -- Direct Form I Implementation
    gen_direct_form_1 : if STRUCTURE = DIRECT_FORM_I generate
        process(clk)
            variable x_curr : signed(INTERNAL_WIDTH-1 downto 0);
            variable y_curr : signed(INTERNAL_WIDTH-1 downto 0);
            variable b_sum  : signed(INTERNAL_WIDTH-1 downto 0);
            variable a_sum  : signed(INTERNAL_WIDTH-1 downto 0);
            variable ovf, unf : std_logic;
			variable temp_result : signed(DATA_WIDTH-1 downto 0);
			variable temp_overflow : std_logic;
			variable temp_underflow : std_logic;
        begin
            if rising_edge(clk) then
                if reset = '1' then
                    -- Reset all states
                    for i in 0 to NUM_BIQUADS-1 loop
                        states_df1(i).x1 <= (others => '0');
                        states_df1(i).x2 <= (others => '0');
                        states_df1(i).y1 <= (others => '0');
                        states_df1(i).y2 <= (others => '0');
                    end loop;
                    section_data <= (others => (others => '0'));
                    valid_pipe <= (others => '0');
                    dout_reg <= (others => '0');
                    dout_valid_reg <= '0';
                    overflow_detect <= '0';
                    underflow_detect <= '0';
                    
                else
                    -- Input to first section
                    if din_valid = '1' then
                        section_data(0) <= resize(signed(din), INTERNAL_WIDTH);
                    end if;
                    valid_pipe(0) <= din_valid;
                    
                    -- Process each biquad section
                    for i in 0 to NUM_BIQUADS-1 loop
                        if valid_pipe(i) = '1' then
                            x_curr := section_data(i);
                            
                            -- Numerator (feedforward): y = b0*x[n] + b1*x[n-1] + b2*x[n-2]
                            b_sum := resize(
                                resize(coeffs(i)(0) * resize(x_curr, COEFF_WIDTH), INTERNAL_WIDTH) +
                                resize(coeffs(i)(1) * resize(states_df1(i).x1, COEFF_WIDTH), INTERNAL_WIDTH) +
                                resize(coeffs(i)(2) * resize(states_df1(i).x2, COEFF_WIDTH), INTERNAL_WIDTH),
                                INTERNAL_WIDTH
                            );
                            
                            -- Denominator (feedback): y = y - a1*y[n-1] - a2*y[n-2]
                            a_sum := resize(
                                resize(coeffs(i)(3) * resize(states_df1(i).y1, COEFF_WIDTH), INTERNAL_WIDTH) +
                                resize(coeffs(i)(4) * resize(states_df1(i).y2, COEFF_WIDTH), INTERNAL_WIDTH),
                                INTERNAL_WIDTH
                            );
                            
                            -- Scale coefficients (assume Q15 format)
                            y_curr := shift_right(b_sum - a_sum, COEFF_WIDTH - 1);
                            
                            -- Update states
                            states_df1(i).x2 <= states_df1(i).x1;
                            states_df1(i).x1 <= x_curr;
                            states_df1(i).y2 <= states_df1(i).y1;
                            states_df1(i).y1 <= y_curr;
                            
                            -- Output to next section
                            section_data(i+1) <= y_curr;
                        end if;
                        
                        valid_pipe(i+1) <= valid_pipe(i);
                    end loop;
                    
					-- Final output with saturation
					if valid_pipe(NUM_BIQUADS) = '1' then
						saturate_detect(
							section_data(NUM_BIQUADS),
							DATA_WIDTH,
							temp_result,
							temp_overflow,
							temp_underflow
						);
						
						dout_reg <= temp_result;
						overflow_detect <= temp_overflow;
						underflow_detect <= temp_underflow;
					end if;
                   
                    dout_valid_reg <= valid_pipe(NUM_BIQUADS);
                end if;
            end if;
        end process;
    end generate;

    -- Direct Form II Implementation (Transposed)
    gen_direct_form_2 : if STRUCTURE = DIRECT_FORM_II generate
        process(clk)
            variable x_in   : signed(INTERNAL_WIDTH-1 downto 0);
            variable w_curr : signed(INTERNAL_WIDTH-1 downto 0);
            variable y_out  : signed(INTERNAL_WIDTH-1 downto 0);
            variable ovf, unf : std_logic;
			variable temp_result : signed(DATA_WIDTH-1 downto 0);
			variable temp_overflow : std_logic;
			variable temp_underflow : std_logic;
        begin
            if rising_edge(clk) then
			        if valid_pipe(NUM_BIQUADS) = '1' then
						saturate_detect(section_data(NUM_BIQUADS), DATA_WIDTH, temp_result, temp_overflow, temp_underflow);
						dout_reg <= temp_result;
						overflow_detect <= temp_overflow;
						underflow_detect <= temp_underflow;
					end if;
                if reset = '1' then
                    -- Reset all states
                    for i in 0 to NUM_BIQUADS-1 loop
                        states_df2(i).w1 <= (others => '0');
                        states_df2(i).w2 <= (others => '0');
                    end loop;
                    section_data <= (others => (others => '0'));
                    valid_pipe <= (others => '0');
                    dout_reg <= (others => '0');
                    dout_valid_reg <= '0';
                    overflow_detect <= '0';
                    underflow_detect <= '0';
                    
                else
                    -- Input to first section
                    if din_valid = '1' then
                        section_data(0) <= resize(signed(din), INTERNAL_WIDTH);
                    end if;
                    valid_pipe(0) <= din_valid;
                    
                    -- Process each biquad section (Direct Form II Transposed)
                    for i in 0 to NUM_BIQUADS-1 loop
                        if valid_pipe(i) = '1' then
                            x_in := section_data(i);
                            
                            -- w[n] = x[n] - a1*w[n-1] - a2*w[n-2]
                            w_curr := resize(
                                x_in - 
                                shift_right(
                                    resize(coeffs(i)(3) * resize(states_df2(i).w1, COEFF_WIDTH), INTERNAL_WIDTH) +
                                    resize(coeffs(i)(4) * resize(states_df2(i).w2, COEFF_WIDTH), INTERNAL_WIDTH),
                                    COEFF_WIDTH - 1
                                ),
                                INTERNAL_WIDTH
                            );
                            
                            -- y[n] = b0*w[n] + b1*w[n-1] + b2*w[n-2]
                            y_out := shift_right(
                                resize(coeffs(i)(0) * resize(w_curr, COEFF_WIDTH), INTERNAL_WIDTH) +
                                resize(coeffs(i)(1) * resize(states_df2(i).w1, COEFF_WIDTH), INTERNAL_WIDTH) +
                                resize(coeffs(i)(2) * resize(states_df2(i).w2, COEFF_WIDTH), INTERNAL_WIDTH),
                                COEFF_WIDTH - 1
                            );
                            
                            -- Update states
                            states_df2(i).w2 <= states_df2(i).w1;
                            states_df2(i).w1 <= w_curr;
                            
                            -- Anti-windup: clamp state variables to prevent overflow
                            if states_df2(i).w1 > shift_left(to_signed(1, INTERNAL_WIDTH), INTERNAL_WIDTH-2) then
                                states_df2(i).w1 <= shift_left(to_signed(1, INTERNAL_WIDTH), INTERNAL_WIDTH-2);
                            elsif states_df2(i).w1 < -shift_left(to_signed(1, INTERNAL_WIDTH), INTERNAL_WIDTH-2) then
                                states_df2(i).w1 <= -shift_left(to_signed(1, INTERNAL_WIDTH), INTERNAL_WIDTH-2);
                            end if;
                            
                            -- Output to next section
                            section_data(i+1) <= y_out;
                        end if;
                        
                        valid_pipe(i+1) <= valid_pipe(i);
                    end loop;
                    
                    
                    dout_valid_reg <= valid_pipe(NUM_BIQUADS);
                end if;
            end if;
        end process;
    end generate;

    -- Output assignments
    dout       <= std_logic_vector(dout_reg);
    dout_valid <= dout_valid_reg;
    overflow   <= overflow_detect;
    underflow  <= underflow_detect;

end architecture rtl;