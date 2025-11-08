library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.pkg_dsp_types.all;
use work.pkg_coeffs.all;

entity fir_filter_enhanced is
    generic (
        TAPS             : integer := 32;
        DATA_WIDTH       : integer := DEFAULT_DATA_WIDTH;
        COEFF_WIDTH      : integer := DEFAULT_COEFF_WIDTH;
        PIPELINE_STAGES  : integer := 3;  -- MAC pipeline depth
        COEFF_RELOADABLE : boolean := false
    );
    port (
        clk          : in  std_logic;
        reset        : in  std_logic;
        
        -- Data interface
        din          : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        din_valid    : in  std_logic;
        dout         : out std_logic_vector(DATA_WIDTH-1 downto 0);
        dout_valid   : out std_logic;
        
        -- Coefficient reload interface (optional)
        coeff_wr_en  : in  std_logic := '0';
        coeff_addr   : in  std_logic_vector(7 downto 0) := (others => '0');
        coeff_data   : in  std_logic_vector(COEFF_WIDTH-1 downto 0) := (others => '0')
    );
end entity fir_filter_enhanced;

architecture rtl of fir_filter_enhanced is
	 -- Helper function for log2 ceiling
    function log2_ceil(n : integer) return integer is
        variable temp : integer := n;
        variable result : integer := 0;
    begin
        while temp > 1 loop
            temp := (temp + 1) / 2;
            result := result + 1;
        end loop;
        return result;
    end function;
	
	    -- Helper function
    function minimum(a, b : integer) return integer is
    begin
        if a < b then
            return a;
        else
            return b;
        end if;
    end function;
	
    -- Constants
    constant ACC_WIDTH : integer := DATA_WIDTH + COEFF_WIDTH + integer(log2_ceil(TAPS)) + 1;
    constant LATENCY   : integer := TAPS + PIPELINE_STAGES + 2;
    
    -- Data shift register
    type data_tap_array is array (0 to TAPS-1) of signed(DATA_WIDTH-1 downto 0);
    signal x_reg : data_tap_array := (others => (others => '0'));
    
    -- Coefficient storage
    type coeff_ram_t is array (0 to TAPS-1) of signed(COEFF_WIDTH-1 downto 0);
	
    function init_coeffs(source : coeff_array_t; length : integer) return coeff_ram_t is
		variable temp : coeff_ram_t := (others => (others => '0'));
		begin
			for i in 0 to length-1 loop
				temp(i) := source(i);
			end loop;
			return temp;
	end function;
	
	constant COEFFS_INIT : coeff_ram_t := init_coeffs(FIR_COEFF_32, TAPS);
	signal coeffs : coeff_ram_t := COEFFS_INIT;

    -- Pipeline stages for MAC operations
    constant MACS_PER_STAGE : integer := (TAPS + PIPELINE_STAGES - 1) / PIPELINE_STAGES;
    
    type mac_pipe_t is array (0 to PIPELINE_STAGES) of signed(ACC_WIDTH-1 downto 0);
    signal mac_pipe : mac_pipe_t := (others => (others => '0'));
    
    -- Valid signal pipeline
    signal valid_pipe : std_logic_vector(0 to LATENCY-1) := (others => '0');
    
    -- Output registers
    signal dout_reg       : signed(DATA_WIDTH-1 downto 0);
    signal dout_valid_reg : std_logic := '0';

begin
    ------------------------------------------------------------------------
    -- Main processing pipeline
    ------------------------------------------------------------------------
    process(clk)
        variable mac_result : signed(ACC_WIDTH-1 downto 0);
        variable mult_result : signed(DATA_WIDTH + COEFF_WIDTH - 1 downto 0);
        variable start_idx, end_idx : integer;
    begin
        if rising_edge(clk) then
            if reset = '1' then
                x_reg          <= (others => (others => '0'));
                mac_pipe       <= (others => (others => '0'));
                valid_pipe     <= (others => '0');
                dout_reg       <= (others => '0');
                dout_valid_reg <= '0';
                
            else
                -- Coefficient reload logic
                if COEFF_RELOADABLE and coeff_wr_en = '1' then
                    coeffs(to_integer(unsigned(coeff_addr))) <= signed(coeff_data);
                end if;
                
                -- Input data shift register
                if din_valid = '1' then
                    for i in TAPS-1 downto 1 loop
                        x_reg(i) <= x_reg(i-1);
                    end loop;
                    x_reg(0) <= signed(din);
                end if;
                
                -- Stage 0: Initialize accumulator
                mac_pipe(0) <= (others => '0');
                
                -- Pipelined MAC computation
                for stage in 0 to PIPELINE_STAGES-1 loop
                    start_idx := stage * MACS_PER_STAGE;
                    end_idx := minimum(start_idx + MACS_PER_STAGE - 1, TAPS - 1);
                    
                    mac_result := mac_pipe(stage);
                    
                    for i in start_idx to end_idx loop
                        mult_result := x_reg(i) * coeffs(i);
                        mac_result := mac_result + resize(mult_result, ACC_WIDTH);
                    end loop;
                    
                    mac_pipe(stage + 1) <= mac_result;
                end loop;
                
                -- Saturation and output
                dout_reg <= saturate(mac_pipe(PIPELINE_STAGES), DATA_WIDTH);
                
                -- Valid signal pipeline
                valid_pipe(0) <= din_valid;
                for i in 1 to LATENCY-1 loop
                    valid_pipe(i) <= valid_pipe(i-1);
                end loop;
                dout_valid_reg <= valid_pipe(LATENCY-1);
                
            end if;
        end if;
    end process;


    -- Output assignments
    dout       <= std_logic_vector(dout_reg);
    dout_valid <= dout_valid_reg;

end architecture rtl;