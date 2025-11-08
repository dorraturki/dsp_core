-- ===========================================================================
--  fft_radix2.vhd : Parameterizable Radix-2 FFT Core
-- ===========================================================================

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use work.pkg_dsp_types.all;
use work.pkg_fft_types.all;

entity fft_radix2 is
    generic (
        FFT_SIZE         : integer := 128;        -- Must be power of 2
        DATA_WIDTH       : integer := 16;         -- Input/output width
        TWIDDLE_WIDTH    : integer := 16;         -- Twiddle factor width
        SIGNED_MODE      : boolean := true;       -- Signed/unsigned
        PIPELINE_STAGES  : integer := 2;          -- Pipeline depth per stage
        USE_BIT_REVERSE  : boolean := true;       -- Auto bit-reversal
        USE_WINDOWING    : boolean := false;      -- Enable windowing
        WINDOW_TYPE      : window_type_t := WINDOW_HANNING
    );
    port (
        clk              : in  std_logic;
        reset            : in  std_logic;
        
        -- Input interface
        din_re           : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        din_im           : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        din_valid        : in  std_logic;
        din_last         : in  std_logic;  -- Last sample of frame
        
        -- Output interface
        dout_re          : out std_logic_vector(DATA_WIDTH-1 downto 0);
        dout_im          : out std_logic_vector(DATA_WIDTH-1 downto 0);
        dout_valid       : out std_logic;
        dout_last        : out std_logic;
        
        -- Status
        overflow         : out std_logic;
        ready            : out std_logic
    );
end entity fft_radix2;

architecture rtl of fft_radix2 is

    -- Constants
    constant NUM_STAGES : integer := log2_ceil(FFT_SIZE);
    constant ADDR_WIDTH : integer := log2_ceil(FFT_SIZE);
    
    -- Generate twiddle ROM
    constant TWIDDLE_ROM : twiddle_array_t(0 to FFT_SIZE/2-1) := 
        generate_twiddle_rom(FFT_SIZE, TWIDDLE_WIDTH);
    
    -- Generate window coefficients
    constant WINDOW_ROM : window_coeff_array_t(0 to FFT_SIZE-1) := 
        generate_window(FFT_SIZE, WINDOW_TYPE, DATA_WIDTH);
    
    -- FSM states
    type state_t is (IDLE, LOADING, PROCESSING, UNLOADING);
    signal state : state_t := IDLE;
    
    -- Dual-port RAM for ping-pong buffering
    type ram_t is array (0 to FFT_SIZE-1) of complex_16_t;
    signal ram_a : ram_t := (others => (re => (others => '0'), im => (others => '0')));
    signal ram_b : ram_t := (others => (re => (others => '0'), im => (others => '0')));
    
    -- Control signals
    signal current_buffer    : std_logic := '0';  -- 0=ram_a, 1=ram_b
    signal write_addr        : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
    signal read_addr         : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
    signal stage_counter     : integer range 0 to NUM_STAGES := 0;
    signal butterfly_counter : integer range 0 to FFT_SIZE/2 := 0;
    
    -- Butterfly signals
    signal bf_x0_re, bf_x0_im : signed(DATA_WIDTH-1 downto 0);
    signal bf_x1_re, bf_x1_im : signed(DATA_WIDTH-1 downto 0);
    signal bf_tw_re, bf_tw_im : signed(TWIDDLE_WIDTH-1 downto 0);
    signal bf_y0_re, bf_y0_im : signed(DATA_WIDTH-1 downto 0);
    signal bf_y1_re, bf_y1_im : signed(DATA_WIDTH-1 downto 0);
    
    -- Pipeline registers
    type pipe_stage_t is record
        re : signed(DATA_WIDTH-1 downto 0);
        im : signed(DATA_WIDTH-1 downto 0);
        valid : std_logic;
    end record;
    
    type pipe_array_t is array (0 to PIPELINE_STAGES-1) of pipe_stage_t;
    signal pipe_y0 : pipe_array_t;
    signal pipe_y1 : pipe_array_t;
    
    -- Output signals
    signal dout_re_reg   : signed(DATA_WIDTH-1 downto 0) := (others => '0');
    signal dout_im_reg   : signed(DATA_WIDTH-1 downto 0) := (others => '0');
    signal dout_valid_reg : std_logic := '0';
    signal dout_last_reg  : std_logic := '0';
    signal overflow_reg   : std_logic := '0';
    signal ready_reg      : std_logic := '1';
    
    -- Helper functions
    function get_twiddle_index(stage : integer; k : integer; n : integer) return integer is
        variable group_size : integer;
        variable twiddle_step : integer;
    begin
        group_size := 2**(stage + 1);
        twiddle_step := FFT_SIZE / group_size;
        return (k mod (group_size/2)) * twiddle_step;
    end function;
    
    function get_butterfly_pair(stage : integer; k : integer; n : integer) 
        return integer is
        variable group_size : integer;
        variable group_num : integer;
        variable pos_in_group : integer;
    begin
        group_size := 2**(stage + 1);
        group_num := k / (group_size / 2);
        pos_in_group := k mod (group_size / 2);
        return group_num * group_size + pos_in_group + group_size / 2;
    end function;

begin

    -- Input windowing (optional)
    windowing_gen : if USE_WINDOWING generate
        process(clk)
            variable windowed_re : signed(DATA_WIDTH*2-1 downto 0);
            variable windowed_im : signed(DATA_WIDTH*2-1 downto 0);
        begin
            if rising_edge(clk) then
                if din_valid = '1' and state = LOADING then
                    windowed_re := signed(din_re) * WINDOW_ROM(to_integer(write_addr));
                    windowed_im := signed(din_im) * WINDOW_ROM(to_integer(write_addr));
                    
                    if current_buffer = '0' then
                        ram_a(to_integer(write_addr)).re <= 
                            resize(shift_right(windowed_re, DATA_WIDTH-1), DATA_WIDTH);
                        ram_a(to_integer(write_addr)).im <= 
                            resize(shift_right(windowed_im, DATA_WIDTH-1), DATA_WIDTH);
                    else
                        ram_b(to_integer(write_addr)).re <= 
                            resize(shift_right(windowed_re, DATA_WIDTH-1), DATA_WIDTH);
                        ram_b(to_integer(write_addr)).im <= 
                            resize(shift_right(windowed_im, DATA_WIDTH-1), DATA_WIDTH);
                    end if;
                end if;
            end if;
        end process;
    end generate;
    
    no_windowing_gen : if not USE_WINDOWING generate
        process(clk)
        begin
            if rising_edge(clk) then
                if din_valid = '1' and state = LOADING then
                    if current_buffer = '0' then
                        ram_a(to_integer(write_addr)).re <= signed(din_re);
                        ram_a(to_integer(write_addr)).im <= signed(din_im);
                    else
                        ram_b(to_integer(write_addr)).re <= signed(din_re);
                        ram_b(to_integer(write_addr)).im <= signed(din_im);
                    end if;
                end if;
            end if;
        end process;
    end generate;

    -- Main FSM and control logic
    main_process : process(clk)
        variable addr0, addr1 : integer;
        variable tw_idx : integer;
        variable bit_rev_addr : unsigned(ADDR_WIDTH-1 downto 0);
    begin
        if rising_edge(clk) then
            if reset = '1' then
                state <= IDLE;
                write_addr <= (others => '0');
                read_addr <= (others => '0');
                stage_counter <= 0;
                butterfly_counter <= 0;
                current_buffer <= '0';
                dout_valid_reg <= '0';
                dout_last_reg <= '0';
                ready_reg <= '1';
                overflow_reg <= '0';
                
            else
                case state is
                    when IDLE =>
                        ready_reg <= '1';
                        dout_valid_reg <= '0';
                        
                        if din_valid = '1' then
                            state <= LOADING;
                            write_addr <= (others => '0');
                            ready_reg <= '0';
                        end if;
                    
                    when LOADING =>
                        if din_valid = '1' then
                            write_addr <= write_addr + 1;
                            
                            if din_last = '1' or write_addr = FFT_SIZE-1 then
                                state <= PROCESSING;
                                stage_counter <= 0;
                                butterfly_counter <= 0;
                            end if;
                        end if;
                    
                    when PROCESSING =>
                        if stage_counter < NUM_STAGES then
                            -- Calculate addresses for butterfly
                            addr0 := butterfly_counter;
                            addr1 := get_butterfly_pair(stage_counter, butterfly_counter, FFT_SIZE);
                            tw_idx := get_twiddle_index(stage_counter, butterfly_counter, FFT_SIZE);
                            
                            -- Read operands
                            if current_buffer = '0' then
                                bf_x0_re <= ram_a(addr0).re;
                                bf_x0_im <= ram_a(addr0).im;
                                bf_x1_re <= ram_a(addr1).re;
                                bf_x1_im <= ram_a(addr1).im;
                            else
                                bf_x0_re <= ram_b(addr0).re;
                                bf_x0_im <= ram_b(addr0).im;
                                bf_x1_re <= ram_b(addr1).re;
                                bf_x1_im <= ram_b(addr1).im;
                            end if;
                            
                            -- Get twiddle factor
                            bf_tw_re <= TWIDDLE_ROM(tw_idx).cos_val;
                            bf_tw_im <= TWIDDLE_ROM(tw_idx).sin_val;
                            
                            -- Perform butterfly
                            butterfly_radix2(bf_x0_re, bf_x0_im, bf_x1_re, bf_x1_im,
                                           bf_tw_re, bf_tw_im,
                                           bf_y0_re, bf_y0_im, bf_y1_re, bf_y1_im);
                            
                            -- Write results back (swap buffer)
                            if current_buffer = '0' then
                                ram_b(addr0).re <= bf_y0_re;
                                ram_b(addr0).im <= bf_y0_im;
                                ram_b(addr1).re <= bf_y1_re;
                                ram_b(addr1).im <= bf_y1_im;
                            else
                                ram_a(addr0).re <= bf_y0_re;
                                ram_a(addr0).im <= bf_y0_im;
                                ram_a(addr1).re <= bf_y1_re;
                                ram_a(addr1).im <= bf_y1_im;
                            end if;
                            
                            butterfly_counter <= butterfly_counter + 1;
                            
                            if butterfly_counter = FFT_SIZE/2 - 1 then
                                butterfly_counter <= 0;
                                stage_counter <= stage_counter + 1;
                                current_buffer <= not current_buffer;
                            end if;
                        else
                            state <= UNLOADING;
                            read_addr <= (others => '0');
                        end if;
                    
                    when UNLOADING =>
                        -- Output with optional bit-reversal
                        if USE_BIT_REVERSE then
                            bit_rev_addr := bit_reverse(read_addr, ADDR_WIDTH);
                            if current_buffer = '0' then
                                dout_re_reg <= ram_a(to_integer(bit_rev_addr)).re;
                                dout_im_reg <= ram_a(to_integer(bit_rev_addr)).im;
                            else
                                dout_re_reg <= ram_b(to_integer(bit_rev_addr)).re;
                                dout_im_reg <= ram_b(to_integer(bit_rev_addr)).im;
                            end if;
                        else
                            if current_buffer = '0' then
                                dout_re_reg <= ram_a(to_integer(read_addr)).re;
                                dout_im_reg <= ram_a(to_integer(read_addr)).im;
                            else
                                dout_re_reg <= ram_b(to_integer(read_addr)).re;
                                dout_im_reg <= ram_b(to_integer(read_addr)).im;
                            end if;
                        end if;
                        
                        dout_valid_reg <= '1';
                        dout_last_reg <= '0';
                        
                        if read_addr = FFT_SIZE-1 then
                            dout_last_reg <= '1';
                            state <= IDLE;
                        end if;
                        
                        read_addr <= read_addr + 1;
                        
                end case;
            end if;
        end if;
    end process;

    -- Output assignments
    dout_re    <= std_logic_vector(dout_re_reg);
    dout_im    <= std_logic_vector(dout_im_reg);
    dout_valid <= dout_valid_reg;
    dout_last  <= dout_last_reg;
    overflow   <= overflow_reg;
    ready      <= ready_reg;

end architecture rtl;