-- ===========================================================================
--  pkg_fft_types.vhd : Type definitions and utilities for FFT
-- ===========================================================================

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use work.pkg_dsp_types.all;

package pkg_fft_types is

    --------------------------------------------------------------------------
    -- FFT Architecture Types
    --------------------------------------------------------------------------
    type fft_arch_t is (
        RADIX_2_DIT,        -- Decimation in Time
        RADIX_2_DIF         -- Decimation in Frequency
    );
    
    type window_type_t is (
        WINDOW_NONE,
        WINDOW_HANNING,
        WINDOW_HAMMING,
        WINDOW_BLACKMAN
    );
    
    --------------------------------------------------------------------------
    -- Complex Data Types
    --------------------------------------------------------------------------
    type complex_16_t is record
        re : signed(15 downto 0);
        im : signed(15 downto 0);
    end record;
    
    type complex_array_16_t is array (natural range <>) of complex_16_t;
    
    --------------------------------------------------------------------------
    -- Twiddle Factor Type
    --------------------------------------------------------------------------
    type twiddle_t is record
        cos_val : signed(15 downto 0);  -- Real part (cosine)
        sin_val : signed(15 downto 0);  -- Imaginary part (sine)
    end record;
    
    type twiddle_array_t is array (natural range <>) of twiddle_t;
    
    --------------------------------------------------------------------------
    -- Window Coefficient Type
    --------------------------------------------------------------------------
    type window_coeff_array_t is array (natural range <>) of signed(15 downto 0);
    
    --------------------------------------------------------------------------
    -- Utility Functions
    --------------------------------------------------------------------------
    
    -- Calculate log2 (ceiling)
    function log2_ceil(n : integer) return integer;
    
    -- Calculate bit-reversed address
    function bit_reverse(addr : unsigned; width : integer) return unsigned;
    
    -- Generate twiddle factors for given FFT size
    function generate_twiddle_rom(fft_size : integer; width : integer) return twiddle_array_t;
    
    -- Generate window coefficients
    function generate_window(fft_size : integer; window_type : window_type_t; width : integer) 
        return window_coeff_array_t;
    
    -- Complex multiplication with rounding
    procedure complex_mult(
        a_re, a_im : in signed;
        b_re, b_im : in signed;
        signal p_re, p_im : out signed
    );
    
    -- Butterfly operation (radix-2)
    procedure butterfly_radix2(
        x0_re, x0_im : in signed;
        x1_re, x1_im : in signed;
        tw_re, tw_im : in signed;
        signal y0_re, y0_im : out signed;
        signal y1_re, y1_im : out signed
    );

end package pkg_fft_types;

package body pkg_fft_types is

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
    
    function bit_reverse(addr : unsigned; width : integer) return unsigned is
        variable result : unsigned(width-1 downto 0) := (others => '0');
    begin
        for i in 0 to width-1 loop
            result(i) := addr(width-1-i);
        end loop;
        return result;
    end function;
    
    function generate_twiddle_rom(fft_size : integer; width : integer) return twiddle_array_t is
        variable rom : twiddle_array_t(0 to fft_size/2-1);
        variable angle : real;
        variable max_val : real := real(2**(width-1) - 1);
    begin
        for i in 0 to fft_size/2-1 loop
            angle := -2.0 * MATH_PI * real(i) / real(fft_size);
            rom(i).cos_val := to_signed(integer(max_val * cos(angle)), width);
            rom(i).sin_val := to_signed(integer(max_val * sin(angle)), width);
        end loop;
        return rom;
    end function;
    
    function generate_window(fft_size : integer; window_type : window_type_t; width : integer) 
        return window_coeff_array_t is
        variable coeffs : window_coeff_array_t(0 to fft_size-1);
        variable n : real;
        variable val : real;
        variable max_val : real := real(2**(width-1) - 1);
    begin
        for i in 0 to fft_size-1 loop
            n := real(i);
            
            case window_type is
                when WINDOW_NONE =>
                    val := 1.0;
                    
                when WINDOW_HANNING =>
                    val := 0.5 - 0.5 * cos(2.0 * MATH_PI * n / real(fft_size-1));
                    
                when WINDOW_HAMMING =>
                    val := 0.54 - 0.46 * cos(2.0 * MATH_PI * n / real(fft_size-1));
                    
                when WINDOW_BLACKMAN =>
                    val := 0.42 - 0.5 * cos(2.0 * MATH_PI * n / real(fft_size-1)) +
                           0.08 * cos(4.0 * MATH_PI * n / real(fft_size-1));
            end case;
            
            coeffs(i) := to_signed(integer(max_val * val), width);
        end loop;
        return coeffs;
    end function;
    
    procedure complex_mult(
        a_re, a_im : in signed;
        b_re, b_im : in signed;
        signal p_re, p_im : out signed
    ) is
        variable temp_re : signed(a_re'length + b_re'length - 1 downto 0);
        variable temp_im : signed(a_im'length + b_im'length - 1 downto 0);
    begin
        -- (a_re + j*a_im) * (b_re + j*b_im) = 
        -- (a_re*b_re - a_im*b_im) + j*(a_re*b_im + a_im*b_re)
        temp_re := (a_re * b_re) - (a_im * b_im);
        temp_im := (a_re * b_im) + (a_im * b_re);
        
        -- Scale down from Q30 to Q15 with rounding
        p_re <= resize(shift_right(temp_re + 16384, 15), a_re'length);
        p_im <= resize(shift_right(temp_im + 16384, 15), a_im'length);
    end procedure;
    
    procedure butterfly_radix2(
        x0_re, x0_im : in signed;
        x1_re, x1_im : in signed;
        tw_re, tw_im : in signed;
        signal y0_re, y0_im : out signed;
        signal y1_re, y1_im : out signed
    ) is
        variable temp_re : signed(x0_re'length + tw_re'length - 1 downto 0);
        variable temp_im : signed(x0_im'length + tw_im'length - 1 downto 0);
        variable prod_re : signed(x0_re'length - 1 downto 0);
        variable prod_im : signed(x0_im'length - 1 downto 0);
    begin
        -- Complex multiplication: x1 * twiddle
        temp_re := (x1_re * tw_re) - (x1_im * tw_im);
        temp_im := (x1_re * tw_im) + (x1_im * tw_re);
        
        -- Scale down with rounding
        prod_re := resize(shift_right(temp_re + 16384, 15), x0_re'length);
        prod_im := resize(shift_right(temp_im + 16384, 15), x0_im'length);
        
        -- Butterfly: y0 = x0 + x1*W, y1 = x0 - x1*W
        y0_re <= resize(x0_re + prod_re, x0_re'length);
        y0_im <= resize(x0_im + prod_im, x0_im'length);
        y1_re <= resize(x0_re - prod_re, x0_re'length);
        y1_im <= resize(x0_im - prod_im, x0_im'length);
    end procedure;

end package body pkg_fft_types;