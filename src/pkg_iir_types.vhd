-- ===========================================================================
--  pkg_iir_types.vhd : Type definitions and utilities for IIR filters
-- ===========================================================================

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package pkg_iir_types is

    --------------------------------------------------------------------------
    -- IIR Filter Structure Types
    --------------------------------------------------------------------------
    type iir_structure_t is (
        DIRECT_FORM_I,      -- Standard Direct Form I (more states, more stable)
        DIRECT_FORM_II      -- Direct Form II Transposed (fewer states, efficient)
    );
    
    --------------------------------------------------------------------------
    -- Biquad Coefficient Record
    -- Standard biquad transfer function:
    -- H(z) = (b0 + b1*z^-1 + b2*z^-2) / (1 + a1*z^-1 + a2*z^-2)
    -- Note: a0 is normalized to 1
    --------------------------------------------------------------------------
    type biquad_coeffs_t is record
        b0 : signed(15 downto 0);  -- Feedforward coefficients
        b1 : signed(15 downto 0);
        b2 : signed(15 downto 0);
        a1 : signed(15 downto 0);  -- Feedback coefficients
        a2 : signed(15 downto 0);
    end record;
    
    type biquad_array_t is array (natural range <>) of biquad_coeffs_t;
    
    --------------------------------------------------------------------------
    -- Predefined IIR Filter Coefficients (16-bit Q15 format)
    --------------------------------------------------------------------------
    
    -- Low-pass Butterworth, Fc=0.1*Fs, 2nd order
    constant BIQUAD_LOWPASS_01 : biquad_coeffs_t := (
        b0 => to_signed( 1311, 16),   -- 0.04
        b1 => to_signed( 2622, 16),   -- 0.08
        b2 => to_signed( 1311, 16),   -- 0.04
        a1 => to_signed(-20248, 16),  -- -0.6180
        a2 => to_signed( 5492, 16)    -- 0.1675
    );
    
    -- High-pass Butterworth, Fc=0.1*Fs, 2nd order
    constant BIQUAD_HIGHPASS_01 : biquad_coeffs_t := (
        b0 => to_signed( 28377, 16),  -- 0.8662
        b1 => to_signed(-56754, 16),  -- -1.7325
        b2 => to_signed( 28377, 16),  -- 0.8662
        a1 => to_signed(-20248, 16),  -- -0.6180
        a2 => to_signed( 5492, 16)    -- 0.1675
    );
    
    -- Band-pass, Fc=0.1*Fs, BW=0.05*Fs, 2nd order
    constant BIQUAD_BANDPASS : biquad_coeffs_t := (
        b0 => to_signed( 5242, 16),   -- 0.16
        b1 => to_signed(    0, 16),   -- 0.00
        b2 => to_signed(-5242, 16),   -- -0.16
        a1 => to_signed(-25886, 16),  -- -0.79
        a2 => to_signed( 21626, 16)   -- 0.66
    );
    
    -- Notch (Band-stop), Fc=0.1*Fs, BW=0.05*Fs, 2nd order
    constant BIQUAD_NOTCH : biquad_coeffs_t := (
        b0 => to_signed( 27525, 16),  -- 0.84
        b1 => to_signed(-25886, 16),  -- -0.79
        b2 => to_signed( 27525, 16),  -- 0.84
        a1 => to_signed(-25886, 16),  -- -0.79
        a2 => to_signed( 21626, 16)   -- 0.66
    );
    
    -- All-pass (Phase shifter), 2nd order
    constant BIQUAD_ALLPASS : biquad_coeffs_t := (
        b0 => to_signed( 5492, 16),   -- 0.1675
        b1 => to_signed(-20248, 16),  -- -0.6180
        b2 => to_signed( 32767, 16),  -- 1.0
        a1 => to_signed(-20248, 16),  -- -0.6180
        a2 => to_signed( 5492, 16)    -- 0.1675
    );
    
    -- Resonator (Peak filter), Q=10
    constant BIQUAD_RESONATOR : biquad_coeffs_t := (
        b0 => to_signed( 1638, 16),   -- 0.05
        b1 => to_signed(    0, 16),   -- 0.00
        b2 => to_signed(-1638, 16),   -- -0.05
        a1 => to_signed(-29491, 16),  -- -0.90
        a2 => to_signed( 31130, 16)   -- 0.95
    );
    
    --------------------------------------------------------------------------
    -- Utility Functions
    --------------------------------------------------------------------------
    
    -- Convert float to Q15 signed (for coefficient generation)
    function float_to_q15(f : real) return signed;
    
    -- Compute biquad gain at DC
    function biquad_dc_gain(coeffs : biquad_coeffs_t) return real;
    
    -- Check stability (poles inside unit circle)
    function is_stable(coeffs : biquad_coeffs_t) return boolean;
    
    -- Scale coefficients to prevent overflow
    function scale_coeffs(coeffs : biquad_coeffs_t; scale : real) return biquad_coeffs_t;

end package pkg_iir_types;

package body pkg_iir_types is

    function float_to_q15(f : real) return signed is
        variable temp : integer;
    begin
        temp := integer(f * 32768.0);
        if temp > 32767 then
            temp := 32767;
        elsif temp < -32768 then
            temp := -32768;
        end if;
        return to_signed(temp, 16);
    end function;
    
    function biquad_dc_gain(coeffs : biquad_coeffs_t) return real is
        variable num_sum : real;
        variable den_sum : real;
    begin
        num_sum := real(to_integer(coeffs.b0)) + 
                   real(to_integer(coeffs.b1)) + 
                   real(to_integer(coeffs.b2));
        den_sum := 32768.0 + 
                   real(to_integer(coeffs.a1)) + 
                   real(to_integer(coeffs.a2));
        
        if den_sum /= 0.0 then
            return num_sum / den_sum;
        else
            return 0.0;
        end if;
    end function;
    
    function is_stable(coeffs : biquad_coeffs_t) return boolean is
        variable a1 : real;
        variable a2 : real;
    begin
        -- Convert to normalized form
        a1 := real(to_integer(coeffs.a1)) / 32768.0;
        a2 := real(to_integer(coeffs.a2)) / 32768.0;
        
        -- Stability conditions for 2nd order system:
        -- |a2| < 1
        -- |a1| < 1 + a2
        
        if abs(a2) >= 1.0 then
            return false;
        end if;
        
        if abs(a1) >= (1.0 + a2) then
            return false;
        end if;
        
        return true;
    end function;
    
    function scale_coeffs(coeffs : biquad_coeffs_t; scale : real) return biquad_coeffs_t is
        variable result : biquad_coeffs_t;
    begin
        result.b0 := to_signed(integer(real(to_integer(coeffs.b0)) * scale), 16);
        result.b1 := to_signed(integer(real(to_integer(coeffs.b1)) * scale), 16);
        result.b2 := to_signed(integer(real(to_integer(coeffs.b2)) * scale), 16);
        result.a1 := coeffs.a1;  -- Don't scale denominator
        result.a2 := coeffs.a2;
        return result;
    end function;

end package body pkg_iir_types;