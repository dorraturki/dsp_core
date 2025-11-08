library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.pkg_dsp_types.all;

package pkg_coeffs is

    --------------------------------------------------------------------------
    -- Exemple : coefficients FIR 32 taps, format signé 16 bits
    --------------------------------------------------------------------------
		constant FIR_COEFF_32 : coeff_array_t(0 to 31) := (
			to_signed(    54, 16),  -- h[0]
			to_signed(    54, 16),  -- h[1]
			to_signed(    44, 16),  -- h[2]
			to_signed(     0, 16),  -- h[3]
			to_signed(   -97, 16),  -- h[4]
			to_signed(  -248, 16),  -- h[5]
			to_signed(  -415, 16),  -- h[6]
			to_signed(  -521, 16),  -- h[7]
			to_signed(  -459, 16),  -- h[8]
			to_signed(  -131, 16),  -- h[9]
			to_signed(   517, 16),  -- h[10]
			to_signed(  1463, 16),  -- h[11]
			to_signed(  2594, 16),  -- h[12]
			to_signed(  3727, 16),  -- h[13]
			to_signed(  4645, 16),  -- h[14]
			to_signed(  5159, 16),  -- h[15]
			to_signed(  5159, 16),  -- h[16]
			to_signed(  4645, 16),  -- h[17]
			to_signed(  3727, 16),  -- h[18]
			to_signed(  2594, 16),  -- h[19]
			to_signed(  1463, 16),  -- h[20]
			to_signed(   517, 16),  -- h[21]
			to_signed(  -131, 16),  -- h[22]
			to_signed(  -459, 16),  -- h[23]
			to_signed(  -521, 16),  -- h[24]
			to_signed(  -415, 16),  -- h[25]
			to_signed(  -248, 16),  -- h[26]
			to_signed(   -97, 16),  -- h[27]
			to_signed(     0, 16),  -- h[28]
			to_signed(    44, 16),  -- h[29]
			to_signed(    54, 16),  -- h[30]
			to_signed(    54, 16)  -- h[31]
		);


    --------------------------------------------------------------------------
    -- Exemple : coefficients IIR de type biquad (ordre 2), format signé 16 bits
    --------------------------------------------------------------------------
    constant IIR_B_COEFF_2 : coeff_array_t(0 to 2) := (
        to_signed(3277, 16),     -- b0
        to_signed(6554, 16),     -- b1
        to_signed(3277, 16)      -- b2
    );
    constant IIR_A_COEFF_2 : coeff_array_t(0 to 2) := (
        to_signed(4096, 16),     -- a0 (en général normalisé à 1)
        to_signed(-2570, 16),    -- a1
        to_signed( 481, 16)      -- a2
    );

    --------------------------------------------------------------------------
    -- Exemple : coefficients de twiddle factors pour FFT 8 points, format signé 16 bits
    --------------------------------------------------------------------------
    constant FFT8_TWIDDLE_REAL : coeff_array_t(0 to 3) := (
        to_signed(32767, 16),   -- cos(0)
        to_signed(23170, 16),   -- cos(pi/4)
        to_signed(    0, 16),   -- cos(pi/2)
        to_signed(-23170, 16)   -- cos(3pi/4)
    );
    constant FFT8_TWIDDLE_IMAG : coeff_array_t(0 to 3) := (
        to_signed(    0, 16),   -- sin(0)
        to_signed(-23170, 16),  -- sin(-pi/4)
        to_signed(-32767, 16),  -- sin(-pi/2)
        to_signed(-23170, 16)   -- sin(-3pi/4)
    );

end package pkg_coeffs;
