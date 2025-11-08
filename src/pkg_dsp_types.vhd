library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package pkg_dsp_types is

    --------------------------------------------------------------------------
    -- Constantes génériques pour largeur de données
    --------------------------------------------------------------------------
    constant DEFAULT_DATA_WIDTH   : integer := 16;
    constant DEFAULT_COEFF_WIDTH  : integer := 16;
    constant MAX_TAPS             : integer := 128;   -- Pour FIR
    constant MAX_ORDER            : integer := 8;     -- Pour IIR
    constant MAX_FFT_SIZE         : integer := 1024;  -- Pour FFT

    --------------------------------------------------------------------------
    -- Types pour données et coefficients (signed/fixed)
    --------------------------------------------------------------------------
    subtype dsp_data_t  is signed(DEFAULT_DATA_WIDTH-1 downto 0);
    subtype dsp_coeff_t is signed(DEFAULT_COEFF_WIDTH-1 downto 0);

    -- Tableau de coefficients pour FIR/IIR
    type coeff_array_t is array (natural range <>) of dsp_coeff_t;
    -- Tableau de données
    type data_array_t  is array (natural range <>) of dsp_data_t;

    -- Tableau complexe pour FFT (re & im)
    type complex_t is record
        re : dsp_data_t;
        im : dsp_data_t;
    end record;

    type complex_array_t is array (natural range <>) of complex_t;

    --------------------------------------------------------------------------
    -- Fonctions utilitaires pour la saturation (overflow clipping)
    --------------------------------------------------------------------------
    function saturate(val : signed; width : integer) return signed;

end package pkg_dsp_types;

package body pkg_dsp_types is

    -- Fonction de saturation (overflow -> valeur max ou min)
	function saturate(val : signed; width : integer) return signed is
		variable result    : signed(width-1 downto 0);
		variable max_value : signed(width-1 downto 0);
		variable min_value : signed(width-1 downto 0);
	begin
		-- Valeur max : "011...1"
		max_value := (others => '1');
		max_value(width-1) := '0';

		-- Valeur min : "100...0"
		min_value := (others => '0');
		min_value(width-1) := '1';

		if val > max_value then
			result := max_value;
		elsif val < min_value then
			result := min_value;
		else
			result := resize(val, width);
		end if;

		return result;
	end function saturate;


end package body pkg_dsp_types;
