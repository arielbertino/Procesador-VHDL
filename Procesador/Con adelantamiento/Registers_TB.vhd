library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
--use ieee.numeric.std.all;

entity Registers_TB is
end Registers_TB;

architecture Behavioral of Registers_TB is
    component Registers is
    Port ( clk : in STD_LOGIC;
           reset : in STD_LOGIC;
           wr : in STD_LOGIC;
           reg1_rd : in STD_LOGIC_VECTOR (4 downto 0);
           reg2_rd : in STD_LOGIC_VECTOR (4 downto 0);
           reg_wr : in STD_LOGIC_VECTOR (4 downto 0);
           data_wr : in STD_LOGIC_VECTOR (31 downto 0);
           data1_rd : out STD_LOGIC_VECTOR (31 downto 0);
           data2_rd : out STD_LOGIC_VECTOR (31 downto 0));
     end component;
     signal clk :      STD_LOGIC;
     signal reset :    STD_LOGIC;
     signal wr :       STD_LOGIC;   -- habilitacion de escritura
     signal reg1_rd :  STD_LOGIC_VECTOR (4 downto 0);   -- dir de data1_rd
     signal reg2_rd :  STD_LOGIC_VECTOR (4 downto 0);   -- dir de data1_rd
     signal reg_wr :   STD_LOGIC_VECTOR (4 downto 0);   -- dir donde escribir en dato data_wr
     signal data_wr :  STD_LOGIC_VECTOR (31 downto 0);  -- dato a escribir en banco
     signal data1_rd : STD_LOGIC_VECTOR (31 downto 0);  -- salida 1 combinacional
     signal data2_rd : STD_LOGIC_VECTOR (31 downto 0); -- salida 2 combinacional
begin
    uut: Registers PORT MAP( 
		clk => clk,
		reset => reset,
		wr => wr,
		reg1_rd => reg1_rd,
		reg2_rd => reg2_rd,
		reg_wr => reg_wr,
		data_wr => data_wr,
		data1_rd => data1_rd,
		data2_rd => data2_rd
	);
    
    process -- generacion del clock
    begin
        clk <= '0';
        wait for 5 ns;
        clk <= '1';
        wait for 5 ns;
    end process;
    
    process -- resetear el banco de registros
    begin
       reset <= '1';
       wait for 5 ns;
        reset <='0'; 
        wait;
    end process;
    
    process -- carga de datos
    begin
        wr <= '1';
        wait for 10 ns;
        reg_wr <= "00000";
        data_wr <= x"00000000";
        wait for 10 ns;
        reg_wr <= "00001";
        data_wr <= x"00000001";
        wait for 10 ns;
        reg_wr <= "00010";
        data_wr <= x"00000002";
        wait for 10 ns;
        reg_wr <= "00011";
        data_wr <= x"00000003";
        wait for 10 ns;
        reg_wr <= "00100";
        data_wr <= x"00000004";
        wait for 10 ns;
        reg_wr <= "00000";
        data_wr <= x"00000004";
        wait for 10 ns;
        wr <= '0';
        wait;
    end process;

    process -- mostrar los valores por las distintas salidas
    begin
        reg1_rd <= "00000";
        wait for 10 ns;
        reg2_rd <= "00001";
        wait for 10 ns;
        reg1_rd <= "00010";
        wait for 10 ns;
        reg2_rd <= "00011";
        wait for 10 ns;
        reg1_rd <= "00100";
        wait for 10 ns;
        reg1_rd <= "00100";
        wait for 10 ns;
        reg1_rd <= "00100";
        wait;
    end process;
end Behavioral;
