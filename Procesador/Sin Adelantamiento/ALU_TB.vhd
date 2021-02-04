library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_SIGNED.ALL;

entity ALU_TB is
end ALU_TB;

architecture Behavioral of ALU_TB is
    component ALU 
        Port ( a       : in STD_LOGIC_VECTOR (31 downto 0);   -- entradas
               b       : in STD_LOGIC_VECTOR (31 downto 0);
               control : in STD_LOGIC_VECTOR (2 downto 0);
               
               result  : out STD_LOGIC_VECTOR (31 downto 0);  --salidas
               zero    : out STD_LOGIC
              );
    end component;
    signal a       : STD_LOGIC_VECTOR (31 downto 0);  -- entradas
    signal b       : STD_LOGIC_VECTOR (31 downto 0);
    signal control : STD_LOGIC_VECTOR (2 downto 0);
   
    signal result  : STD_LOGIC_VECTOR (31 downto 0); --salidas
    signal zero    : STD_LOGIC;

begin
    uut: ALU PORT MAP(
		a => a,
		b => b,
		control => control,
		zero => zero,
		result => result
	);
    process
    begin 
        a <= x"0000000f";
        b <= x"000000ff";
        control <= "000";
        wait for 10 ns;
        control <= "001";
        wait for 10 ns;
        control <= "010";
        wait for 10 ns;
        control <= "110";
        wait for 10 ns;
        control <= "111";
        wait for 10 ns;
        control <= "100";
        wait;
    end process;

end;
