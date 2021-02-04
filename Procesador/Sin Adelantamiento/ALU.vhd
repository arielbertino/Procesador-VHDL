library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_SIGNED.ALL;

entity ALU is
    Port ( a       : in STD_LOGIC_VECTOR (31 downto 0);
           b       : in STD_LOGIC_VECTOR (31 downto 0);
           control : in STD_LOGIC_VECTOR (2 downto 0);
           result  : out STD_LOGIC_VECTOR (31 downto 0);
           zero    : out STD_LOGIC);
end ALU;

architecture Behavioral of ALU is
    SIGNAL res: STD_LOGIC_VECTOR (31 downto 0);
begin
    process (a, b, control)
    begin
       case control is
          when  "000" => res <= a and b;
          when  "001" => res <= a or b;
          when  "010" => res <= a + b;
          when  "100" => res <= b(15 downto 0) & x"0000";
          when  "110" => res <= a - b;
          when  "111" => 
                if (a < b) then
                    res <= x"00000001";
                else
                    res <= x"00000000";
                end if;
          when others => res <= x"00000000";
       end case;
    end process;
    process (res)
    begin
        if (res = x"00000000") then
            zero <= '1';
        else
            zero <= '0';
        end if;
    end process;
    result <= res;
end Behavioral;
