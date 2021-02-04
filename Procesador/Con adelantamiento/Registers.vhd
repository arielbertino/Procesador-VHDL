library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_arith.ALL;
use IEEE.STD_LOGIC_unsigned.ALL;

entity Registers is
    Port ( clk : in STD_LOGIC;
           reset : in STD_LOGIC;
           wr : in STD_LOGIC;
           reg1_rd : in STD_LOGIC_VECTOR (4 downto 0);
           reg2_rd : in STD_LOGIC_VECTOR (4 downto 0);
           reg_wr : in STD_LOGIC_VECTOR (4 downto 0);
           data_wr : in STD_LOGIC_VECTOR (31 downto 0);
           data1_rd : out STD_LOGIC_VECTOR (31 downto 0);
           data2_rd : out STD_LOGIC_VECTOR (31 downto 0));
end Registers;

architecture Behavioral of Registers is
    type ram_type is array (31 downto 0) of std_logic_vector (31 downto 0); -- 32 regs de 32 bits c/u
    signal regNro : ram_type;
begin
    
    process (clk)
    begin
        if (reset = '1') then  -- reset asincrono a nivel alto      
                for i in 0 to 31 loop
                    regNro(i) <= x"00000000";
                end loop;
        elsif (falling_edge(clk)) then  -- escritura en flaco descendente
            if (wr = '1') then
                -- escribe en la posicion reg_wr el dato data_wr
                regNro(conv_integer(reg_wr)) <= data_wr;
            end if;
        end if;
    end process;
    
    process(reg1_rd) -- proceso para acceder a reg2_rd -- lectura
    begin
        if (reg1_rd /= 0) then
            data1_rd <= regNro(conv_integer(reg1_rd));
        else
            data1_rd <= x"00000000";
        end if;
    end process;
    
    process(reg2_rd) -- proceso para acceder a reg2_rd -- lectura
        begin
            if (reg2_rd /= 0) then
                data2_rd <= regNro(conv_integer(reg2_rd));
            else
                data2_rd <= x"00000000";
            end if;
    end process;
end Behavioral;
