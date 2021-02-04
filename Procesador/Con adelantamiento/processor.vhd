library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity processor is
port(
    Clk         : in  std_logic;
    Reset       : in  std_logic;
    -- Memoria de Instruccion (Instruction memory)
    I_Addr      : out std_logic_vector(31 downto 0);
    I_RdStb     : out std_logic;
    I_WrStb     : out std_logic;
    I_DataOut   : out std_logic_vector(31 downto 0);
    I_DataIn    : in  std_logic_vector(31 downto 0);
    -- Memoria de Datos (Data memory)
    D_Addr      : out std_logic_vector(31 downto 0);
    D_RdStb     : out std_logic;
    D_WrStb     : out std_logic;
    D_DataOut   : out std_logic_vector(31 downto 0);
    D_DataIn    : in  std_logic_vector(31 downto 0)
);
end processor;

architecture processor_arq of processor is 

-- DECLARACION DE COMPONENTES

-- COMPONENTES DE LA ETAPA ID 

    component Registers
        port(clk     : in  STD_LOGIC;
            reset    : in  STD_LOGIC;
            wr       : in  STD_LOGIC;
            reg1_rd  : in  STD_LOGIC_VECTOR (4  downto 0);
            reg2_rd  : in  STD_LOGIC_VECTOR (4  downto 0);
            reg_wr   : in  STD_LOGIC_VECTOR (4  downto 0);
            data_wr  : in  STD_LOGIC_VECTOR (31 downto 0);
            data1_rd : out STD_LOGIC_VECTOR (31 downto 0);
            data2_rd : out STD_LOGIC_VECTOR (31 downto 0)
        );
    end component;
    
-- COMPONENTES DE LA ETAPA EX

    COMPONENT ALU 
    Port(a      : in  STD_LOGIC_VECTOR (31 downto 0);
        b       : in  STD_LOGIC_VECTOR (31 downto 0);
        control : in  STD_LOGIC_VECTOR (2  downto 0);
        result  : out STD_LOGIC_VECTOR (31 downto 0);
        zero    : out STD_LOGIC);
end COMPONENT;
   
-- DECLARACION DE SEÑALES

-- SEÑALES DE LA ESTAPA IF

    SIGNAL MUX_PC_IN    : STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL IF_PC        : STD_LOGIC_VECTOR (31 DOWNTO 0); 
    SIGNAL IF_PC_SIG    : STD_LOGIC_VECTOR (31 DOWNTO 0); -- ENTRADA DEL REG DE SEGMENTACION IF/ID Y ENTRADA 0 DEL MUX
    SIGNAL AND_MEM_PCSRC: STD_LOGIC;                      -- CONTROL DEL MUX 
    SIGNAL EX_MEM_IF_MUX: STD_LOGIC_VECTOR (31 DOWNTO 0); -- ENTRADA 1 DEL MUX

-- SEÑALES DEL REGISTRO DE SEGMENTACION IF/ID

    SIGNAL IFID_PC_SIG  : STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL IFID_I_DataIn: STD_LOGIC_VECTOR (31 DOWNTO 0);
    
-- SEÑALES DE LA ETADA ID 

    SIGNAL ID_PCSIG     : STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL ID_I_DataIn  : STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL ID_READ_DATA1: STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL ID_READ_DATA2: STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL ID_ALU_CTRL  : STD_LOGIC_VECTOR (31 DOWNTO 0);
    
    SIGNAL ID_REGWRITE : STD_LOGIC;  -- SEÑALES DE CONTROL
    SIGNAL ID_MEMTOREG : STD_LOGIC;
    SIGNAL ID_BRANCH   : STD_LOGIC;
    SIGNAL ID_MEMREAD  : STD_LOGIC;
    SIGNAL ID_MEMWRITE : STD_LOGIC;
    SIGNAL ID_REGDST   : STD_LOGIC;
    SIGNAL ID_ALUOP    : STD_LOGIC_VECTOR (1 DOWNTO 0);
    SIGNAL ID_ALUSRC   : STD_LOGIC;

-- SEÑALES DEL REGISTRO DE SEGMENTACION ID/EX

    SIGNAL ID_EX_PCSIG      : STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL ID_EX_READ_DATA1 : STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL ID_EX_READ_DATA2 : STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL ID_EX_ALU_CTRL   : STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL ID_EX_RT         : STD_LOGIC_VECTOR (4  DOWNTO 0);
    SIGNAL ID_EX_RD         : STD_LOGIC_VECTOR (4  DOWNTO 0);
    
    SIGNAL ID_EX_REGWRITE : STD_LOGIC;  -- SEÑALES DE CONTROL EN EL REGISTRO DE SEGMENTACION
    SIGNAL ID_EX_MEMTOREG : STD_LOGIC;
    SIGNAL ID_EX_BRANCH   : STD_LOGIC;
    SIGNAL ID_EX_MEMREAD  : STD_LOGIC;
    SIGNAL ID_EX_MEMWRITE : STD_LOGIC;
    SIGNAL ID_EX_REGDST   : STD_LOGIC;
    SIGNAL ID_EX_ALUOP    : STD_LOGIC_VECTOR (1 DOWNTO 0);
    SIGNAL ID_EX_ALUSRC   : STD_LOGIC;
    
    SIGNAL ID_EX_I_DataIn : STD_LOGIC_VECTOR (4 DOWNTO 0); -- SEÑAL PARA LA UNIDAD DE ADELANTAMIENTO

-- SEÑALES DE LA ETAPA EX
   
    SIGNAL EX_PCSIG      : STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL EX_SHIFT      : STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL EX_ADD_RESULT : STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL EX_ALU_CTRL   : STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL EX_RT         : STD_LOGIC_VECTOR (4  DOWNTO 0);
    SIGNAL EX_RD         : STD_LOGIC_VECTOR (4  DOWNTO 0);
    SIGNAL EX_REGDST     : STD_LOGIC;
    SIGNAL EX_MUX2       : STD_LOGIC_VECTOR (4  DOWNTO 0);
    SIGNAL EX_DATA1      : STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL EX_DATA2      : STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL EX_ALUSRC     : STD_LOGIC;
    SIGNAL EX_MUX1       : STD_LOGIC_VECTOR (31 DOWNTO 0);
    
    SIGNAL EX_REGWRITE  : STD_LOGIC;  -- SEÑALES DE CONTROL EN EN LA ETAPA EX
    SIGNAL EX_MEMTOREG  : STD_LOGIC;
    SIGNAL EX_BRANCH    : STD_LOGIC;
    SIGNAL EX_MEMREAD   : STD_LOGIC;
    SIGNAL EX_MEMWRITE  : STD_LOGIC;
    SIGNAL EX_ALUOP     : STD_LOGIC_VECTOR (1 DOWNTO 0);
    SIGNAL EX_ALU_CTRL2 : STD_LOGIC_VECTOR (2 DOWNTO 0);
    
    SIGNAL EX_ALU_RESULT : STD_LOGIC_VECTOR (31 DOWNTO 0); -- SALIDAS DE LA ALU
    SIGNAL EX_ALU_ZERO   : STD_LOGIC;
    
    SIGNAL EX_I_DataIn   : STD_LOGIC_VECTOR (4 DOWNTO 0);  -- SEÑAL ENTRADA DE LA UNIDAD DE ADELANTAMIENTO  
    SIGNAL EX_CTRL_ALU_A : STD_LOGIC_VECTOR (1 DOWNTO 0);  -- SEÑALES DE LOS MUX PARA ADELANTAMIENTO
    SIGNAL EX_CTRL_ALU_B : STD_LOGIC_VECTOR (1 DOWNTO 0);
    SIGNAL EX_MUX_A      : STD_LOGIC_VECTOR (31 DOWNTO 0);  -- SEÑAL SALIDA DEL MUX_A ENTRADA A DE LA ALU
    SIGNAL EX_MUX_B_RT   : STD_LOGIC_VECTOR (31 DOWNTO 0);  -- SEÑAL SALIDA DEL MUX_B ENTRADA DEL MUX1
    
-- SEÑALES DEL REGISTRO DE SEGMENTACION EX/MEM

    SIGNAL EX_MEM_REGWRITE : STD_LOGIC;  -- SEÑALES DE CONTROL EN EL REGISTRO DE SEGMENTACION
    SIGNAL EX_MEM_MEMTOREG : STD_LOGIC;
    SIGNAL EX_MEM_BRANCH   : STD_LOGIC;
    SIGNAL EX_MEM_MEMREAD  : STD_LOGIC;
    SIGNAL EX_MEM_MEMWRITE : STD_LOGIC;
    
    SIGNAL EX_MEM_ADD_RESULT : STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL EX_MEM_ALU_ZERO   : STD_LOGIC;
    SIGNAL EX_MEM_ALU_RESULT : STD_LOGIC_VECTOR (31 DOWNTO 0); -- SALIDAS DE LA ALU
        
    SIGNAL EX_MEM_DATA2 : STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL EX_MEM_MUX2  : STD_LOGIC_VECTOR (4  DOWNTO 0);

-- SEÑALES DE LA ETAPA MEM

    SIGNAL MEM_REGWRITE : STD_LOGIC;  -- SEÑALES DE CONTROL EN EL REGISTRO DE SEGMENTACION
    SIGNAL MEM_MEMTOREG : STD_LOGIC;
    SIGNAL MEM_BRANCH   : STD_LOGIC;
    SIGNAL MEM_ALU_ZERO : STD_LOGIC;
    SIGNAL MEM_MUX2     : STD_LOGIC_VECTOR (4 DOWNTO 0);
    
    SIGNAL MEM_MEMREAD  : STD_LOGIC;
    SIGNAL MEM_MEMWRITE : STD_LOGIC;
    
    SIGNAL MEM_ALU_RESULT : STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL MEM_DATA2      : STD_LOGIC_VECTOR (31 DOWNTO 0);
    
    SIGNAL MEM_READ_DATA : STD_LOGIC_VECTOR (31 DOWNTO 0);

-- SEÑALES DEL REGISTRO DE SEGMENTACION EX/MEM

    SIGNAL MEM_WB_REGWRITE   : STD_LOGIC;  -- SEÑALES DE CONTROL DESDE REGISTRO DE SEGMENTACION
    SIGNAL MEM_WB_MEMTOREG   : STD_LOGIC;
    SIGNAL MEM_WB_READ_DATA  : STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL MEM_WB_ALU_RESULT : STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL MEM_WB_MUX2       : STD_LOGIC_VECTOR (4  DOWNTO 0);

-- SEÑALES DE LA ETAPA WB

    SIGNAL WB_MEMTOREG   : STD_LOGIC;
    SIGNAL WB_REGWRITE   : STD_LOGIC;
    SIGNAL WB_MUX2       : STD_LOGIC_VECTOR (4  DOWNTO 0);
    SIGNAL WB_MUX1       : STD_LOGIC_VECTOR (31 DOWNTO 0);    
    SIGNAL WB_READ_DATA  : STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL WB_ALU_RESULT : STD_LOGIC_VECTOR (31 DOWNTO 0);

begin

    -- ETAPA IF 
    
    PROCESS(AND_MEM_PCSRC, IF_PC_SIG, EX_MEM_IF_MUX) -- MUX ETAPA IF
    BEGIN
        IF (AND_MEM_PCSRC = '1') THEN
            MUX_PC_IN <= EX_MEM_IF_MUX;
        ELSE
            MUX_PC_IN <= IF_PC_SIG;
            END IF;
    END PROCESS;
    
    PROCESS(Clk, Reset) -- GENERACION DE PC
    BEGIN
        IF (Reset = '1') THEN
            IF_PC <= X"00000000";
        ELSIF (RISING_EDGE(Clk)) THEN
            IF_PC <= MUX_PC_IN;
        END IF;
    END PROCESS;
     
    IF_PC_SIG <= IF_PC + X"00000004"; -- INCREMENTAR PC - PC = PC + 4
     
    I_Addr    <= IF_PC;
    I_RdStb   <= '1';
    I_WrStb   <= '0';
    I_DataOut <= (OTHERS => '0');
    -- APARTIR DEL PC EN I_Addr POR I_DataIn tengo el dato necesario;
    
    --  RESGISTRO DE SEGMENTACION IF/ID
    
    PROCESS(Clk, Reset)
    BEGIN
        IF (Reset ='1') THEN
            IFID_PC_SIG   <= x"00000000";
            IFID_I_DataIn <= x"00000000";
        ELSIF (RISING_EDGE(Clk)) THEN
            IFID_PC_SIG   <= IF_PC_SIG;
            IFID_I_DataIn <= I_DataIn;
        END IF; 
    END PROCESS;
    
    -- ETAPA ID
    
    ID_PCSIG    <= IFID_PC_SIG;
    ID_I_DataIn <= IFID_I_DataIn;
    ID_REGISTERS : Registers PORT MAP(Clk, Reset, WB_REGWRITE, ID_I_DataIn(25 DOWNTO 21), ID_I_DataIn(20 DOWNTO 16), WB_MUX2, WB_MUX1, ID_READ_DATA1, ID_READ_DATA2);
    
    PROCESS(ID_I_DataIn) -- EXTENSION DE SIGNO
    BEGIN
        IF(ID_I_DataIn(15) = '0') THEN
            ID_ALU_CTRL <= x"0000" & ID_I_DataIn(15 DOWNTO 0);
        ELSE
            ID_ALU_CTRL <= x"FFFF" & ID_I_DataIn(15 DOWNTO 0);
        END IF;
    END PROCESS;
    
    PROCESS(ID_I_DataIn) -- CONTROL DE LA ETAPA ID 
    BEGIN
        CASE ID_I_DataIn(31 DOWNTO 26) IS
            WHEN "000000" => ID_REGWRITE <= '1'; --REG A REG
                             ID_MEMTOREG <= '0';
                             ID_BRANCH   <= '0';
                             ID_MEMREAD  <= '0';
                             ID_MEMWRITE <= '0';
                             ID_REGDST   <=  '1';
                             ID_ALUOP    <=  "10";
                             ID_ALUSRC   <=  '0';
                            
            WHEN "100011" => ID_REGWRITE <= '1'; --LOAD
                             ID_MEMTOREG <= '1';
                             ID_BRANCH   <= '0';
                             ID_MEMREAD  <= '1';
                             ID_MEMWRITE <='0';
                             ID_REGDST   <= '0';
                             ID_ALUOP    <= "00";
                             ID_ALUSRC   <= '1';
                              
            WHEN "101011" => ID_REGWRITE <='0'; --STORE
                             ID_MEMTOREG <= '0';
                             ID_BRANCH   <= '0';
                             ID_MEMREAD  <= '0';
                             ID_MEMWRITE <= '1';
                             ID_REGDST   <= '0';
                             ID_ALUOP    <= "00";
                             ID_ALUSRC   <= '1';
                             
           WHEN "001111" =>  ID_REGWRITE <= '1'; --LUI   NO SE QUE PONER EN LOS CONTROLES 
                             ID_MEMTOREG <= '0';
                             ID_BRANCH   <= '0';
                             ID_MEMREAD  <= '0';
                             ID_MEMWRITE <= '0';
                             ID_REGDST   <= '0';
                             ID_ALUOP    <= "11";
                             ID_ALUSRC   <= '1';
                             
            WHEN "000100" => ID_REGWRITE <= '0'; --BEQ
                             ID_MEMTOREG <= '0';
                             ID_BRANCH   <='1';
                             ID_MEMREAD  <='0';
                             ID_MEMWRITE <= '0';
                             ID_REGDST   <= '0';
                             ID_ALUOP    <= "01";
                             ID_ALUSRC   <= '0';
                             
            WHEN OTHERS =>   ID_REGWRITE <= '0';
                             ID_MEMTOREG <= '0';
                             ID_BRANCH   <='0';
                             ID_MEMREAD  <='0';
                             ID_MEMWRITE <= '0';
                             ID_REGDST   <= '0';
                             ID_ALUOP    <= "00";
                             ID_ALUSRC   <= '0';
            END CASE;
    END PROCESS;
    
--  RESGISTRO DE SEGMENTACION ID/EX
    
    PROCESS(Reset, Clk)
    BEGIN
        IF(Reset = '1')THEN
            ID_EX_PCSIG      <= x"00000000";
            ID_EX_READ_DATA1 <= x"00000000";
            ID_EX_READ_DATA2 <= x"00000000";
            ID_EX_ALU_CTRL   <= x"00000000";
            ID_EX_RT         <= "00000";
            ID_EX_RD         <= "00000";
                         
            ID_EX_REGWRITE <= '0';  -- SEÑALES DE CONTROL DE ETAPA ID A REGISTRI ID/EX
            ID_EX_MEMTOREG <= '0';
            ID_EX_BRANCH   <= '0';
            ID_EX_MEMREAD  <= '0';
            ID_EX_MEMWRITE <= '0';
            ID_EX_REGDST   <= '0';
            ID_EX_ALUOP    <= "00";
            ID_EX_ALUSRC   <= '0';
            
            ID_EX_I_DataIn <= "00000"; -- SEÑAL DE LA UNIDAD DE ADELANTAMIENTO
              
        ELSIF (RISING_EDGE(Clk))THEN
            ID_EX_PCSIG      <= ID_PCSIG;
            ID_EX_READ_DATA1 <= ID_READ_DATA1;
            ID_EX_READ_DATA2 <= ID_READ_DATA2;
            ID_EX_ALU_CTRL   <= ID_ALU_CTRL;
            ID_EX_RT         <= ID_I_DataIn(20 DOWNTO 16);
            ID_EX_RD         <= ID_I_DataIn(15 DOWNTO 11);
                    
            ID_EX_REGWRITE <= ID_REGWRITE;  -- SEÑALES DE CONTROL DE ETAPA ID A REGISTRI ID/EX
            ID_EX_MEMTOREG <= ID_MEMTOREG;
            ID_EX_BRANCH   <= ID_BRANCH;
            ID_EX_MEMREAD  <= ID_MEMREAD;
            ID_EX_MEMWRITE <= ID_MEMWRITE;
            ID_EX_REGDST   <= ID_REGDST;
            ID_EX_ALUOP    <= ID_ALUOP;
            ID_EX_ALUSRC   <= ID_ALUSRC;
            
            ID_EX_I_DataIn <= ID_I_DataIn(25 DOWNTO 21); -- SEÑAL DE LA UNIDAD DE ADELANTAMIENTO
        END IF;
    END PROCESS;
    
-- ETAPA EX
    
    EX_ALUSRC   <= ID_EX_ALUSRC; 
    EX_DATA1    <= ID_EX_READ_DATA1;
    EX_DATA2    <= ID_EX_READ_DATA2;
    EX_PCSIG    <= ID_EX_PCSIG;
    EX_SHIFT    <= EX_ALU_CTRL(29 DOWNTO 0) & "00"; -- SHIFT LEFT 2
    EX_ALU_CTRL <= ID_EX_ALU_CTRL;
    EX_RT       <= ID_EX_RT;
    EX_RD       <= ID_EX_RD;
    EX_REGDST   <= ID_EX_REGDST;
    
    EX_REGWRITE <= ID_EX_REGWRITE;  -- PASAJE DE SEÑALES DE CONTROL DESDE EL REGISTRO DE SEGMENTACION HACIA LA ETAPA EX
    EX_MEMTOREG <= ID_EX_MEMTOREG;
    EX_BRANCH   <= ID_EX_BRANCH;
    EX_MEMREAD  <= ID_EX_MEMREAD;
    EX_MEMWRITE <= ID_EX_MEMWRITE;
    EX_ALUOP    <= ID_EX_ALUOP;
    
    EX_I_DataIn <= ID_EX_I_DataIn;  -- SEÑAL ENTRADA DE LA UNIDAD DE ADELANTAMIENTO
    
    PROCESS(EX_PCSIG, EX_SHIFT) -- ADD ETAPA EX, SIGUIENTE DIRECCION
    BEGIN
        EX_ADD_RESULT <= EX_PCSIG + EX_SHIFT;
    END PROCESS;

    PROCESS(EX_REGDST, EX_RT, EX_RD) -- MUX2 ETAPA EX
    BEGIN
        IF (EX_REGDST = '1') THEN
            EX_MUX2 <= EX_RD;
        ELSE
            EX_MUX2 <= EX_RT;
        END IF;
    END PROCESS;
    
    PROCESS(EX_ALU_CTRL, EX_ALUOP) -- ALU CONTROL
    BEGIN
        IF (EX_ALUOP = "10") THEN -- OPERACIONES TIPO-R
            IF (EX_ALU_CTRL(5 DOWNTO 0) = "100000") THEN
                EX_ALU_CTRL2 <= "010";  -- SUMA
            ELSIF (EX_ALU_CTRL(5 DOWNTO 0) = "100010") THEN
                EX_ALU_CTRL2 <= "110";  -- RESTA
            ELSIF (EX_ALU_CTRL(5 DOWNTO 0) = "100100") THEN
                EX_ALU_CTRL2 <= "000"; -- AND
            ELSIF (EX_ALU_CTRL(5 DOWNTO 0) = "100101") THEN
                EX_ALU_CTRL2 <= "001"; -- OR
            ELSIF (EX_ALU_CTRL(5 DOWNTO 0) = "101010") THEN
                EX_ALU_CTRL2 <= "111"; -- RS < RT
            else
                EX_ALU_CTRL2 <= "000"; -- nunca entra, funciona como el when others del case
            END IF;
        ELSIF (EX_ALUOP = "00") THEN  -- LW Y SW
            EX_ALU_CTRL2 <= "010";  -- SUMA
        ELSIF (EX_ALUOP = "01") THEN -- BEQ
            EX_ALU_CTRL2 <= "010";  -- SUMA
        ELSIF (EX_ALUOP = "11") THEN -- LUI
            EX_ALU_CTRL2 <= "100";  -- SHIF-LEFT (<<16)
        ELSE 
            EX_ALU_CTRL2 <= "000"; -- nunca entra, funciona como el when others del case
        END IF;        
    END PROCESS;
    
    EX_ALU : ALU PORT MAP(EX_MUX_A, EX_MUX1, EX_ALU_CTRL2, EX_ALU_RESULT, EX_ALU_ZERO); -- INSTANCIACION DE LA ALU
    
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    PROCESS (ID_EX_I_DataIn, EX_RT, WB_MUX2, MEM_MUX2, WB_REGWRITE, MEM_REGWRITE) -- PROCESO PARA LA UNIDAD DE ADELANTAMIENTO
    BEGIN
        IF (WB_REGWRITE = '1' AND WB_MUX2 /= "00000") THEN
            IF ((MEM_MUX2 /= ID_EX_I_DataIn) AND (WB_MUX2 = ID_EX_I_DataIn)) THEN
                EX_CTRL_ALU_A <= "01";
            ELSE
                EX_CTRL_ALU_A <= "00";
            END IF;
            IF (MEM_MUX2 = EX_RT) THEN
                EX_CTRL_ALU_B <= "01";
            ELSE
                EX_CTRL_ALU_B <= "00";
           END IF;
        END IF; 
        IF (MEM_REGWRITE = '1' AND MEM_MUX2 /= "00000") THEN
            IF (MEM_MUX2 = ID_EX_I_DataIn) THEN
                EX_CTRL_ALU_A <= "10";
            ELSE
                EX_CTRL_ALU_A <= "00";
            END IF;
            IF (MEM_MUX2 = EX_RT) THEN
                EX_CTRL_ALU_B <= "10";
            ELSE
                EX_CTRL_ALU_B <= "00";
            END IF;
        END IF;
    END PROCESS;
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    PROCESS (EX_DATA1, WB_MUX1, MEM_ALU_RESULT, EX_CTRL_ALU_A)  -- MULTIPLEXOR A DE ADELANTAMIENTO
    BEGIN
        IF (EX_CTRL_ALU_A = "00") THEN
            EX_MUX_A <= EX_DATA1;
        ELSIF (EX_CTRL_ALU_A = "00") THEN
            EX_MUX_A <= WB_MUX1;
        ELSIF (EX_CTRL_ALU_A = "00") THEN
            EX_MUX_A <= MEM_ALU_RESULT;
        END IF;
    END PROCESS;
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    PROCESS (EX_ALUSRC, EX_MUX_B_RT, EX_ALU_CTRL)  -- MULTIPLEXOR MODIFICADO PARA PARA ADELANTAMIENTO DEJA PASA EL INMEDATO O EL REGSITRO QUE VIENE DEL MUX_B
    BEGIN
        IF (EX_ALUSRC = '1') THEN
            EX_MUX1 <= EX_ALU_CTRL;
        ELSE
            EX_MUX1 <= EX_MUX_B_RT;
        END IF;
    END PROCESS;
    
    --EX_MUX1 <= EX_ALU_CTRL WHEN (EX_ALUSRC = '1')  -- MULTIPLEXOR QUE HAY QUE MODIFICAR PARA AGREGAR FORWARDING   -- SIN FORWARDING
    --ELSE EX_DATA2;
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
   
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    PROCESS (EX_CTRL_ALU_B, MEM_ALU_RESULT, EX_DATA2) -- MULTIPLEXOR B DE ADELANTAMIENTO
    BEGIN
        IF (EX_CTRL_ALU_B = "00") THEN
            EX_MUX_B_RT <= EX_DATA2;
        ELSIF (EX_CTRL_ALU_B = "01") THEN
            EX_MUX_B_RT <= WB_MUX1;
        ELSE                              --IF  (EX_CTRL_ALU_B = "10" ) THEN--
            EX_MUX_B_RT <= MEM_ALU_RESULT;
        END IF;
    END PROCESS;
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

    
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
--  RESGISTRO DE SEGMENTACION EX/MEM
    
    PROCESS(Reset, Clk)
    BEGIN
        IF(Reset = '1')THEN
            EX_MEM_REGWRITE <= '0';  -- SEÑALES DE CONTROL EN EL REGISTRO DE SEGMENTACION
            EX_MEM_MEMTOREG <= '0';
            EX_MEM_BRANCH   <= '0';
            EX_MEM_MEMREAD  <= '0';
            EX_MEM_MEMWRITE <= '0';
                   
            EX_MEM_ADD_RESULT <= X"00000000"; -- SALIDAS DE LA ALU
            EX_MEM_ALU_ZERO   <= '0';
            EX_MEM_ALU_RESULT <= X"00000000"; 
                      
            EX_MEM_DATA2 <= X"00000000";
            EX_MEM_MUX2  <= "00000";
              
        ELSIF (RISING_EDGE(Clk))THEN
            EX_MEM_REGWRITE <= EX_REGWRITE;  -- SEÑALES DE CONTROL EN EL REGISTRO DE SEGMENTACION
            EX_MEM_MEMTOREG <= EX_MEMTOREG;
            EX_MEM_BRANCH   <= EX_BRANCH;
            EX_MEM_MEMREAD  <= EX_MEMREAD;
            EX_MEM_MEMWRITE <= EX_MEMWRITE;
           
            EX_MEM_ADD_RESULT <= EX_ADD_RESULT; -- SALIDAS DE LA ALU
            EX_MEM_ALU_ZERO   <= EX_ALU_ZERO;
            EX_MEM_ALU_RESULT <= EX_ALU_RESULT; 
               
            EX_MEM_DATA2 <=  EX_MUX_B_RT; -- DATO ADELANTADO AL REGISTRO DE SEGMENTCION EX/MEM
            EX_MEM_MUX2  <= EX_MUX2;
        END IF;
    END PROCESS;    

-- ETAPA MEM
    
    MEM_REGWRITE <= EX_MEM_REGWRITE;  -- SEÑALES DE CONTROL DESDE REGISTRO DE SEGMENTACION
    MEM_MEMTOREG <= EX_MEM_MEMTOREG;
    MEM_ALU_ZERO <= EX_MEM_ALU_ZERO;
    MEM_BRANCH   <= EX_MEM_BRANCH;
    MEM_MUX2     <= EX_MEM_MUX2;
    
    AND_MEM_PCSRC <= MEM_BRANCH AND MEM_ALU_ZERO; -- AND PARA SABER SI SALTA O NO
    EX_MEM_IF_MUX <= EX_MEM_ADD_RESULT; -- SIGUIENTE DIRECCION EN EL MUX D ELA ETAPA IF
    
    MEM_MEMREAD  <= EX_MEM_MEMREAD;
    MEM_MEMWRITE <= EX_MEM_MEMWRITE;
    
    MEM_ALU_RESULT <= EX_MEM_ALU_RESULT;
    MEM_DATA2      <= EX_MEM_DATA2;
    
    -- MEMORIA DE DATOS
    
    D_Addr        <= MEM_ALU_RESULT;
    D_RdStb       <= MEM_MEMREAD; 
    D_WrStb       <= MEM_MEMWRITE; 
    D_DataOut     <= MEM_DATA2;
    MEM_READ_DATA <= D_DataIn;
     
--  RESGISTRO DE SEGMENTACION MEM/WB
        
        PROCESS(Reset, Clk)
        BEGIN
            IF(Reset = '1')THEN
                MEM_WB_REGWRITE   <= '0';  -- SEÑALES DE CONTROL DESDE REGISTRO DE SEGMENTACION
                MEM_WB_MEMTOREG   <= '0';
                MEM_WB_READ_DATA  <= X"00000000";
                MEM_WB_ALU_RESULT <= X"00000000";
                MEM_WB_MUX2       <= "00000";
            ELSIF (RISING_EDGE(Clk))THEN
                MEM_WB_REGWRITE   <= MEM_REGWRITE;  -- SEÑALES DE CONTROL DESDE REGISTRO DE SEGMENTACION
                MEM_WB_MEMTOREG   <= MEM_MEMTOREG;
                MEM_WB_READ_DATA  <= MEM_READ_DATA;
                MEM_WB_ALU_RESULT <= MEM_ALU_RESULT;
                MEM_WB_MUX2       <= MEM_MUX2;
            END IF;
        END PROCESS;  
        
-- ETAPA WB

    WB_REGWRITE   <= MEM_WB_REGWRITE;  -- SEÑALES DE CONTROL DESDE REGISTRO DE SEGMENTACION
    WB_MEMTOREG   <= MEM_WB_MEMTOREG;
    WB_MUX2       <= MEM_WB_MUX2;
    WB_READ_DATA  <= MEM_WB_READ_DATA;
    WB_ALU_RESULT <= MEM_WB_ALU_RESULT;
    
    PROCESS (WB_READ_DATA, WB_ALU_RESULT, WB_MEMTOREG)
    BEGIN
        IF (WB_MEMTOREG = '1') THEN
            WB_MUX1 <= WB_READ_DATA;
        ELSE 
            WB_MUX1 <= WB_ALU_RESULT;
        END IF;
    END PROCESS;
    
end processor_arq;
