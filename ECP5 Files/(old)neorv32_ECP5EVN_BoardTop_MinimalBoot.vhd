-- #################################################################################################
-- # << NEORV32 - Example setup including the bootloader, for the ECP5EVN (c) Board >>             #
-- # ********************************************************************************************* #
-- # BSD 3-Clause License                                                                          #
-- #                                                                                               #
-- # Copyright (c) 2023, Stephan Nolting. All rights reserved.                                     #
-- #                                                                                               #
-- # Redistribution and use in source and binary forms, with or without modification, are          #
-- # permitted provided that the following conditions are met:                                     #
-- #                                                                                               #
-- # 1. Redistributions of source code must retain the above copyright notice, this list of        #
-- #    conditions and the following disclaimer.                                                   #
-- #                                                                                               #
-- # 2. Redistributions in binary form must reproduce the above copyright notice, this list of     #
-- #    conditions and the following disclaimer in the documentation and/or other materials        #
-- #    provided with the distribution.                                                            #
-- #                                                                                               #
-- # 3. Neither the name of the copyright holder nor the names of its contributors may be used to  #
-- #    endorse or promote products derived from this software without specific prior written      #
-- #    permission.                                                                                #
-- #                                                                                               #
-- # THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS   #
-- # OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF               #
-- # MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE    #
-- # COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,     #
-- # EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE #
-- # GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED    #
-- # AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     #
-- # NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED  #
-- # OF THE POSSIBILITY OF SUCH DAMAGE.                                                            #
-- # ********************************************************************************************* #
-- # The NEORV32 Processor - https://github.com/stnolting/neorv32              (c) Stephan Nolting #
-- #################################################################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library neorv32;
use neorv32.neorv32_package.all;


entity neorv32_ECP5EVN_BoardTop_MinimalBoot is
  port (
    -- Clock and Reset inputs
    ECP5EVN_CLK   : in  std_logic;
    ECP5EVN_RST_N : in  std_logic;
    -- LED outputs
    ECP5EVN_LED0 : out std_logic;
    ECP5EVN_LED1 : out std_logic;
    ECP5EVN_LED2 : out std_logic;
    ECP5EVN_LED3 : out std_logic;
    ECP5EVN_LED4 : out std_logic;
    ECP5EVN_LED5 : out std_logic;
    ECP5EVN_LED6 : out std_logic;
    ECP5EVN_LED7 : out std_logic;
    -- UART0
    ECP5EVN_RX : in  std_logic;
    ECP5EVN_TX : out std_logic;
    clk_200 : in std_logic  -- 200 MHz LVDS clock (P pin)
  );
end entity;

architecture neorv32_ECP5EVN_BoardTop_MinimalBoot_rtl of neorv32_ECP5EVN_BoardTop_MinimalBoot is

  -- configuration --
  -- clock frequency in Hz (now 100 MHz system clock)
  constant f_clock_c : natural := 12_000_000;

  -- internal IO connection --
  signal con_pwm    : std_ulogic_vector(2 downto 0);
  signal con_gpio_o : std_ulogic_vector(3 downto 0);

  -- PLL and clocking --
  --signal clk_sys    : std_logic;  -- 100 MHz system clock from PLL
  --signal pll_locked : std_logic;
  --signal rstn_sync  : std_logic;
--signal clk_200 : std_logic;
--signal clk_100 : std_logic := '0';
signal clk_sys : std_logic;

begin
--u_pll_3 : entity work.pll_3
--  port map (
--    pll_neorv32_inst_CLKI  => clk_200,  -- 200 MHz LVDS from Y19/W20
--    pll_neorv32_inst_CLKOP => clk_sys   -- PLL output clock
--  );

--process(clk_200)
--begin
--  if rising_edge(clk_200) then
--    clk_100 <= not clk_100;
--  end if;
--end process;

-- combine external reset with PLL lock
  -- rstn_sync <= ECP5EVN_RST_N and pll_locked;

  -- The core of the problem ----------------------------------------------------------------
  -- ----------------------------------------------------------------------------------------
  neorv32_inst: entity neorv32.neorv32_ProcessorTop_MinimalBoot
  generic map (
    CLOCK_FREQUENCY => f_clock_c, -- clock frequency of clk_i in Hz (100 MHz)
    IMEM_SIZE       => 64*1024,
    DMEM_SIZE       => 64*1024
  )
  port map (
    -- Global control --
    clk_i  => std_ulogic(ECP5EVN_CLK),
    rstn_i => std_ulogic(ECP5EVN_RST_N),

    -- GPIO --
    --gpio_o     => con_gpio_o,

    -- primary UART --
    uart_txd_o => ECP5EVN_TX, -- UART0 send data
    uart_rxd_i => ECP5EVN_RX -- UART0 receive data

    -- PWM (to on-board RGB LED) --
    --pwm_o      => con_pwm
  );

  -- IO Connection --------------------------------------------------------------------------
  -- ----------------------------------------------------------------------------------------
  ECP5EVN_LED0 <= con_gpio_o(0);
  ECP5EVN_LED1 <= con_gpio_o(1);
  ECP5EVN_LED2 <= con_gpio_o(2);
  ECP5EVN_LED3 <= con_gpio_o(3);
  ECP5EVN_LED4 <= '0';        -- unused
  ECP5EVN_LED5 <= con_pwm(0);
  ECP5EVN_LED6 <= con_pwm(1);
  ECP5EVN_LED7 <= con_pwm(2);

end architecture;
