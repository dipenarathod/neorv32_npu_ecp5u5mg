-- ================================================================================ --
-- NEORV32 Templates - Minimal generic setup with the bootloader enabled            --
-- -------------------------------------------------------------------------------- --
-- The NEORV32 RISC-V Processor - https://github.com/stnolting/neorv32              --
-- Copyright (c) NEORV32 contributors.                                              --
-- Copyright (c) 2020 - 2025 Stephan Nolting. All rights reserved.                  --
-- Licensed under the BSD-3-Clause license, see LICENSE for details.                --
-- SPDX-License-Identifier: BSD-3-Clause                                            --
-- ================================================================================ --

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library neorv32;
use neorv32.neorv32_package.all;
entity neorv32_ProcessorTop_MinimalBoot is
  generic (
    -- Clocking --
    CLOCK_FREQUENCY : natural := 0;       -- clock frequency of clk_i in Hz
    -- Internal Instruction memory --
    IMEM_EN         : boolean := true;    -- implement processor-internal instruction memory
    IMEM_SIZE       : natural := 64*1024; -- size of processor-internal instruction memory in bytes
    -- Internal Data memory --
    DMEM_EN         : boolean := true;    -- implement processor-internal data memory
    DMEM_SIZE       : natural := 64*1024 -- size of processor-internal data memory in bytes
    -- Processor peripherals --
    --IO_GPIO_NUM     : natural := 4;       -- number of GPIO input/output pairs (0..32)
    --IO_PWM_NUM_CH   : natural := 3        -- number of PWM channels to implement (0..16)
  );
  port (
    -- Global control --
    clk_i      : in  std_logic;
    rstn_i     : in  std_logic;
    -- GPIO (available if IO_GPIO_EN = true) --
    --gpio_o     : out std_ulogic_vector(IO_GPIO_NUM-1 downto 0);
    -- primary UART0 (available if IO_UART0_EN = true) --
    uart_txd_o : out std_ulogic; -- UART0 send data
    uart_rxd_i : in  std_ulogic := '0'; -- UART0 receive data
    -- PWM (available if IO_PWM_NUM_CH > 0) --
    --pwm_o      : out std_ulogic_vector(IO_PWM_NUM_CH-1 downto 0)
		--Interface for the camera harware
		SIO_C      : Inout Std_ulogic; --SIO_C - SCCB clock signal. FPGA -> Camera
		SIO_D      : Inout Std_ulogic; --SIO_D - SCCB data signal (bi-direcctional). FPGA <--> Camera
		VSYNC      : In    Std_ulogic; --Camera VSYNC signal
		HREF       : In    Std_ulogic; --Camera HREF signal
		PCLK       : In    Std_ulogic; --Camera PCLK signal
		Data       : In    Std_ulogic_vector (7 Downto 0) --Camera data out pins
  );
end entity;

architecture neorv32_ProcessorTop_MinimalBoot_rtl of neorv32_ProcessorTop_MinimalBoot is

  -- internal IO connection --
  signal con_gpio_o : std_ulogic_vector(31 downto 0);
  signal con_pwm_o  : std_ulogic_vector(15 downto 0);

  signal rstn_internal:std_ulogic;  --internal signal to invert the reset signal
  
  
  --Interconnect component
  component wb_1m2s_interconnect Is
	Port(
		clk        : In  Std_ulogic; --system clock
		reset      : In  Std_ulogic; --synchronous reset
		--Master pins
		m_i_wb_cyc   : In  Std_ulogic; --Master Wishbone: cycle valid
		m_i_wb_stb   : In  Std_ulogic; --Master Wishbone: strobe
		m_i_wb_we    : In  Std_ulogic; --Master Wishbone: 1=write, 0=read
		m_i_wb_addr  : In  Std_ulogic_vector(31 Downto 0);--Master Wishbone: address
		m_i_wb_data  : In  Std_ulogic_vector(31 Downto 0);--Master Wishbone: write data
		m_o_wb_ack   : Out Std_ulogic; --Master Wishbone: acknowledge
		m_o_wb_stall : Out Std_ulogic; --Master Wishbone: stall (always '0')
		m_o_wb_data  : Out Std_ulogic_vector(31 Downto 0); --Master Wishbone: read data

		--S0 pins. Peripheral pin directions are inverted compared to master
		s0_o_wb_cyc   : Out  Std_ulogic; --S0 Wishbone: cycle valid
		s0_o_wb_stb   : Out  Std_ulogic; --S0 Wishbone: strobe
		s0_o_wb_we    : Out  Std_ulogic; --S0 Wishbone: 1=write, 0=read
		s0_o_wb_addr  : Out  Std_ulogic_vector(31 Downto 0);--S0 Wishbone: address
		s0_o_wb_data  : Out  Std_ulogic_vector(31 Downto 0);--S0 Wishbone: write data
		s0_i_wb_ack   : In Std_ulogic; --S0 Wishbone: acknowledge
		s0_i_wb_stall : In Std_ulogic; --S0 Wishbone: stall (always '0')
		s0_i_wb_data  : In Std_ulogic_vector(31 Downto 0); --S0 Wishbone: read data

		--S1 pins
		s1_o_wb_cyc   : Out  Std_ulogic; --S1 Wishbone: cycle valid
		s1_o_wb_stb   : Out  Std_ulogic; --S1 Wishbone: strobe
		s1_o_wb_we    : Out  Std_ulogic; --S1 Wishbone: 1=write, 0=read
		s1_o_wb_addr  : Out  Std_ulogic_vector(31 Downto 0);--S1 Wishbone: address
		s1_o_wb_data  : Out  Std_ulogic_vector(31 Downto 0);--S1 Wishbone: write data
		s1_i_wb_ack   : In Std_ulogic; --S1 Wishbone: acknowledge
		s1_i_wb_stall : In Std_ulogic; --S1 Wishbone: stall (always '0')
		s1_i_wb_data  : In Std_ulogic_vector(31 Downto 0) --S1 Wishbone: read data
	);
	End component;
  
    --NPU declaration
  component wb_peripheral_top
    generic (
      BASE_ADDRESS    : std_ulogic_vector(31 downto 0) := x"90000000"
    );
    port (
      clk        : in  std_ulogic;
      reset      : in  std_ulogic;
      i_wb_cyc   : in  std_ulogic;
      i_wb_stb   : in  std_ulogic;
      i_wb_we    : in  std_ulogic;
      i_wb_addr  : in  std_ulogic_vector(31 downto 0);
      i_wb_data  : in  std_ulogic_vector(31 downto 0);
      o_wb_ack   : out std_ulogic;
      o_wb_stall : out std_ulogic;
      o_wb_data  : out std_ulogic_vector(31 downto 0)
      --buttons    : in  std_ulogic_vector(2 downto 0);
      --leds       : out std_ulogic_vector(7 downto 0)
    );
    end component;

	--Camera component
	Component wb_ov5640 Is
		Generic (
			BASE_ADDRESS                 : Std_ulogic_vector(31 Downto 0) := x"90010000"; --peripheral base (informational)
			CAMERA_CONTROL_ADDRESS       : Std_ulogic_vector(31 Downto 0) := x"90010000"; --Camera control register. [0] = enable, [1] = reset
			CAMERA_STATUS_ADDRESS        : Std_ulogic_vector(31 Downto 0) := x"90010004"; --Camera status register. [0]=busy, [1]=done (sticky)
			IMAGE_FORMAT_ADDRESS         : Std_ulogic_vector(31 Downto 0) := x"90010008"; --Image format. [0] = 1 for YUV422. (Lowest3 bits can be used to select the format)
			IMAGE_RESOLUTION_ADDRESS     : Std_ulogic_vector(31 Downto 0) := x"9001000C"; --[15:0] = image width. [31:16] = image height
			MASTER_WORDS_TO_READ_ADDRESS : Std_ulogic_vector(31 Downto 0) := x"90010010"; --32-bit words the master has to read to gather the complete image
			IMAGE_BUFFER_BASE            : Std_ulogic_vector(31 Downto 0) := x"90011000" --Image buffer base address
		);
		Port (
			clk        : In    Std_ulogic; --system clock
			reset      : In    Std_ulogic; --synchronous reset
			i_wb_cyc   : In    Std_ulogic; --Wishbone: cycle valid
			i_wb_stb   : In    Std_ulogic; --Wishbone: strobe
			i_wb_we    : In    Std_ulogic; --Wishbone: 1=write, 0=read
			i_wb_addr  : In    Std_ulogic_vector(31 Downto 0);--Wishbone: address
			i_wb_data  : In    Std_ulogic_vector(31 Downto 0);--Wishbone: write data
			o_wb_ack   : Out   Std_ulogic; --Wishbone: acknowledge
			o_wb_stall : Out   Std_ulogic; --Wishbone: stall (always '0')
			o_wb_data  : Out   Std_ulogic_vector(31 Downto 0); --Wishbone: read data
			--Interface for the camera harware
			SIO_C      : Inout Std_ulogic; --SIO_C - SCCB clock signal. FPGA -> Camera
			SIO_D      : Inout Std_ulogic; --SIO_D - SCCB data signal (bi-direcctional). FPGA <--> Camera
			VSYNC      : In    Std_ulogic; --Camera VSYNC signal
			HREF       : In    Std_ulogic; --Camera HREF signal
			PCLK       : In    Std_ulogic; --Camera PCLK signal
			Data       : In    Std_ulogic_vector (7 Downto 0) --Camera data out pins

		);
	End component;

	component wb_buttons_leds is
	  generic (
		BASE_ADDRESS    : std_ulogic_vector(31 downto 0) := x"9000_0000";
		LED_ADDRESS     : std_ulogic_vector(31 downto 0) := x"9000_0000";
		BUTTON_ADDRESS  : std_ulogic_vector(31 downto 0) := x"9000_0004"
	  );
	  port (
		clk        : in  std_ulogic;
		reset      : in  std_ulogic;

		-- Wishbone interface
		i_wb_cyc   : in  std_ulogic;
		i_wb_stb   : in  std_ulogic;
		i_wb_we    : in  std_ulogic;
		i_wb_addr  : in  std_ulogic_vector(31 downto 0);
		i_wb_data  : in  std_ulogic_vector(31 downto 0);
		o_wb_ack   : out std_ulogic;
		o_wb_stall : out std_ulogic;
		o_wb_data  : out std_ulogic_vector(31 downto 0);

		-- I/O
		buttons    : in  std_ulogic_vector(2 downto 0);
		leds       : out std_ulogic_vector(7 downto 0)
	  );
	end component;

    -- External bus interface (available if XBUS_EN = true) --
	--Now connected to the interconnect
  signal  xbus_adr_o :std_ulogic_vector(31 downto 0);                    -- address
  signal  xbus_dat_o     : std_ulogic_vector(31 downto 0);                    -- write data
  signal  xbus_cti_o     : std_ulogic_vector(2 downto 0);                     -- cycle type
  signal  xbus_tag_o     : std_ulogic_vector(2 downto 0);                     -- access tag
  signal  xbus_we_o      : std_ulogic;                                        -- read/write
  signal  xbus_sel_o     : std_ulogic_vector(3 downto 0);                     	-- byte enable
  signal  xbus_stb_o     : std_ulogic;                                        	-- strobe
  signal  xbus_cyc_o     : std_ulogic;                                        	-- valid cycle
  signal  xbus_dat_i     :  std_ulogic_vector(31 downto 0) := (others => 'L'); -- read data
  signal  xbus_ack_i     :  std_ulogic := 'L';                                 -- transfer acknowledge
  signal  xbus_err_i     :  std_ulogic := 'L';                                 -- transfer error
  
	--Slave 0 Wishbone signals
	signal		s0_o_wb_cyc   :   Std_ulogic; --S0 Wishbone: cycle valid
	signal		s0_o_wb_stb   :   Std_ulogic; --S0 Wishbone: strobe
	signal		s0_o_wb_we    :   Std_ulogic; --S0 Wishbone: 1=write, 0=read
	signal		s0_o_wb_addr  :   Std_ulogic_vector(31 Downto 0);--S0 Wishbone: address
	signal		s0_o_wb_data  :   Std_ulogic_vector(31 Downto 0);--S0 Wishbone: write data
	signal		s0_i_wb_ack   :  Std_ulogic; --S0 Wishbone: acknowledge
	signal		s0_i_wb_stall :  Std_ulogic; --S0 Wishbone: stall (always '0')
	signal		s0_i_wb_data  :  Std_ulogic_vector(31 Downto 0); --S0 Wishbone: read data
		--Slave 1 Wishbone signals
	signal		s1_o_wb_cyc   :   Std_ulogic; --S1 Wishbone: cycle valid
	signal		s1_o_wb_stb   :   Std_ulogic; --S1 Wishbone: strobe
	signal		s1_o_wb_we    :   Std_ulogic; --S1 Wishbone: 1=write, 0=read
	signal		s1_o_wb_addr  :   Std_ulogic_vector(31 Downto 0);--S1 Wishbone: address
	signal		s1_o_wb_data  :   Std_ulogic_vector(31 Downto 0);--S1 Wishbone: write data
	signal		s1_i_wb_ack   :  Std_ulogic; --S1 Wishbone: acknowledge
	signal		s1_i_wb_stall : Std_ulogic; --S1 Wishbone: stall (always '0')
	signal		s1_i_wb_data  :  Std_ulogic_vector(31 Downto 0); --S1 Wishbone: read data
  
begin
rstn_internal <= not(rstn_i);
  -- The core of the problem ----------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  neorv32_inst: entity neorv32.neorv32_top
  generic map (
    -- Clocking --
    CLOCK_FREQUENCY  => CLOCK_FREQUENCY, -- clock frequency of clk_i in Hz
    -- Boot Configuration --
    BOOT_MODE_SELECT => 0,               -- boot via internal bootloader
    -- RISC-V CPU Extensions --
    RISCV_ISA_Zicntr => true,            -- implement base counters?
    RISCV_ISA_M      => true,              -- implement mul/div extension?
    RISCV_ISA_C      => true,              -- implement compressed extension?
    -- Internal Instruction memory --
    IMEM_EN          => true,         -- implement processor-internal instruction memory
    IMEM_SIZE        => IMEM_SIZE,       -- size of processor-internal instruction memory in bytes
    -- Internal Data memory --
    DMEM_EN          => true,         -- implement processor-internal data memory
    DMEM_SIZE        => DMEM_SIZE,       -- size of processor-internal data memory in bytes
    -- Processor peripherals --
    --IO_GPIO_NUM      => IO_GPIO_NUM,     -- number of GPIO input/output pairs (0..32)
    IO_CLINT_EN      => true,            -- implement core local interruptor (CLINT)?
    IO_UART0_EN      => true,            -- implement primary universal asynchronous receiver/transmitter (UART0)?
    --IO_PWM_NUM_CH    => IO_PWM_NUM_CH,    -- number of PWM channels to implement (0..12); 0 = disabled
    XBUS_EN => true,
    XBUS_TIMEOUT => 20
  )
  port map (
    -- Global control --
    clk_i       => clk_i,                        -- global clock, rising edge
    rstn_i      => rstn_i,                       -- global reset, low-active, async
    -- GPIO (available if IO_GPIO_NUM > 0) --
    --gpio_o      => con_gpio_o,                   -- parallel output
    --gpio_i      => (others => '0'),              -- parallel input
    -- primary UART0 (available if IO_UART0_EN = true) --
    uart0_txd_o => uart_txd_o,                   -- UART0 send data
    uart0_rxd_i => uart_rxd_i,                   -- UART0 receive data
    -- PWM (available if IO_PWM_NUM_CH > 0) --
    --pwm_o       => con_pwm_o,                     -- pwm channels
    xbus_adr_o =>   xbus_adr_o,               -- address
    xbus_dat_o =>   xbus_dat_o,                   -- write data
    xbus_cti_o =>   xbus_cti_o,                    -- cycle type
    xbus_tag_o =>   xbus_tag_o,                    -- access tag
    xbus_we_o =>   xbus_we_o,                                        -- read/write
    xbus_sel_o =>   xbus_sel_o,                  -- byte enable
    xbus_stb_o =>   xbus_stb_o,                                       -- strobe
    xbus_cyc_o =>   xbus_cyc_o,                                        -- valid cycle
    xbus_dat_i =>   xbus_dat_i,-- read data
    xbus_ack_i =>   xbus_ack_i,                              -- transfer acknowledge
    xbus_err_i =>   xbus_err_i                               -- transfer error
  );



	wb_1m2s_interconnect_inst:wb_1m2s_interconnect
	 port map(
		clk=>clk_i,
		reset=>rstn_internal,
		--Master pins
		m_i_wb_cyc => xbus_cyc_o,
		m_i_wb_stb   => xbus_stb_o,
		m_i_wb_we    => xbus_we_o,
		m_i_wb_addr  => xbus_adr_o,
		m_i_wb_data  => xbus_dat_o,
		m_o_wb_ack   => xbus_ack_i,
		m_o_wb_stall => open,
		m_o_wb_data  => xbus_dat_i,

		--S0 pins. Peripheral pin directions are inverted compared to master
		s0_o_wb_cyc   => s0_o_wb_cyc,
		s0_o_wb_stb   => s0_o_wb_stb,
		s0_o_wb_we    => s0_o_wb_we,
		s0_o_wb_addr  => s0_o_wb_addr,
		s0_o_wb_data  => s0_o_wb_data,
		s0_i_wb_ack   => s0_i_wb_ack,
		s0_i_wb_stall => s0_i_wb_stall,
		s0_i_wb_data  => s0_i_wb_data,

		--S1 pins
		s1_o_wb_cyc => s1_o_wb_cyc,
		s1_o_wb_stb   => s1_o_wb_stb,
		s1_o_wb_we    => s1_o_wb_we,
		s1_o_wb_addr  => s1_o_wb_addr,
		s1_o_wb_data  => s1_o_wb_data,
		s1_i_wb_ack  => s1_i_wb_ack,
		s1_i_wb_stall => s1_i_wb_stall,
		s1_i_wb_data  => s1_i_wb_data
	);

  wb_peripheral_top_inst:wb_peripheral_top
  generic map(
      BASE_ADDRESS  => x"90000000"
  )
  port map(
    clk=>clk_i,
    reset=>rstn_internal,
    i_wb_cyc => s0_o_wb_cyc,
    i_wb_stb =>s0_o_wb_stb,
    i_wb_we  => s0_o_wb_we,
    i_wb_addr => s0_o_wb_addr,
    i_wb_data => s0_o_wb_data,
    o_wb_ack  => s0_i_wb_ack,
    o_wb_stall => s0_i_wb_stall,
    o_wb_data => s0_i_wb_data
    --buttons   => buttons,
    --leds      => leds
  );
  xbus_err_i <= '0';

	wb_ov5640_inst : wb_ov5640
	port map(
		clk => clk_i,
		reset => rstn_internal,
    i_wb_cyc => s1_o_wb_cyc,
    i_wb_stb => s1_o_wb_stb,
    i_wb_we  => s1_o_wb_we,
    i_wb_addr => s1_o_wb_addr,
    i_wb_data => s1_o_wb_data,
    o_wb_ack  => s1_i_wb_ack,
    o_wb_stall => s1_i_wb_stall,
    o_wb_data => s1_i_wb_data,
		SIO_C => SIO_C,
		SIO_D => SIO_D,
		VSYNC => VSYNC,
		HREF => HREF,
		PCLK => PCLK,
		Data => Data
	);

  -- GPIO --
  --gpio_o <= con_gpio_o(IO_GPIO_NUM-1 downto 0);

  -- PWM --
  --pwm_o <= con_pwm_o(IO_PWM_NUM_CH-1 downto 0);

end architecture;
