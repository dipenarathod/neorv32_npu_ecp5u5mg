--Wrapper for I2C controller to write to the OV5640 registers at bootup using values from a table
Library ieee;
Use ieee.std_logic_1164.All;
Use ieee.numeric_std.All;

Library work;

entity sccb_i2c_wrapper is
	  generic (
			INPUT_CLK_HZ : integer := 72_000_000;
			BUS_CLK_HZ   : integer := 400_000
	  );
	  port (
			clk     	: in  std_ulogic;
			reset   	: in  std_ulogic;         
			--SCCB pins
			SIO_c    	: inout std_ulogic;
			SIO_D    	: inout std_ulogic;
			--control
			start   	: in  std_ulogic;          	--1 to start
			busy  		: out std_ulogic;				--0 = idle. 1 = busy
			done_o  	: out std_ulogic;				--0 = not done. 1 = busy
			err			: out std_ulogic				--Included because the I2C controller has it	
	  );
end;

Architecture rtl of sccb_i2c_wrapper is
	
		--I2C controller
		component i2c_controller Is
			Generic (
				input_clk : integer := 72_000_000;						--input clock speed from user logic in Hz
				bus_clk   : integer := 400_000							--speed of I2C bus. OV5640 supports 400KHz (fast mode). TODO: test if 72MHz can be split in 400KHz
			);
			Port (
				clk        	: In  	Std_ulogic; --system clock
				reset_n    : In  	Std_ulogic; --Needs to be active low
				ena		 	: In 	Std_logic;	--0= no transaction inititated. 1 : latches in addr, rw, and data_wr to initiate a transaction
												--If ena is high at the end of a transaction, then a new address, read/write command, and data are latched in to continue a transaction
				addr		: In 	Std_logic_vector (6 downto 0);	--Address of target slave
				rw			: In	Std_logic;	--0: write command. 1 = read command
				data_wr		: In 	Std_logic_vector (7 downto 0);	--data to transmit if rw = 0 (write)
				data_rd		: out 	Std_logic_vector (7 downto 0);	--data to read if rw = 1 (read)
				busy		: out	Std_logic;	--0: I2c master is idle and last read command data is available on data_rd. 1 = command has been latched and trasnaction is in progress
				ack_error	: out	Std_logic;	--0: no acknowledge errors. 1 = error
				SDA			: Inout Std_logic;	--Data line
				SCL			: Inout	Std_logic	--Serial clock line
			);
		End component;
		
		--Signals for I2C cotnroller
		signal i2c_ena			: std_logic;
		signal device_addr		: std_logic_vector(6 downto 0) := x"3C";	--OV5640 address = 0x78. For I2C (SCCB), do 0x78>>1 = 0x3c
		signal i2c_rw       	: std_logic := '0';
		signal i2c_data_wr   	: std_logic_vector(7 downto 0) := (others => '0');
		signal i2c_data_rd   	: std_logic_vector(7 downto 0) := (others => '0');	--No use, but created in case
		signal i2c_busy     	: std_logic;
		signal i2c_ack_error 	: std_logic;
		
		--ROM Tables
		Type ov5640_reg_addr_arr 		is Array (Natural range <>) of std_logic_vector(15 downto 0);	--OV5640 address array type
		Type ov5640_reg_data_arr		is Array (Natural range <>) of std_logic_vector(7 downto 0);	--OV5640 data arrays
		
		signal ov5640_reg_addr	: ov5640_reg_addr_arr := (x"4300", x"3808",x"3009",x"380A",x"380B");	--register address array
		signal ov5460_reg_data	: ov5640_reg_data_arr := (x"30", x"00",x"32", x"00",x"32");			--register values. Refer datasheet for each address to understand values
		
		signal table_length	:Natural := ov5640_reg_addr'Length; --ROM table Length
		
		--FSM
		--3-phase write transmission according to the user manual: ID address -> High byte of 16- bit address -> Low byte of 16-bit address -> Value
		
	
	begin
	
	i2c_controller_inst : i2c_controller
		port map(
			clk => clk,
			reset_n => not reset,
			ena => i2c_ena,
			addr => device_addr,
			rw => i2c_rw,
			data_wr => i2c_data_wr,
			data_rd => i2c_data_rd,
			busy => i2c_busy,
			ack_error => i2c_ack_error,
			SDA => SIO_D,
			SCL => SIO_C
		);
	
	
end;

