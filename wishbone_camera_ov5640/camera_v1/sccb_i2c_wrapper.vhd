--Wrapper for I2C controller to write to the OV5640 registers at bootup using values from a table
Library ieee;
Use ieee.std_logic_1164.All;
Use ieee.numeric_std.All;

Library work;

Entity sccb_i2c_wrapper Is
	Generic (
		INPUT_CLK_HZ : Integer := 72_000_000;
		BUS_CLK_HZ   : Integer := 400_000
	);
	Port (
		clk   : In    Std_ulogic;
		reset : In    Std_ulogic;
		--SCCB pins
		SIO_C : Inout Std_ulogic;
		SIO_D : Inout Std_ulogic;
		--control
		start : In    Std_ulogic; --1 to start
		busy  : Out   Std_ulogic; --0 = idle. 1 = busy
		done  : Out   Std_ulogic; --0 = not done. 1 = busy
		err   : Out   Std_ulogic --Included because the I2C controller has it	
	);
End;

Architecture rtl Of sccb_i2c_wrapper Is

	--I2C controller
	Component i2c_controller Is
		Generic (
			input_clk : Integer := 72_000_000; --input clock speed from user logic in Hz
			bus_clk   : Integer := 400_000 --speed of I2C bus. OV5640 supports 400KHz (fast mode). 
		);
		Port (
			clk       : In    Std_ulogic; --system clock
			reset_n   : In    Std_ulogic; --Needs to be active low
			ena       : In    Std_logic; --0= no transaction inititated. 1 : latches in addr, rw, and data_wr to initiate a transaction
			--If ena is high at the end of a transaction, then a new address, read/write command, and data are latched in to continue a transaction
			addr      : In    Std_logic_vector (6 Downto 0); --Address of target slave
			rw        : In    Std_logic; --0: write command. 1 = read command
			data_wr   : In    Std_logic_vector (7 Downto 0); --data to transmit if rw = 0 (write)
			data_rd   : Out   Std_logic_vector (7 Downto 0); --data to read if rw = 1 (read)
			busy      : Out   Std_logic; --0: I2c master is idle and last read command data is available on data_rd. 1 = command has been latched and trasnaction is in progress
			ack_error : Out   Std_logic; --0: no acknowledge errors. 1 = error
			SDA       : Inout Std_logic; --Data line
			SCL       : Inout Std_logic --Serial clock line
		);
	End Component;

	--Signals for I2C cotnroller
	Signal i2c_ena : Std_logic;
	Signal device_addr : Std_logic_vector(6 Downto 0) := "0111100"; --OV5640 address = 0x78. For I2C (SCCB), do 0x78>>1 = 0x3c
	Signal i2c_rw : Std_logic := '0';
	Signal i2c_data_wr : Std_logic_vector(7 Downto 0) := (Others => '0');
	Signal i2c_data_rd : Std_logic_vector(7 Downto 0) := (Others => '0'); --No use, but created in case
	Signal i2c_busy : Std_logic;
	Signal i2c_ack_error : Std_logic;
	--ROM Tables
	Type ov5640_reg_addr_arr Is Array (Natural Range <>) Of Std_logic_vector(15 Downto 0); --OV5640 address array type
	Type ov5640_reg_data_arr Is Array (Natural Range <>) Of Std_logic_vector(7 Downto 0); --OV5640 data arrays

	Signal ov5640_reg_addr : ov5640_reg_addr_arr (0 To 21) := (
		x"5001", x"4300",
		x"3800", x"3801",
		x"3802", x"3803",
		x"3804", x"3805",
		x"3806", x"3807",
		x"3808", x"3809",
		x"380A", x"380B",
		x"380C", x"380D",
		x"380E", x"380F",
		x"3810", x"3811",
		x"3812", x"3813"); --register address array

	Signal ov5640_reg_data : ov5640_reg_data_arr (0 To 21) := (
		x"20", x"30", 	--Set output to YUV422 (YUYV), enabled scaling, wrote all regsiters from 3800-380F, and introduced offset registers
		x"00", x"00",
		x"00", x"00",
		x"0A", x"3F",
		x"07", x"9F",
		x"00", x"32",
		x"00", x"32",
		x"0B", x"1C",	--These 2 and the 2 below are timing values to control FPS from what I understand. Need to experiment with them. I am using default values for now
		x"07", x"B0",
		x"00", x"10",	--These 2 and the two below are offset register values. Need to experiment with them. I am using default values for now
		x"00", x"06"); 
		--register values. Refer datasheet for each address to understand values

	Signal table_length : Natural := ov5640_reg_addr'Length; --ROM table Length

	Signal table_index : Natural := 0;
	Signal reg_addr_loaded : Std_logic_vector(15 Downto 0); --Loaded address from ROM table
	Signal reg_data_loaded : Std_logic_vector(7 Downto 0); --Loaded register data

	--BRAM inference hints
	Attribute ram_style : String;
	Attribute syn_ramstyle : String;

	Attribute ram_style Of ov5640_reg_addr : Signal Is "block";
	Attribute ram_style Of ov5640_reg_data : Signal Is "block";

	Attribute syn_ramstyle Of ov5640_reg_addr : Signal Is "block_ram";
	Attribute syn_ramstyle Of ov5640_reg_data : Signal Is "block_ram";
	--FSM
	--3-phase write transmission according to the user manual: ID address -> High byte of 16- bit address -> Low byte of 16-bit address -> Value
	Type state_t Is (SCCB_IDLE, SCCB_BEGIN, SCCB_ADDR_WRITE_UPPER, SCCB_ADDR_LOAD_LOWER, SCCB_ADDR_WRITE_LOWER, SCCB_ADDR_WAIT, SCCB_DATA_LOAD, SCCB_DATA_WAIT, SCCB_DATA_WRITE, SCCB_DONE);
	Signal state : state_t := SCCB_IDLE;
Begin

	busy <= '1' When (state /= SCCB_IDLE And state /= SCCB_DONE) Else '0';
	done <= '1' When (state = SCCB_DONE) Else '0';
	err <= i2c_ack_error;
	i2c_controller_inst : i2c_controller
	Port Map(
		clk       => clk,
		reset_n   => reset,
		ena       => i2c_ena,
		addr      => device_addr,
		rw        => i2c_rw,
		data_wr   => i2c_data_wr,
		data_rd   => i2c_data_rd,
		busy      => i2c_busy,
		ack_error => i2c_ack_error,
		SDA       => SIO_D,
		SCL       => SIO_C
	);

	--FSM Process
	--3-phase write transmission according to the user manual: ID address -> High byte of 16- bit address -> Low byte of 16-bit address -> Value
	--I2C is a stricter protocol than SCCB: Start -> data address -> register addresses -> data. The ACK bit does not matter in SCCB
	--The FSM is only worked through once, when start rise is 1
	Process (clk)
	Begin
		If rising_edge(clk) Then
			If (reset = '1') Then
				table_index <= 0;
				state <= SCCB_IDLE;
				i2c_ena <= '0';
				i2c_rw <= '0';
				i2c_data_wr <= (Others => '0');
			Else
				Case state Is
					When SCCB_IDLE =>
						--Idle until start is asserted
						i2c_ena <= '0';
						If (start = '1') Then
							table_index <= 0;
							state <= SCCB_BEGIN;
						End If;

					When SCCB_BEGIN =>

						--We want to send two address (upper and lower byte), so we must ensure ENA remains high. the i2C controller hndles the device address logic
						--Begin transaction. First byte after device address is upper byte of reg address so load that
						i2c_rw <= '0'; --Write command
						i2c_ena <= '1'; --Latched to initiate a transaction
						i2c_data_wr <= reg_addr_loaded(15 Downto 8); --Get the upper byte of the address
						state <= SCCB_ADDR_WRITE_UPPER;

					When SCCB_ADDR_WRITE_UPPER =>
						--Wait until controller indicates transaction has started by setting busy
						If (i2c_ack_error = '1') Then --OV5640 is unknown territory. I am using the error signal.
							i2c_ena <= '0'; --Do not start a transaction if there is an error
							state <= SCCB_DONE; --Done if there is an error
						Elsif (i2c_busy = '1') Then --Only load the lower byte once the upper byte is being uploaded. Otherwise, we will problematically skip over the upper byte of the reg address
							--Preload next byte while controller is busy with current byte
							state <= SCCB_ADDR_LOAD_LOWER;
						End If;

					When SCCB_ADDR_LOAD_LOWER =>
						--Load lower byte of reg address while upper address byte is being sent
						i2c_data_wr <= reg_addr_loaded(7 Downto 0);
						state <= SCCB_ADDR_WAIT;

					When SCCB_ADDR_WAIT =>
						--Wait until busy is 0 indicating the byte has been transmitted
						If (i2c_ack_error = '1') Then --OV5640 is unknown territory. I am using the error signal.
							i2c_ena <= '0'; --Do not start a transaction if there is an error
							state <= SCCB_DONE; --Done if there is an error
						Elsif (i2c_busy = '0') Then
							state <= SCCB_ADDR_WRITE_LOWER;
						End If;

					When SCCB_ADDR_WRITE_LOWER =>
						--Load data byte while lower reg-address byte is sent
						If (i2c_ack_error = '1') Then --OV5640 is unknown territory. I am using the error signal.
							i2c_ena <= '0'; --Do not start a transaction if there is an error
							state <= SCCB_DONE; --Done if there is an error
						Elsif (i2c_busy = '1') Then
							--i2c_data_wr <= reg_data_loaded;
							state <= SCCB_DATA_LOAD;
						End If;

					When SCCB_DATA_LOAD =>
						--Load data byte until the lower address byte is sent 
						i2c_data_wr <= reg_data_loaded;
						state <= SCCB_DATA_WAIT;

					When SCCB_DATA_WAIT =>
						--Wait until busy is 0 indicating the byte has been transmitted
						If (i2c_ack_error = '1') Then --OV5640 is unknown territory. I am using the error signal.
							i2c_ena <= '0'; --Do not start a transaction if there is an error
							state <= SCCB_DONE; --Done if there is an error
						Elsif (i2c_busy = '0') Then
							--Deassert ena so the controller will STOP after the data byte
							i2c_ena <= '0';
							state <= SCCB_DATA_WRITE;
						End If;

					When SCCB_DATA_WRITE =>
						--Wait for transaction to complete
						If (i2c_ack_error = '1') Then --OV5640 is unknown territory. I am using the error signal.
							i2c_ena <= '0'; --Do not start a transaction if there is an error
							state <= SCCB_DONE; --Done if there is an error
						Elsif (i2c_busy = '0') Then
							If (table_index = table_length - 1) Then
								state <= SCCB_DONE;
							Else
								table_index <= table_index + 1;
								state <= SCCB_BEGIN;
							End If;
						End If;

					When SCCB_DONE =>
						--Done
						i2c_ena <= '0';
						If (start = '0') Then
							state <= SCCB_IDLE;
						End If;
				End Case;
			End If;
		End If;
	End Process;

	--Read address ROM table
	Process (clk)
	Begin
		If (rising_edge(clk)) Then
			If (reset = '1') Then
				reg_addr_loaded <= (Others => '0');
			Else
				reg_addr_loaded <= ov5640_reg_addr(table_index);
			End If;
		End If;
	End Process;

	--Read data table
	Process (clk)
	Begin
		If (rising_edge(clk)) Then
			If (reset = '1') Then
				reg_data_loaded <= (Others => '0');
			Else
				reg_data_loaded <= ov5640_reg_data(table_index);
			End If;
		End If;
	End Process;

End;

