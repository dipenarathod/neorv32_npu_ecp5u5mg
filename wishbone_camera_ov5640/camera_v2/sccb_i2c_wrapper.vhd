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

Signal ov5640_reg_addr : ov5640_reg_addr_arr (0 To 62) := (
    -- System Control
    x"3103",  -- 0: System control
    x"3008",  -- 1: Software reset
    x"3008",  -- 2: Power up
    x"3103",  -- 3: System control
    x"3017",  -- 4: IO control
    x"3018",  -- 5: IO control
    x"3108",  -- 6: System control (SCCB)
    -- PLL and Clock
    x"3034",  -- 7: PLL control
    x"3035",  -- 8: PLL system divider
    x"3036",  -- 9: PLL multiplier
    x"3037",  -- 10: PLL root divider
    -- Array Control
    x"3630",  -- 11: Array control
    x"3631",  -- 12: Array control
    x"3632",  -- 13: Array control
    x"3633",  -- 14: Array control
    x"3621",  -- 15: Array control
    -- Analog Control
    x"3704",  -- 16: Analog control
    x"3703",  -- 17: Analog control
    x"3715",  -- 18: Analog control
    x"3717",  -- 19: Analog control
    x"370b",  -- 20: Analog control
    x"3705",  -- 21: Analog control
    x"3600",  -- 22: VCM control
    x"3601",  -- 23: VCM control
    -- Timing Control - Windowing (50x50 center crop)
    x"3800",  -- 24: X start[11:8] (center: 1271 = 0x04F7)
    x"3801",  -- 25: X start[7:0]
    x"3802",  -- 26: Y start[11:8] (center: 947 = 0x03B3)
    x"3803",  -- 27: Y start[7:0]
    x"3804",  -- 28: X end[11:8] (center+50: 1321 = 0x0529)
    x"3805",  -- 29: X end[7:0]
    x"3806",  -- 30: Y end[11:8] (center+50: 997 = 0x03E5)
    x"3807",  -- 31: Y end[7:0]
    -- Output Size
    x"3808",  -- 32: DVP output horizontal[11:8]
    x"3809",  -- 33: DVP output horizontal[7:0] = 50
    x"380a",  -- 34: DVP output vertical[11:8]
    x"380b",  -- 35: DVP output vertical[7:0] = 50
    -- Timing Registers
    x"380c",  -- 36: HTS[15:8] (horizontal total size)
    x"380d",  -- 37: HTS[7:0]
    x"380e",  -- 38: VTS[15:8] (vertical total size)
    x"380f",  -- 39: VTS[7:0]
    -- Offset Registers
    x"3810",  -- 40: ISP horizontal offset[11:8]
    x"3811",  -- 41: ISP horizontal offset[7:0]
    x"3812",  -- 42: ISP vertical offset[10:8]
    x"3813",  -- 43: ISP vertical offset[7:0]
    -- Subsampling (set to 1x1, no subsampling for direct crop)
    x"3814",  -- 44: Horizontal increment
    x"3815",  -- 45: Vertical increment
    x"3820",  -- 46: Timing control (flip/mirror/binning)
    x"3821",  -- 47: Timing control (flip/mirror/binning)
    -- Format Control
    x"4300",  -- 48: Format control (YUV422 YUYV)
    x"501f",  -- 49: ISP format select
    -- ISP Control
    x"5000",  -- 50: ISP control (lens correction, gamma, etc)
    x"5001",  -- 51: ISP control (AWB, scale, etc)
    -- AEC/AGC Control
    x"3503",  -- 52: AEC/AGC manual enable
    x"3a00",  -- 53: AEC/AGC control
    x"3a02",  -- 54: 60Hz max exposure
    x"3a03",  -- 55: 60Hz max exposure
    x"3a14",  -- 56: 50Hz max exposure
    x"3a15",  -- 57: 50Hz max exposure
    -- BLC Control
    x"4001",  -- 58: BLC control
    x"4004",  -- 59: BLC control
    -- Final
    x"3008",   -- 60: Start streaming
    x"4202",  -- 61: DVP output control (CRITICAL!)
    x"300e"   -- 62: DVP output enable
);

Signal ov5640_reg_data : ov5640_reg_data_arr (0 To 62) := (
    -- System Control
    x"03",    -- 0: Use PLL clock
    x"82",    -- 1: Software reset
    x"42",    -- 2: Power up, keep in standby
    x"03",    -- 3: Use PLL clock
    x"ff",    -- 4: Enable all outputs
    x"ff",    -- 5: Enable all outputs
    x"01",    -- 6: SCCB system control
    -- PLL and Clock (24MHz input, 24MHz PCLK output)
    x"1a",    -- 7: PLL charge pump control, 10-bit mode
    x"11",    -- 8: System clock divider /1, scale divider /1
    x"46",    -- 9: PLL multiplier = 70 (24MHz * 70 / 10 / 1 = 168MHz sysclk)
    x"13",    -- 10: PLL root divider /1, pre-divider /3
    -- Array Control
    x"36",    -- 11: Array control
    x"0e",    -- 12: Array control
    x"e2",    -- 13: Array control
    x"12",    -- 14: Array control
    x"e0",    -- 15: Array control
    -- Analog Control
    x"a0",    -- 16: Analog control
    x"5a",    -- 17: Analog control
    x"78",    -- 18: Analog control
    x"01",    -- 19: Analog control
    x"60",    -- 20: Analog control
    x"1a",    -- 21: Analog control
    x"08",    -- 22: VCM control
    x"33",    -- 23: VCM control
    -- Timing Control - Windowing (50x50 from center)
    x"04",    -- 24: X start = 1271 (0x04F7) [11:8]
    x"F7",    -- 25: X start [7:0]
    x"03",    -- 26: Y start = 947 (0x03B3) [11:8]
    x"B3",    -- 27: Y start [7:0]
    x"05",    -- 28: X end = 1321 (0x0529) [11:8]
    x"29",    -- 29: X end [7:0]
    x"03",    -- 30: Y end = 997 (0x03E5) [11:8]
    x"E5",    -- 31: Y end [7:0]
    -- Output Size (50x50)
    x"00",    -- 32: Output width [11:8]
    x"32",    -- 33: Output width [7:0] = 50 pixels
    x"00",    -- 34: Output height [11:8]
    x"32",    -- 35: Output height [7:0] = 50 pixels
    -- Timing Registers (optimized for small output)
    x"01",    -- 36: HTS = 400 (0x0190) [15:8] - horizontal total
    x"90",    -- 37: HTS [7:0]
    x"00",    -- 38: VTS = 200 (0x00C8) [15:8] - vertical total
    x"C8",    -- 39: VTS [7:0]
    -- Offset Registers (minimal for 1:1 mapping)
    x"00",    -- 40: H offset [11:8] = 0
    x"00",    -- 41: H offset [7:0] = 0
    x"00",    -- 42: V offset [10:8] = 0
    x"00",    -- 43: V offset [7:0] = 0
    -- Subsampling (1x1, no subsampling - direct pixel mapping)
    x"11",    -- 44: H increment (1x odd, 1x even)
    x"11",    -- 45: V increment (1x odd, 1x even)
    x"40",    -- 46: No vflip, no sensor vflip
    x"00",    -- 47: No mirror, no binning
    -- Format Control
    x"30",    -- 48: YUV422, YUYV sequence
    x"00",    -- 49: ISP YUV422 format
    -- ISP Control
    x"a7",    -- 50: Enable LENC, Gamma, BPC, WPC, CIP
    x"a3",    -- 51: Enable SDE, scale, UV avg, CMX, AWB
    -- AEC/AGC Control
    x"00",    -- 52: Auto AEC/AGC
    x"38",    -- 53: AEC control, band function enabled
    x"00",    -- 54: Max exposure = 200 [15:8]
    x"C8",    -- 55: Max exposure = 200 [7:0]
    x"00",    -- 56: Max exposure = 200 [15:8]
    x"C8",    -- 57: Max exposure = 200 [7:0]
    -- BLC Control
    x"02",    -- 58: BLC start line
    x"02",    -- 59: BLC auto enabled
    -- Final
    x"02",     -- 60: Start streaming (exit standby)
    x"00",    -- 61: Enable streaming output (0x00 = ON, 0x0F = OFF)
    x"45"     -- 62: Enable DVP, VSYNC, HREF
);


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
