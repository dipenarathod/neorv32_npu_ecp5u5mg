--https://forum.digikey.com/t/i2c-master-vhdl/12797
--Based on: https://github.com/vogma/OV7670_ArtyA7/blob/main/OV7670_ArtyA7_srcs/rtl/i2c/i2c_master.vhd
--The code is largely the same as on the repo with some name changes and comments

Library ieee;
Use ieee.std_logic_1164.All;
Use ieee.numeric_std.All;

Library work;

Entity i2c_controller Is
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
End Entity;

architecture rtl of i2c_controller is 
	constant divider: integer := (input_clk/bus_clk)/4;	--number of clocks in 1/4 cycle of SIO_C
	
	--FSM
	type I2C_CONTROLLER_STATE is (READY, START, COMMAND, SLV_ACK1, WRITE_STATE, READ_STATE, SLV_ACK2, MASTER_ACK, STOP);
	signal state: I2C_CONTROLLER_STATE;
	signal state_next: I2C_CONTROLLER_STATE;
	
  signal count_reg, count_next		: integer range 0 to divider*4;	-- counter for i2c-clock timing
  signal bit_cnt, bit_cnt_next		: integer range 0 to 7 := 7;		-- tracks bit number in transaction
  signal data_clk							: std_logic;							-- data clock for sda
  signal data_clk_prev					: std_logic;							-- data clock during previous system clock
  signal scl_clk							: std_logic;							-- constantly running internal scl
  signal scl_ena, scl_ena_next		: std_logic := '0';					-- enables internal scl to output
  signal sda_int, sda_int_next		: std_logic := '1';					-- internal sda
  signal sda_ena							: std_logic;							-- enables internal sda to output
  signal addr_rw, addr_next			: std_logic_vector(7 downto 0);	-- latched in address and read/write
  signal data_tx, data_tx_next		: std_logic_vector(7 downto 0);	-- latched in data to write to slave
  signal data_rx, data_rx_next		: std_logic_vector(7 downto 0);	-- data received from slave
  signal stretch_reg, stretch_next	: std_logic;							-- stretch from slave
  signal busy_r, busy_next				: std_logic;							-- busy flag
  signal ack_reg, ack_next				: std_logic;							-- acknowledge flag
  signal data_clk_re, data_clk_fe	: std_logic;							-- rising and falling edge of data_clk
	
begin

	--About the stretch register
	--In I2C, the slave is allowed to slow the master down. The master releases the SCL and let's it go high via pull-up.
	--If the slave is not ready, the it holds the SCL low. This process is called clock stretching
	--The master must wait until SCL goes high before continuing
	--A counter is used to count pulses to generate a clock for I2C using the system clock. The count only goes up if the 
	--stretch register is set (which is determined by the process below this procces)
	--The following process updates the counter and stretch register value every clock cycle
	-- register setup for i2c-bus clock and data clock
	process(clk, reset_n)
	begin
		if(reset_n = '1') then						-- reset asserted
			stretch_reg <= '0';
			count_reg <= 0;
		elsif(rising_edge(clk)) then
			stretch_reg <= stretch_next;
			data_clk_prev <= data_clk;				-- store previous value of data clock
			count_reg <= count_next;
		end if;
	end process;
	
	--The following process determines if the slave is ready or not and sets/rests the stretch register
	-- (also) generate the timing for the bus clock (scl_clk) and the data clock (data_clk)
	process(count_reg, stretch_reg, scl)
	begin
		stretch_next <= stretch_reg;		
      case count_reg is
			when 0 to divider-1 =>					-- first 1/4 cycle of clocking
				scl_clk <= '0';						-- set scl
				data_clk <= '0';
			when divider to divider*2-1 =>		-- second 1/4 cycle of clocking
				scl_clk <= '0';
				data_clk <= '1';
			when divider*2 to divider*3-1 =>		-- third 1/4 cycle of clocking
				scl_clk <= '1';						-- release scl
				if(scl = '0') then					-- detect if slave is stretching clock
					stretch_next <= '1';
				else
					stretch_next <= '0';
				end if;
				data_clk <= '1';
			when others =>								-- last 1/4 cycle of clocking
				scl_clk <= '1';
				data_clk <= '0';
		end case;
	end process;
	
	--The following process saves/forwards various regsiter values (and is responsible for resetting them as well)
	-- register setup and reset
	process(clk, reset_n)
	begin
		if(reset_n = '1') then						-- reset asserted
			state <= ready;							-- return to initial state
			busy_r <= '1';								-- indicate not available
			scl_ena <= '0';							-- sets scl high impedance
			sda_int <= '1';							-- sets sda high impedance
			ack_reg <= '0';							-- clear acknowledge error flag
			bit_cnt <= 7;								-- restarts data bit counter
			data_rx <= x"00";							-- clear data read port
		elsif(rising_edge(clk)) then				-- set new register values
			state <= state_next;
			busy_r <= busy_next;
			ack_reg <= ack_next;
			bit_cnt <= bit_cnt_next;
			scl_ena <= scl_ena_next;
			sda_int <= sda_int_next;
			addr_rw <= addr_next;
			data_tx <= data_tx_next;
			data_rx <= data_rx_next;
		end if;
	end process;
	
	--FSM from DigiKey diagram
	-- state machine and writing to sda during scl low (data_clk rising edge)
	process(state, busy_r, ack_reg, bit_cnt, scl_ena, sda_int, data_clk_re, data_clk_fe, ena, addr, rw, data_wr, addr_rw, data_tx, data_rx, sda)
	begin
		-- hold previous values as default
		state_next <= state;
		busy_next <= busy_r;
		ack_next <= ack_reg;
		bit_cnt_next <= bit_cnt;
		scl_ena_next <= scl_ena;
		sda_int_next <= sda_int;
		addr_next <= addr_rw;
		data_tx_next <= data_tx;
		data_rx_next <= data_rx;
		
		-- differentiate between rising and falling edge of data clock
		if(data_clk_re = '1') then											-- data clock rising edge
			case state is
				when ready =>													-- idle state
					if(ena = '1') then										-- transaction requested
						busy_next <= '1';										-- flag busy
						addr_next <= addr & rw;								-- collect requested slave address and command
						data_tx_next <= data_wr;							-- collect requested data to write
						state_next <= start;									-- go to start bit
					else															-- remain idle
						busy_next <= '0';										-- unflag busy and remain idle
					end if;
				when start =>													-- start bit of transaction
					busy_next <= '1';											-- resume busy if continuous mode
					sda_int_next <= addr_rw(bit_cnt);					-- set first address bit to bus
					state_next <= command;									-- go to command
				when command =>												-- address and command byte of transaction
					if(bit_cnt = 0) then										-- command transmit finished
						sda_int_next <= '1';									-- release sda for slave acknowledge
						bit_cnt_next <= 7;									-- reset bit counter for "byte" states
						state_next <= slv_ack1;								-- go to slave acknowledge (command)
					else															-- next clock cycle of command state
						bit_cnt_next <= bit_cnt - 1;						-- keep track of transaction bits
						sda_int_next <= addr_rw(bit_cnt-1);				-- write address/command bit to bus and continue with command
					end if;
				when slv_ack1 =>												-- slave acknowledge bit (command)
					if(addr_rw(0) = '0') then								-- write command
						sda_int_next <= data_tx(bit_cnt);				-- write first bit of data
						state_next <= WRITE_STATE;										-- go to write byte
					else															-- read command
						sda_int_next <= '1';									-- release sda from incoming data
						state_next <= READ_STATE;										-- go to read byte
					end if;
				when WRITE_STATE =>														-- write byte of transaction
					busy_next <= '1';											-- resume busy if continuous mode
					if(bit_cnt = 0) then										-- write byte transmit finished
						sda_int_next <= '1';									-- release sda for slave acknowledge
						bit_cnt_next <= 7;									-- reset bit counter for "byte" states
						state_next <= slv_ack2;								-- go to slave acknowledge (write)
					else															-- next clock cycle of write state
						bit_cnt_next <= bit_cnt - 1;						-- keep track of transaction bits
						sda_int_next <= data_tx(bit_cnt-1);				-- write next bit to bus and continue writing
					end if;
				when READ_STATE =>														-- read byte of transaction
					busy_next <= '1';											-- resume busy if continuous mode
					if(bit_cnt = 0) then										-- read byte receive finished
						if(ena = '1' and addr_rw = addr & rw) then	-- continuing with another read at same address
							sda_int_next <= '0';								-- acknowledge the byte has been received
						else														-- stopping or continuing with a write
							sda_int_next <= '1';								-- send a no-acknowledge (before stop or repeated start)
						end if;
						bit_cnt_next <= 7;									-- reset bit counter for "byte" states
						state_next <= MASTER_ACK;								-- go to master acknowledge
					else															-- next clock cycle of read state
						bit_cnt_next <= bit_cnt - 1;						-- keep track of transaction bits and continue reading
					end if;
				when slv_ack2 =>												-- slave acknowledge bit (write)
					if(ena = '1') then										-- continue transaction
						busy_next <= '0';										-- continue is accepted
						addr_next <= addr & rw;								-- collect requested slave address and command
						data_tx_next <= data_wr;							-- collect requested data to write
						if(addr_rw = addr & rw) then						-- continue transaction with another write
							sda_int_next <= data_wr(bit_cnt);			-- write first bit of data
							state_next <=WRITE_STATE;									-- go to write byte
						else														-- continue transaction with a read or new slave
							state_next <= start;								-- go to repeated start
						end if;
					else															-- complete transaction
						state_next <= stop;									-- go to stop bit
					end if;
				when MASTER_ACK =>												-- master acknowledge bit after a read
					if(ena = '1') then										-- continue transaction
						busy_next <= '0';										-- continue is accepted and data received is available on bus
						addr_next <= addr & rw;								-- collect requested slave address and command
						data_tx_next <= data_wr;							-- collect requested data to write
						if(addr_rw = addr & rw) then						-- continue transaction with another read
							sda_int_next <= '1';								-- release sda from incoming data
							state_next <= READ_STATE;									-- go to read byte
						else														-- continue transaction with a write or new slave
							state_next <= start;								-- repeated start
						end if;    
					else															-- complete transaction
						state_next <= stop;									-- go to stop bit
					end if;
				when stop =>													-- stop bit of transaction
					busy_next <= '0';											-- unflag busy
					state_next <= ready;										-- go to idle state
			end case;    
		elsif (data_clk_fe = '1') then									-- data clock falling edge
			case state is
				when start =>                  
					if(scl_ena = '0') then									-- starting new transaction
						scl_ena_next <= '1';									-- enable scl output
						ack_next <= '0';										-- reset acknowledge error output
					end if;
				when slv_ack1 =>												-- receiving slave acknowledge (command)
					if(sda /= '0') then										-- no-acknowledge or previous no-acknowledge
						ack_next <= '1';										-- set error output if no-acknowledge
					end if;
				when READ_STATE =>														-- receiving slave data
					data_rx_next(bit_cnt) <= sda;							-- receive current slave data bit
				when slv_ack2 =>												-- receiving slave acknowledge (write)
					if(sda /= '0') then										-- no-acknowledge or previous no-acknowledge
						ack_next <= '1';										-- set error output if no-acknowledge
					end if;
				when stop =>
					scl_ena_next <= '0';										-- disable scl
				when others =>
					NULL;
			end case;
		end if;
	end process;  

	data_clk_re <= data_clk and not data_clk_prev;					-- rising edge of data_clk
	data_clk_fe <= not data_clk and data_clk_prev;					-- falling edge of data_clk
	
	-- next-logic for i2c-clock timing
	count_next <=	0 when count_reg = divider*4-1 else				-- reset timer when end of timing cycle
						count_reg + 1 when stretch_reg = '0'			-- continue clock timing when no clock stretching from slave detected
						else count_reg;										-- hold clock timing if clock stretching from slave detected
	
	-- next-logic for sda output enable
	with state select
		sda_ena <= 	data_clk_prev when start,							-- generate start condition
						not data_clk_prev when stop,						-- generate stop condition
						sda_int when others;									-- set to internal sda signal    

	-- set scl and sda outputs
	scl <= '0' when (scl_ena = '1' and scl_clk = '0') else 'Z';
	sda <= '0' when sda_ena = '0' else 'Z';
  
	-- set flag output
	busy <= busy_r;
	ack_error <= ack_reg;

	-- set data_rd output
	data_rd <= data_rx;	
  
end;