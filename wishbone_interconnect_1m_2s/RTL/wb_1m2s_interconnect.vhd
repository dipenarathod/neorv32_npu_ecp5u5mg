Library ieee;
Use ieee.std_logic_1164.All;
Use ieee.numeric_std.All;

--'m_' prefix for master connections
--'S0_' prefix for peripheral 0
--'S1_' prefix for peripheral 1
Entity wb_1m2s_interconnect Is
	Generic (
		S0_BASE : Std_ulogic_vector(31 Downto 0) := x"9000_0000";
		--S0_MASK : std_ulogic_vector(31 downto 0) := x"FFFF_0000";  --Mask for S0
		S1_BASE : Std_ulogic_vector(31 Downto 0) := x"9001_0000";
		S_MASK  : Std_ulogic_vector(31 Downto 0) := x"FFFF_0000" --Mask for S0 and S1
	);
	Port (
		clk           : In  Std_ulogic; --system clock
		reset         : In  Std_ulogic; --synchronous reset
		--Master pins
		m_i_wb_cyc    : In  Std_ulogic; --Master Wishbone: cycle valid
		m_i_wb_stb    : In  Std_ulogic; --Master Wishbone: strobe
		m_i_wb_we     : In  Std_ulogic; --Master Wishbone: 1=write, 0=read
		m_i_wb_addr   : In  Std_ulogic_vector(31 Downto 0);--Master Wishbone: address
		m_i_wb_data   : In  Std_ulogic_vector(31 Downto 0);--Master Wishbone: write data
		m_o_wb_ack    : Out Std_ulogic; --Master Wishbone: acknowledge
		m_o_wb_stall  : Out Std_ulogic; --Master Wishbone: stall (always '0')
		m_o_wb_data   : Out Std_ulogic_vector(31 Downto 0); --Master Wishbone: read data

		--S0 pins. Peripheral pin directions are inverted compared to master
		s0_o_wb_cyc   : Out Std_ulogic; --S0 Wishbone: cycle valid
		s0_o_wb_stb   : Out Std_ulogic; --S0 Wishbone: strobe
		s0_o_wb_we    : Out Std_ulogic; --S0 Wishbone: 1=write, 0=read
		s0_o_wb_addr  : Out Std_ulogic_vector(31 Downto 0);--S0 Wishbone: address
		s0_o_wb_data  : Out Std_ulogic_vector(31 Downto 0);--S0 Wishbone: write data
		s0_i_wb_ack   : In  Std_ulogic; --S0 Wishbone: acknowledge
		s0_i_wb_stall : In  Std_ulogic; --S0 Wishbone: stall (always '0')
		s0_i_wb_data  : In  Std_ulogic_vector(31 Downto 0); --S0 Wishbone: read data

		--S1 pins
		s1_o_wb_cyc   : Out Std_ulogic; --S1 Wishbone: cycle valid
		s1_o_wb_stb   : Out Std_ulogic; --S1 Wishbone: strobe
		s1_o_wb_we    : Out Std_ulogic; --S1 Wishbone: 1=write, 0=read
		s1_o_wb_addr  : Out Std_ulogic_vector(31 Downto 0);--S1 Wishbone: address
		s1_o_wb_data  : Out Std_ulogic_vector(31 Downto 0);--S1 Wishbone: write data
		s1_i_wb_ack   : In  Std_ulogic; --S1 Wishbone: acknowledge
		s1_i_wb_stall : In  Std_ulogic; --S1 Wishbone: stall (always '0')
		s1_i_wb_data  : In  Std_ulogic_vector(31 Downto 0) --S1 Wishbone: read data
	);
End Entity;

Architecture rtl Of wb_1m2s_interconnect Is
	Signal slave_select : Std_ulogic_vector(1 Downto 0); --00 = None. 01 = S0. 10 = S1
	Signal slave_select_lat : Std_ulogic_vector(1 Downto 0); --00 = None. 01 = S0. 10 = S1. Latched variant

	Signal interconnect_selected : Std_ulogic; --1 when master cycle and strobe are high. 0 otherwise

	--Latched master request fields that are held stable until ack
	Signal m_addr_lat : Std_ulogic_vector(31 Downto 0);
	Signal m_data_lat : Std_ulogic_vector(31 Downto 0);
	Signal m_we_lat : Std_ulogic;

	--Latched signals for peripherals
	Signal slave_addr : Std_ulogic_vector(31 Downto 0);
	Signal slave_data : Std_ulogic_vector(31 Downto 0);
	Signal slave_we : Std_ulogic;
	Signal stb_lat : Std_ulogic;

Begin

	interconnect_selected <= m_i_wb_cyc And m_i_wb_stb;

	--Logic to select peripheral based on incoming address
	--Calculated when a new address is passed to the interconnect
	--Process(all) block for neatness (and because I finally figured out how to enable VHDL 2008)
	Process (All) Is
	Begin
		If ((m_i_wb_addr And S_MASK) = S0_BASE) Then
			slave_select <= "01";
		Elsif ((m_i_wb_addr And S_MASK) = S1_BASE) Then
			slave_select <= "10";
		Else
			slave_select <= "00";
		End If;
	End Process;

	--Logic to latch select and hold it until it receives acknowledgement from the selected peripheral
	Process (clk) Is
	Begin
		If rising_edge(clk) Then
			If (reset = '1') Then
				slave_select_lat <= "00";
				m_addr_lat <= (Others => '0');
				m_data_lat <= (Others => '0');
				m_we_lat <= '0';
			Else
				--Inteconnect selected and no active slave. Latch relevant signals
				If ((interconnect_selected = '1') And (slave_select_lat = "00")) Then
					slave_select_lat <= slave_select;
					m_addr_lat <= m_i_wb_addr;
					m_data_lat <= m_i_wb_data;
					m_we_lat <= m_i_wb_we;
					stb_lat <= '1';
				End If;

				--Transaction finished acknowledgement from the selected peripheral
				If ((slave_select_lat = "01" And s0_i_wb_ack = '1') Or
					(slave_select_lat = "10" And s1_i_wb_ack = '1') Or
					m_i_wb_cyc = '0') Then
					slave_select_lat <= "00";
					stb_lat <= '0';
					--m_o_wb_ack <= '1';
				End If;
			End If;
		End If;
	End Process;

	--We can forward data over all signals to the peripherals except cycle and strobe (as they gate the transaction)
	--When a transaction is active, use the latched address/data/we so the slave sees stable inputs until ACK
	--Not using these give a timing error 
	slave_addr <= m_addr_lat When (slave_select_lat /= "00") Else m_i_wb_addr;
	slave_data <= m_data_lat When (slave_select_lat /= "00") Else m_i_wb_data;
	slave_we <= m_we_lat When (slave_select_lat /= "00") Else m_i_wb_we;

	s0_o_wb_we <= m_we_lat;
	s0_o_wb_addr <= m_addr_lat;
	s0_o_wb_data <= m_data_lat;
	--s0 stall is 0

	s1_o_wb_we <= m_we_lat;
	s1_o_wb_addr <= m_addr_lat;
	s1_o_wb_data <= m_data_lat;
	--s1 stall is 0

	--Set peripheral cycle and strobe signals
	--Can be put in a process block
	s0_o_wb_cyc <= '1' When (slave_select_lat = "01" And stb_lat = '1') Else '0';
	s0_o_wb_stb <= '1' When (slave_select_lat = "01" And stb_lat = '1') Else '0';
	s1_o_wb_cyc <= '1' When (slave_select_lat = "10" And stb_lat = '1') Else '0';
	s1_o_wb_stb <= '1' When (slave_select_lat = "10" And stb_lat = '1') Else '0';

	--Return path
	--Process(all) block for neatness (and because I finally figured out how to enable VHDL 2008)
	Process (All) Is
	Begin
		Case slave_select_lat Is
			When "01" =>
				m_o_wb_data <= s0_i_wb_data;
				m_o_wb_ack <= s0_i_wb_ack;
				m_o_wb_stall <= s0_i_wb_stall;
			When "10" =>
				m_o_wb_data <= s1_i_wb_data;
				m_o_wb_ack <= s1_i_wb_ack;
				m_o_wb_stall <= s1_i_wb_stall;
			When Others =>
				m_o_wb_data <= (Others => '0');
				m_o_wb_ack <= '0';
				m_o_wb_stall <= '0';
		End Case;
	End Process;

End Architecture;