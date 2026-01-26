Library ieee;
Use ieee.std_logic_1164.All;
Use ieee.numeric_std.All;

Library work;

use work.ov5640_image_buffer.All;

Entity wb_ov5640 Is
	Generic (
		BASE_ADDRESS              			: Std_ulogic_vector(31 Downto 0) := x"90010000"; --peripheral base (informational)
		CAMERA_CONTROL_ADDRESS            	: Std_ulogic_vector(31 Downto 0) := x"90010000"; --Camera control register. [0] = enable, [1] = reset
		CAMERA_STATUS_ADDRESS           	: Std_ulogic_vector(31 Downto 0) := x"90010004"; --Camera status register. [0]=busy, [1]=done (sticky)
		IMAGE_FORMAT_ADDRESS           	: Std_ulogic_vector(31 Downto 0) := x"90010008"; --Image format. [0] = 1 for YUV422. (Lowest3 bits can be used to select the format)
		IMAGE_RESOLUTION_ADDRESS           : Std_ulogic_vector(31 Downto 0) := x"9001000C"; --[15:0] = image width. [31:16] = image height
		MASTER_WORDS_TO_READ_ADDRESS				: Std_ulogic_vector(31 Downto 0) := x"90010010"; --32-bit words the master has to read to gather the complete image
		IMAGE_BUFFER_BASE			   			: Std_ulogic_vector(31 Downto 0) := x"90011000" --Image buffer base address
	);
	Port (
		clk        : In  Std_ulogic; --system clock
		reset      : In  Std_ulogic; --synchronous reset
		i_wb_cyc   : In  Std_ulogic; --Wishbone: cycle valid
		i_wb_stb   : In  Std_ulogic; --Wishbone: strobe
		i_wb_we    : In  Std_ulogic; --Wishbone: 1=write, 0=read
		i_wb_addr  : In  Std_ulogic_vector(31 Downto 0);--Wishbone: address
		i_wb_data  : In  Std_ulogic_vector(31 Downto 0);--Wishbone: write data
		o_wb_ack   : Out Std_ulogic; --Wishbone: acknowledge
		o_wb_stall : Out Std_ulogic; --Wishbone: stall (always '0')
		o_wb_data  : Out Std_ulogic_vector(31 Downto 0); --Wishbone: read data
		--Interface for the camera harware
		SIO_C		: InOut std_ulogic;	--SIO_C - SCCB clock signal. FPGA -> Camera
		SIO_D		: InOut std_ulogic;	--SIO_D - SCCB data signal (bi-direcctional). FPGA <--> Camera
		VSYNC	 	: In Std_ulogic;	--Camera VSYNC signal
		HREF		: In Std_ulogic;	--Camera HREF signal
		PCLK		: In Std_ulogic;	--Camera PCLK signal
		Data		: In Std_ulogic_vector (7 downto 0) --Camera data out pins
		
	);
End Entity;

--2-wire SCCB is similar to I2C apparently. SIO_C = SCL and SIO_D = SDA
--There is an exception. SCCB transmission uses 9-bits. 9th bit is a waste
--The SCCB document talks about using a tr
Architecture rtl Of wb_ov5640 Is
	--Main registers
	signal camera_control_reg 	: Std_ulogic_vector(31 Downto 0) := (Others => '0');
	signal camera_status_reg	: Std_ulogic_vector(31 Downto 0) := (Others => '0');
	signal image_format_reg	: Std_ulogic_vector(31 Downto 0) := (Others => '0');
	signal image_resolution_reg: Std_ulogic_vector(31 Downto 0) := (Others => '0');
	signal master_words_to_read_reg: Std_ulogic_vector(31 Downto 0) := (Others => '0');
	
	--Image buffer
	Signal image_buffer : tensor_mem_type := (Others => (Others => '0'));
	Signal image_buffer_wb_rdata	: std_ulogic_vector(31 Downto 0) := (Others => '0');
	--BRAM inference hints
	Attribute ram_style : String;
	Attribute syn_ramstyle : String;
	Attribute ram_style Of image_buffer : Signal Is "block";
	Attribute syn_ramstyle Of image_buffer : Signal Is "block_ram";
	
	--Camera interface registers
	--SCCB
	Signal SCCB_device_addr : Std_ulogic_vector(15 Downto 0) := (Others => '0');
	Signal SCCB_reg_addr : Std_ulogic_vector(15 Downto 0) := (Others => '0');
	Signal SCCB_device_data : Std_ulogic_vector(7 Downto 0) := (Others => '0');
	--Other latches
	signal vsync_lat : std_ulogic;
	signal href_lat : std_ulogic;
	signal pclk_lat: std_ulogic;
	signal data_lat: std_ulogic_vector (7 downto 0) := (others => '0');
	
	--Wishbone
	Signal ack_r : Std_ulogic := '0';
	Signal wb_req : Std_ulogic := '0'; --Variable tp combine checks (Clock is high and the slave (NPU) is selected)

	--Only allow the CPU to access tensor windows when the NPU is idle.
	--The CPU can still poll the NPU to check if it is busy.
	--
	--To make BRAM inference easier, each tensor memory is written/read from a single clocked process
	--and we multiplex the memory port between WB (when idle) and NPU (when busy).
	Signal camera_busy : Std_ulogic := '0';

	--Wishbone read mux selector (latched for the transaction being acknowledged)
	--000: register readback
	--001: image buffer window
	Signal wb_rsel : Std_ulogic_vector(2 Downto 0) := (Others => '0'); --select signal for tensor mux
	Signal reg_rdata : Std_ulogic_vector(31 Downto 0) := (Others => '0');

	--Address helper: translate byte address to word offset within a tensor window
	Function get_tensor_offset(addr, base : Std_ulogic_vector(31 Downto 0)) Return Natural Is
		Variable offset : unsigned(31 Downto 0);
	Begin
		offset := unsigned(addr) - unsigned(base); --word + byte offset (relative position of element from base address)
		--return to_integer(offset(11 downto 2));        --just the word offset
		Return to_integer(shift_right(offset, 2)); --Right shift 2 removes they byte offset within a word. We are left with just the word index
	End Function;
	
Begin

	--Simple, non-stalling slave peripheral
	o_wb_stall <= '0';
	o_wb_ack <= ack_r;
	--Select the camera
	wb_req <= i_wb_cyc And i_wb_stb; --Clock is high and the slave (NPU) is selected

	--The acknowledgement process is combined with the tensor multiplex select logic and register reads
	Process (clk)
		Variable is_valid : Std_ulogic;
		Variable is_tensor : Std_ulogic;

	Begin
		If (rising_edge(clk)) Then
			If (reset = '1') Then
				ack_r <= '0';
				wb_rsel <= (Others => '0');
				reg_rdata <= (Others => '0');
			Else
				ack_r <= '0';
				If (wb_req = '1') Then
					--Default
					is_valid := '0';
					is_tensor := '0';
					wb_rsel <= (Others => '0');
					reg_rdata <= (Others => '0');

					--Register reads
					If (i_wb_addr = CAMERA_CONTROL_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= camera_control_reg;
					Elsif (i_wb_addr = CAMERA_STATUS_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= camera_status_reg;
					Elsif (i_wb_addr = IMAGE_FORMAT_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= image_format_reg;
					Elsif (i_wb_addr = IMAGE_RESOLUTION_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= image_resolution_reg;
					Elsif (i_wb_addr = MASTER_WORDS_TO_READ_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= master_words_to_read_reg;
					--Tensor windows are valid only when idle (npu_busy='0')
					Elsif (unsigned(i_wb_addr) >= unsigned(IMAGE_BUFFER_BASE) And
						unsigned(i_wb_addr) < unsigned(IMAGE_BUFFER_BASE) + to_unsigned(TENSOR_BYTES, 32)) Then
						is_valid := '1';
						is_tensor := '1';
						wb_rsel <= "001";

					--Gate image buffer while camera is busy
					If (is_valid = '1') Then
						If (is_tensor = '1' And camera_busy = '1') Then
							ack_r <= '0';
						Else
							ack_r <= '1';
						End If;
					End If;
				End If;
			End If;
		End If;
		End if;
	End Process;

	With wb_rsel Select
		o_wb_data <= reg_rdata When "000",
		image_buffer_wb_rdata When Others;




	--Image buffer: WB read (when idle) + camera write
	--TODO: Camera write
	Process (clk)
		Variable tensor_offset : Natural;
		Variable w_index : Natural;
		Variable byte_sel : Natural Range 0 To 3;
		Variable word_tmp : Std_ulogic_vector(31 Downto 0);
	Begin
		If (rising_edge(clk)) Then
			If (reset = '1') Then
				image_buffer_wb_rdata <= (Others => '0');
			Else
				If (camera_busy = '0') Then
					--WB read port
					If (wb_req = '1' And
						unsigned(i_wb_addr) >= unsigned(IMAGE_BUFFER_BASE) And unsigned(i_wb_addr) < unsigned(IMAGE_BUFFER_BASE) + to_unsigned(TENSOR_BYTES, 32)) Then
						tensor_offset := get_tensor_offset(i_wb_addr, IMAGE_BUFFER_BASE);
						If (tensor_offset < TENSOR_WORDS) Then
							image_buffer_wb_rdata <= image_buffer(tensor_offset);
						Else
							image_buffer_wb_rdata <= (Others => '0');
						End If;
					End If;

				-- Else
				-- 	--NPU write
				-- 	If (state = S_P_WRITE) Then
				-- 		--Write a single signed int8 into the packed word at element index out_i_reg
				-- 		w_index := to_integer(out_i_reg(15 Downto 2));
				-- 		byte_sel := to_integer(out_i_reg(1 Downto 0));
				-- 		If (w_index < TENSOR_WORDS) Then
				-- 			word_tmp := tensor_R_mem(w_index);
				-- 			Case byte_sel Is
				-- 				When 0 => word_tmp(7 Downto 0) := Std_ulogic_vector(r8_reg);
				-- 				When 1 => word_tmp(15 Downto 8) := Std_ulogic_vector(r8_reg);
				-- 				When 2 => word_tmp(23 Downto 16) := Std_ulogic_vector(r8_reg);
				-- 				When Others => word_tmp(31 Downto 24) := Std_ulogic_vector(r8_reg);
				-- 			End Case;
				-- 			tensor_R_mem(w_index) <= word_tmp;
				-- 		End If;

				-- 	Elsif (state = S_DENSE_WRITE) Then
				-- 		--Write dense_result (signed int8) at element index out_i_reg
				-- 		w_index := to_integer(out_i_reg(15 Downto 2));
				-- 		byte_sel := to_integer(out_i_reg(1 Downto 0));
				-- 		If (w_index < TENSOR_WORDS) Then
				-- 			word_tmp := tensor_R_mem(w_index);
				-- 			Case byte_sel Is
				-- 				When 0 => word_tmp(7 Downto 0) := Std_ulogic_vector(dense_result);
				-- 				When 1 => word_tmp(15 Downto 8) := Std_ulogic_vector(dense_result);
				-- 				When 2 => word_tmp(23 Downto 16) := Std_ulogic_vector(dense_result);
				-- 				When Others => word_tmp(31 Downto 24) := Std_ulogic_vector(dense_result);
				-- 			End Case;
				-- 			tensor_R_mem(w_index) <= word_tmp;
				-- 		End If;

				-- 	Elsif (state = S_ACT_WRITE) Then
				-- 		--Softmax EXP is in-place on A, so only write to R for all other activation cases
				-- 		If (Not (op_code_reg = OP_SOFTMAX And softmax_mode_latched = '0')) Then
				-- 			If (to_integer(word_i_reg) < TENSOR_WORDS) Then
				-- 				tensor_R_mem(to_integer(word_i_reg)) <= r_w_reg;
				-- 			End If;
				-- 		End If;
				-- 	End If;
				 End If;
			End If;
		End If;
	End Process;

End Architecture;