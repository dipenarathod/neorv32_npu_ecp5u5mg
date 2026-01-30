Library ieee;
Use ieee.std_logic_1164.All;
Use ieee.numeric_std.All;

Library work;

Use work.ov5640_image_buffer.All;

Entity wb_ov5640 Is
	Generic (
		BASE_ADDRESS                    : Std_ulogic_vector(31 Downto 0) := x"90010000"; --peripheral base (informational)
		CAMERA_CONTROL_ADDRESS          : Std_ulogic_vector(31 Downto 0) := x"90010000"; --Camera control register. [0] = 1 from master, capture image. Peripheral sets it to 0 after capturing an image
		CAMERA_STATUS_ADDRESS           : Std_ulogic_vector(31 Downto 0) := x"90010004"; --Camera status register. [0] = 1 = busy, [1] = 1 = done
		IMAGE_FORMAT_ADDRESS            : Std_ulogic_vector(31 Downto 0) := x"90010008"; --Image format. [0] = 1 for YUV422. (Lowest3 bits can be used to select the format) (not used)
		IMAGE_RESOLUTION_ADDRESS        : Std_ulogic_vector(31 Downto 0) := x"9001000C"; --[15:0] = image width. [31:16] = image height
		MASTER_WORDS_TO_READ_ADDRESS    : Std_ulogic_vector(31 Downto 0) := x"90010010"; --32-bit words the master has to read to gather the complete image
		SCCB_PROGRAM_STATUS_REG_ADDRESS : Std_ulogic_vector(31 Downto 0) := x"90010014"; --Register to show SCCB programmer status. [0] = start latched. [1] = program started. [2] = wrapper busy. [3] = done. [4] = error 
		IMAGE_BUFFER_BASE               : Std_ulogic_vector(31 Downto 0) := x"90011000" --Image buffer base address
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
End Entity;

--2-wire SCCB is similar to I2C apparently. SIO_C = SCL and SIO_D = SDA
Architecture rtl Of wb_ov5640 Is
	--SCCB controller component
	Component sccb_i2c_wrapper Is
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
	End Component;

	--Main registers
	Signal camera_control_reg : Std_ulogic_vector(31 Downto 0) := (Others => '0');
	Signal camera_status_reg : Std_ulogic_vector(31 Downto 0) := (Others => '0');
	Signal image_format_reg : Std_ulogic_vector(31 Downto 0) := (Others => '0');
	Signal image_resolution_reg : Std_ulogic_vector(31 Downto 0) := (Others => '0');
	Signal master_words_to_read_reg : Std_ulogic_vector(31 Downto 0) := (Others => '0');
	Signal sccb_program_status_reg : Std_ulogic_vector(31 Downto 0) := (Others => '0');

	--Image buffer
	Signal image_buffer : tensor_mem_type := (Others => (Others => '0'));
	Signal image_buffer_wb_rdata : Std_ulogic_vector(31 Downto 0) := (Others => '0');
	--BRAM inference hints
	Attribute ram_style : String;
	Attribute syn_ramstyle : String;
	Attribute ram_style Of image_buffer : Signal Is "block";
	Attribute syn_ramstyle Of image_buffer : Signal Is "block_ram";

	--Registers to signal when data from camera can be written to the BRAM (and what data can be written). PCLK domain
	Signal cam_to_buffer_write_signal : Std_ulogic := '0'; --Signal from camera FSM to write to buffer
	Signal cam_to_buffer_index : unsigned(31 Downto 0) := (Others => '0'); --word index into the image buffer
	Signal cam_to_buffer_data : Std_ulogic_vector(31 Downto 0) := (Others => '0'); --Data to write at index 
	--Camera interface registers
	--SCCB controller
	--Signal SCCB_device_addr : Std_ulogic_vector(15 Downto 0) := (Others => '0');
	--Signal SCCB_reg_addr : Std_ulogic_vector(15 Downto 0) := (Others => '0');
	--Signal SCCB_device_data : Std_ulogic_vector(7 Downto 0) := (Others => '0');
	Signal start_lat : Std_ulogic := '0';
	Signal busy_lat : Std_ulogic;
	Signal done_lat : Std_ulogic;
	Signal err_lat : Std_ulogic;
	Signal sccb_boot_program_started : Std_ulogic := '0';
	--Other latches
	Signal vsync_lat : Std_ulogic;
	Signal href_lat : Std_ulogic;
	Signal pclk_lat : Std_ulogic;
	Signal data_lat : Std_ulogic_vector (7 Downto 0) := (Others => '0');

	-- Capture (PCLK domain)
	Type cap_state_t Is (CAP_IDLE, CAP_ARM, CAP_WAIT_ACTIVE, CAP_CAPTURE, CAP_DONE);
	Signal cap_state : cap_state_t := CAP_IDLE;

	Signal cap_busy_pclk : Std_ulogic := '0';
	Signal cap_done_pclk : Std_ulogic := '0';

	-- sync capture request into PCLK domain
	--Two stage synchoronizers are required to avoid metastability problems as we are operating with two different clock domains. 
	--The camera_control_register (0) is a flip-plop with timing windows around the system clock. Reading it near PCLK's edge may give unknown values
	--We first read the camera_control_register (0) and then store it in ff1. It may be unstable, but by when we read ff1 to store in ff2, there should be enough time for the flip-flop to be stable to store correct values in ff2.
	Signal cap_req_ff1 : Std_ulogic := '0';
	Signal cap_req_ff2 : Std_ulogic := '0';

	-- sync busy/done back into clk domain
	--Similar synchronizer logic as the cap_req registers
	Signal cap_busy_c1 : Std_ulogic := '0';
	Signal cap_busy_c2 : Std_ulogic := '0';
	Signal cap_done_c1 : Std_ulogic := '0';
	Signal cap_done_c2 : Std_ulogic := '0';
	Signal vsync_prev : Std_ulogic := '0';

	-- YUV422 byte phase and grayscale packing
	Signal yuv_phase : unsigned(1 Downto 0) := (Others => '0'); -- 0=Y0 1=U 2=Y1 3=V
	Signal y_word_buf : Std_ulogic_vector(31 Downto 0) := (Others => '0');
	Signal y_byte_idx : unsigned(1 Downto 0) := (Others => '0'); -- 0..3
	Signal y_word_index : unsigned(31 Downto 0) := (Others => '0');
	Signal y_pixel_count : unsigned(31 Downto 0) := (Others => '0');

	-- total pixels to capture
	--Similar synchronizer logic as the cap_req registers
	Signal total_pixels_clk : unsigned(31 Downto 0) := to_unsigned(MAX_DIM * MAX_DIM, 32);
	Signal total_pix_p1 : unsigned(31 Downto 0) := (Others => '0');
	Signal total_pix_p2 : unsigned(31 Downto 0) := (Others => '0');

	--Wishbone
	Signal ack_r : Std_ulogic := '0';
	Signal wb_req : Std_ulogic := '0'; --Variable tp combine checks (Clock is high and the slave (NPU) is selected)

	--Only read from the buffer over wishbone when the camera is not busy
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
	camera_busy <= cap_busy_c2; -- camera busy
	--SCCB component instantiation
	sccb_controller_inst : sccb_i2c_wrapper
	Port Map(
		clk   => clk,
		reset => reset,
		SIO_C => SIO_C,
		SIO_D => SIO_D,
		start => start_lat,
		busy  => busy_lat,
		done  => done_lat,
		err   => err_lat
	);

	Process (clk)
	Begin
		If rising_edge(clk) Then
			If (reset = '1') Then
				sccb_boot_program_started <= '0';
				start_lat <= '0';
			Else
				If (sccb_boot_program_started = '0') Then
					start_lat <= '1'; --request sccb wrapper to program the camera
					If (busy_lat = '1') Then --If wrapper is busy, then it has started
						start_lat <= '0'; --Deassert start latch input for wrapper
						sccb_boot_program_started <= '1'; --never request wrapper again

					End If;
				Else
					start_lat <= '0';
				End If;
			End If;
		End If;
	End Process;

	--Process to set bits of SCCB program status register. [0] = start latched. [1] = program started. [2] = wrapper busy. [3] = done. [4] = error
	Process (clk)
	Begin
		If rising_edge(clk) Then
			If (reset = '1') Then
				sccb_program_status_reg <= (Others => '0');
			Else
				If (start_lat = '1') Then
					sccb_program_status_reg (0) <= '1';
				End If;
				If (sccb_boot_program_started = '1') Then
					sccb_program_status_reg (1) <= '1';
				End If;
				If (busy_lat = '1') Then
					sccb_program_status_reg (2) <= '1';
				End If;
				If (done_lat = '1') Then
					sccb_program_status_reg (3) <= '1';
				End If;
				If (err_lat = '1') Then
					sccb_program_status_reg (4) <= '1';
				End If;
			End If;
		End If;
	End Process;

	-- Sync start request + total_pixels into PCLK domain
	Process (PCLK)
	Begin
		If rising_edge(PCLK) Then
			If reset = '1' Then
				cap_req_ff1 <= '0';
				cap_req_ff2 <= '0';
				total_pix_p1 <= (Others => '0');
				total_pix_p2 <= (Others => '0');
			Else
				cap_req_ff1 <= camera_control_reg(0);
				cap_req_ff2 <= cap_req_ff1;

				total_pix_p1 <= total_pixels_clk;
				total_pix_p2 <= total_pix_p1;
			End If;
		End If;
	End Process;

	-- Sync busy/done back to clk domain
	Process (clk)
	Begin
		If rising_edge(clk) Then
			If reset = '1' Then
				cap_busy_c1 <= '0';
				cap_busy_c2 <= '0';
				cap_done_c1 <= '0';
				cap_done_c2 <= '0';
			Else
				cap_busy_c1 <= cap_busy_pclk;
				cap_busy_c2 <= cap_busy_c1;
				cap_done_c1 <= cap_done_pclk;
				cap_done_c2 <= cap_done_c1;
			End If;
		End If;
	End Process;

	-- Update camera status + auto-clear control bit
	Process (clk)
	Begin
		If rising_edge(clk) Then
			If reset = '1' Then
				camera_status_reg <= (Others => '0');
			Else
				camera_status_reg(0) <= camera_busy; -- busy
				If cap_done_c2 = '1' Then
					camera_status_reg(1) <= '1'; -- done sticky
					--camera_control_reg(0) <= '0';    -- self-clear capture bit
					--Camera control reg is reset in Ada code
				End If;
			End If;
		End If;
	End Process;

	Process (PCLK)
		Variable buf_v : Std_ulogic_vector(31 Downto 0);
		Variable count_next : unsigned(31 Downto 0);
	Begin
		If rising_edge(PCLK) Then
			If reset = '1' Then
				cap_state <= CAP_IDLE;
				cap_busy_pclk <= '0';
				cap_done_pclk <= '0';
				vsync_prev <= '0';
				yuv_phase <= (Others => '0');
				y_word_buf <= (Others => '0');
				y_byte_idx <= (Others => '0');
				y_word_index <= (Others => '0');
				y_pixel_count <= (Others => '0');
			Else
				-- track VSYNC edge
				vsync_prev <= VSYNC;

				-- Default: clear write signal every cycle
				cam_to_buffer_write_signal <= '0';

				Case cap_state Is
					When CAP_IDLE =>
						cap_done_pclk <= '0';
						If (cap_req_ff2 = '1') Then
							cap_busy_pclk <= '1'; -- Assert busy immediately
							cap_state <= CAP_ARM;
						Else
							cap_busy_pclk <= '0';
						End If;

					When CAP_ARM =>
						cap_busy_pclk <= '1'; -- Keep busy asserted
						-- start on a clean frame boundary: wait for VSYNC rising edge
						If (vsync_prev = '0' And VSYNC = '1') Then
							cap_state <= CAP_WAIT_ACTIVE;
						End If;

					When CAP_WAIT_ACTIVE =>
						cap_busy_pclk <= '1'; -- Keep busy asserted
						-- wait until active frame begins (common: VSYNC goes low)
						If VSYNC = '0' Then
							yuv_phase <= (Others => '0');
							y_word_buf <= (Others => '0');
							y_byte_idx <= (Others => '0');
							y_word_index <= (Others => '0');
							y_pixel_count <= (Others => '0');
							cap_state <= CAP_CAPTURE;
						End If;

					When CAP_CAPTURE =>
						cap_busy_pclk <= '1'; -- Keep busy asserted

						-- end on VSYNC high (frame end)
						If VSYNC = '1' Then
							-- flush partial word if needed
							If (y_byte_idx /= "00") Then
								If to_integer(y_word_index) < TENSOR_WORDS Then
									cam_to_buffer_index <= y_word_index;
									cam_to_buffer_data <= y_word_buf; -- Use y_word_buf, not buf_v!
									cam_to_buffer_write_signal <= '1';
								End If;
							End If;
							-- Set done flags when transitioning
							cap_busy_pclk <= '0';
							cap_done_pclk <= '1';
							cap_state <= CAP_DONE;

						Else
							If HREF = '1' Then
								-- capture only Y bytes from YUYV (phase 0 and 2)
								If (yuv_phase = "00" Or yuv_phase = "10") Then
									buf_v := y_word_buf;

									Case y_byte_idx Is
										When "00" => buf_v(7 Downto 0) := Data;
										When "01" => buf_v(15 Downto 8) := Data;
										When "10" => buf_v(23 Downto 16) := Data;
										When Others => buf_v(31 Downto 24) := Data;
									End Case;

									count_next := y_pixel_count + 1;
									y_pixel_count <= count_next;

									-- store/advance packing
									If y_byte_idx = "11" Then
										If to_integer(y_word_index) < TENSOR_WORDS Then
											cam_to_buffer_index <= y_word_index;
											cam_to_buffer_data <= buf_v;
											cam_to_buffer_write_signal <= '1';
										End If;
										y_word_index <= y_word_index + 1;
										y_byte_idx <= (Others => '0');
										y_word_buf <= (Others => '0');
									Else
										y_byte_idx <= y_byte_idx + 1;
										y_word_buf <= buf_v;
									End If;

									-- stop when enough pixels captured
									If (count_next >= total_pix_p2) Then
										-- flush partial if we didn't write above
										If (y_byte_idx /= "11") Then
											If to_integer(y_word_index) < TENSOR_WORDS Then
												cam_to_buffer_index <= y_word_index;
												cam_to_buffer_data <= buf_v;
												cam_to_buffer_write_signal <= '1';
											End If;
										End If;
										-- Set done flags when transitioning
										cap_busy_pclk <= '0';
										cap_done_pclk <= '1';
										cap_state <= CAP_DONE;
									End If;
								End If;

								-- advance phase every byte
								yuv_phase <= yuv_phase + 1;
							Else
								-- re-align phase
								yuv_phase <= (Others => '0');
							End If;
						End If;

					When CAP_DONE =>
						cap_busy_pclk <= '0';
						cap_done_pclk <= '1';
						If cap_req_ff2 = '0' Then
							cap_done_pclk <= '0';
							cap_state <= CAP_IDLE;
						End If;
				End Case;
			End If;
		End If;
	End Process;
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
					Elsif (i_wb_addr = SCCB_PROGRAM_STATUS_REG_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= sccb_program_status_reg;
						--Tensor windows are valid only when idle (npu_busy='0')
					Elsif (unsigned(i_wb_addr) >= unsigned(IMAGE_BUFFER_BASE) And
						unsigned(i_wb_addr) < unsigned(IMAGE_BUFFER_BASE) + to_unsigned(TENSOR_BYTES, 32)) Then
						is_valid := '1';
						is_tensor := '1';
						wb_rsel <= "001";
					End If;
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
	End Process;

	--Wishbone register write process
	Process (clk)
	Begin
		If (rising_edge(clk)) Then
			If (reset = '1') Then
				--camera_control_reg <= (Others => '0');
				--camera_status_reg <= (Others => '0');
				image_format_reg <= (Others => '0');
				image_resolution_reg <= (Others => '0');
				master_words_to_read_reg <= (Others => '0');
			Else

				If (wb_req = '1' And i_wb_we = '1') Then
					--Registers are always writable (NPU busy status does not matter)
					If (i_wb_addr = CAMERA_CONTROL_ADDRESS) Then
						camera_control_reg <= i_wb_data;
					Elsif (i_wb_addr = CAMERA_STATUS_ADDRESS) Then
						Null; --Wishbone does not write to status register
					Elsif (i_wb_addr = IMAGE_FORMAT_ADDRESS) Then
						image_format_reg <= i_wb_data;
					Elsif (i_wb_addr = IMAGE_RESOLUTION_ADDRESS) Then
						image_resolution_reg <= i_wb_data;
					Elsif (i_wb_addr = MASTER_WORDS_TO_READ_ADDRESS) Then
						master_words_to_read_reg <= i_wb_data;
					End If;
				End If;
			End If;
		End If;
	End Process;

	With wb_rsel Select
		o_wb_data <= reg_rdata When "000",
		image_buffer_wb_rdata When Others;
	--Image buffer: WB read (when idle) + camera write
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
				End If;
			End If;
		End If;
	End Process;

	--PCLK camera write
	--ECP5 supports dual-clock BRAM  (many FPGAs do)
	--We write on PCLK
	Process (PCLK)
	Begin
		If (rising_edge(PCLK)) Then
			If (reset = '1') Then
				--cam_to_buffer_data <= (Others => '0');
				--cam_to_buffer_index <= (Others => '0');
			Else
				If (cam_to_buffer_write_signal = '1') Then
					If (to_integer(cam_to_buffer_index) < TENSOR_WORDS) Then
						image_buffer(to_integer(cam_to_buffer_index)) <= cam_to_buffer_data;
					End If;
				End If;
			End If;
		End If;
	End Process;
End Architecture;