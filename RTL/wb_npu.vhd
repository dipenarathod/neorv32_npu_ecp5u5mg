Library ieee;
Use ieee.std_logic_1164.All;
Use ieee.numeric_std.All;

Library work;
Use work.tensor_operations_base.All; --import word array sizes
Use work.tensor_operations_pooling.All; --import pooling opcodes & helpers (read/max/avg)
Use work.tensor_operations_activation.All;
Use work.tensor_operations_conv_dense.All;
--Revised address for tensors B, C, and R to allow addressing for the new 100x100 tensors (2500 words)
Entity wb_npu Is
	Generic (
		BASE_ADDRESS              : Std_ulogic_vector(31 Downto 0) := x"90000000"; --peripheral base (informational)
		TENSOR_A_BASE             : Std_ulogic_vector(31 Downto 0) := x"90000600"; --A window base
		TENSOR_B_BASE             : Std_ulogic_vector(31 Downto 0) := x"90002D10"; --B window base
		TENSOR_C_BASE             : Std_ulogic_vector(31 Downto 0) := x"9000B9B0"; --C window base
		TENSOR_R_BASE             : Std_ulogic_vector(31 Downto 0) := x"9000D8F0"; --R window base
		CTRL_REG_ADDRESS          : Std_ulogic_vector(31 Downto 0) := x"90000008"; --[0]=start, [5:1]=opcode
		STATUS_REG_ADDRESS        : Std_ulogic_vector(31 Downto 0) := x"9000000C"; --[0]=busy, [1]=done (sticky)
		DIM_REG_ADDRESS           : Std_ulogic_vector(31 Downto 0) := x"90000010"; --N (LSB 8 bits). Conv: input feature width
		POOL_BASE_INDEX_ADDRESS   : Std_ulogic_vector(31 Downto 0) := x"90000014"; --top-left index in A
		R_OUT_INDEX_ADDRESS       : Std_ulogic_vector(31 Downto 0) := x"90000018"; --out index in R
		WORD_INDEX_ADDRESS        : Std_ulogic_vector(31 Downto 0) := x"9000001C"; --word index for tensor indexing
		SUM_REG_ADDRESS           : Std_ulogic_vector(31 Downto 0) := x"90000020"; --Softmax sum parameter (write-only)
		SOFTMAX_MODE_ADDRESS      : Std_ulogic_vector(31 Downto 0) := x"90000024"; --Softmax mode: 0=EXP, 1=DIV
		WEIGHT_BASE_INDEX_ADDRESS : Std_ulogic_vector(31 Downto 0) := x"90000028"; --Dense: weight base index in B
		BIAS_INDEX_ADDRESS        : Std_ulogic_vector(31 Downto 0) := x"9000002C"; --Dense: bias word index in C
		N_INPUTS_ADDRESS          : Std_ulogic_vector(31 Downto 0) := x"90000030"; --Dense: number of inputs N. Conv: Number of input channels
		ZERO_POINT_REG_ADDRESS    : Std_ulogic_vector(31 Downto 0) := x"9000003C"; --Zero-point register
		QUANTIZED_MULTIPLIER_REG_ADDRESS    : Std_ulogic_vector(31 Downto 0) := x"90000040";  --Quantized multiplier
		QUANTIZED_MULTIPLIER_RIGHT_SHIFT_REG_ADDRESS    : Std_ulogic_vector(31 Downto 0) := x"90000044";  --Right shift for Quantized multiplier
		N_OUTPUTS_ADDRESS 		: Std_ulogic_vector(31 Downto 0) := x"90000048";  --Dense: Nuimber of output neurons. Conv: Number of output channels
		WORDS_TO_COPY_FROM_R_TO_A_ADDRESS 		: Std_ulogic_vector(31 Downto 0) := x"9000004C";  --total words to copy from R to A
		REQUANT_PROD_HI_ADDRESS 		: Std_ulogic_vector(31 Downto 0) := x"90000050";  --total words to copy from R to A
		REQUANT_PROD_LO_ADDRESS 		: Std_ulogic_vector(31 Downto 0) := x"90000054";  --total words to copy from R to A
		REQUANT_RESULT_32_ADDRESS 		: Std_ulogic_vector(31 Downto 0) := x"9000005C";  --total words to copy from R to A
		REQUANT_RESULT_8_ADDRESS 		: Std_ulogic_vector(31 Downto 0) := x"90000060";  --total words to copy from R to A
		ACCUMULATOR_ADDRESS 		: Std_ulogic_vector(31 Downto 0) := x"90000064"  --total words to copy from R to A

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
		o_wb_data  : Out Std_ulogic_vector(31 Downto 0) --Wishbone: read data
	);
End Entity;

Architecture rtl Of wb_npu Is

	Constant OP_COPY_R_TO_A : Std_ulogic_vector(4 Downto 0) := "11110";
	Constant OP_NOP : Std_ulogic_vector(4 Downto 0) := "11111";

	--Wishbone
	Signal ack_r : Std_ulogic := '0';
	Signal wb_req : Std_ulogic := '0'; --Variable tp combine checks (Clock is high and the slave (NPU) is selected)

	--Only allow the CPU to access tensor windows when the NPU is idle.
	--The CPU can still poll the NPU to check if it is busy.
	--
	--To make BRAM inference easier, each tensor memory is written/read from a single clocked process
	--and we multiplex the memory port between WB (when idle) and NPU (when busy).
	Signal npu_busy : Std_ulogic := '0';

	--Wishbone read mux selector (latched for the transaction being acknowledged)
	--000: register readback
	--001: tensor_A window
	--010: tensor_B window
	--011: tensor_C window
	--100: tensor_R window
	Signal wb_rsel : Std_ulogic_vector(2 Downto 0) := (Others => '0'); --select signal for tensor mux
	Signal reg_rdata : Std_ulogic_vector(31 Downto 0) := (Others => '0');

	--Tensors
	Signal tensor_A_mem : tensor_A_mem_type := (Others => (Others => '0'));
	Signal tensor_B_mem : tensor_B_mem_type := (Others => (Others => '0'));
	Signal tensor_C_mem : tensor_C_mem_type := (Others => (Others => '0'));
	Signal tensor_R_mem : tensor_R_mem_type := (Others => (Others => '0'));

	--BRAM inference hints
	Attribute ram_style : String;
	Attribute syn_ramstyle : String;

	Attribute ram_style Of tensor_A_mem : Signal Is "block";
	Attribute ram_style Of tensor_B_mem : Signal Is "block";
	Attribute ram_style Of tensor_C_mem : Signal Is "block";
	Attribute ram_style Of tensor_R_mem : Signal Is "block";

	Attribute syn_ramstyle Of tensor_A_mem : Signal Is "block_ram";
	Attribute syn_ramstyle Of tensor_B_mem : Signal Is "block_ram";
	Attribute syn_ramstyle Of tensor_C_mem : Signal Is "block_ram";
	Attribute syn_ramstyle Of tensor_R_mem : Signal Is "block_ram";

	--Read data for Wishbone access to tensors (valid when wb_rsel selects them for reading)
	Signal tensor_A_wb_rdata : Std_ulogic_vector(31 Downto 0) := (Others => '0');
	Signal tensor_B_wb_rdata : Std_ulogic_vector(31 Downto 0) := (Others => '0');
	Signal tensor_C_wb_rdata : Std_ulogic_vector(31 Downto 0) := (Others => '0');
	Signal tensor_R_wb_rdata : Std_ulogic_vector(31 Downto 0) := (Others => '0');

	--NPU-side BRAM ports (synchronous read, 1-cycle latency)
	--16-bit addresses can address 2^16 = 64KB worth of memort. Change width to more depending on needs
	Signal tensor_A_npu_addr : unsigned(15 Downto 0) := (Others => '0');
	Signal tensor_A_npu_rdata : Std_ulogic_vector(31 Downto 0) := (Others => '0');

	Signal tensor_B_npu_addr : unsigned(15 Downto 0) := (Others => '0');
	Signal tensor_B_npu_rdata : Std_ulogic_vector(31 Downto 0) := (Others => '0');

	Signal tensor_C_npu_addr : unsigned(15 Downto 0) := (Others => '0');
	Signal tensor_C_npu_rdata : Std_ulogic_vector(31 Downto 0) := (Others => '0');

	Signal tensor_R_npu_addr : unsigned(15 Downto 0) := (Others => '0');
	Signal tensor_R_npu_rdata : Std_ulogic_vector(31 Downto 0) := (Others => '0');

	--Control and status registers
	Signal ctrl_reg : Std_ulogic_vector(31 Downto 0) := (Others => '0'); --[0]=start, [5:1]=opcode
	Signal status_reg : Std_ulogic_vector(31 Downto 0) := (Others => '0'); --[0]=busy, [1]=done
	Signal dim_side_len_8 : Std_ulogic_vector(7 Downto 0) := (Others => '0'); --N side length
	Signal dim_side_len_bus : Std_ulogic_vector(31 Downto 0) := (Others => '0'); --zero-extended N

	--Pooling address parameters
	Signal pool_base_index : Std_ulogic_vector(31 Downto 0) := (Others => '0'); --A flat index (top-left)
	Signal pool_row_start_index : unsigned (15 downto 0) := (Others => '0');
	Signal pool_out_dim : unsigned (7 downto 0) := (Others => '0');
	Signal pool_out_row : unsigned (7 downto 0) := (Others => '0');
	Signal pool_out_col : unsigned (7 downto 0) := (Others => '0');
	Signal r_out_index : Std_ulogic_vector(31 Downto 0) := (Others => '0'); --R flat index

	--Elementwise word index
	Signal word_index_reg : Std_ulogic_vector(31 Downto 0) := (Others => '0'); --packed word index

	--Softmax parameters (write-only from Ada)
	Signal sum_param_reg : Std_ulogic_vector(31 Downto 0) := (Others => '0'); --Sum calculated by Ada
	Signal softmax_mode_reg : Std_ulogic_vector(31 Downto 0) := (Others => '0'); --Flag to differ between exponent and div mode. 0=EXP, 1=DIV
	--Using anpther opcode is possible, but I (DAR) don't suggest wasting an opcode


	--Start edge detection (one-cycle pulse)
	--ctrl0_prev is introduced to ensure a new command is not triggered every cycle (when ctrl is set)
	Signal start_cmd : Std_ulogic := '0';
	Signal ctrl0_prev : Std_ulogic := '0';

	--Muxed write paths for DIM (allowing bus or internal updates)
	--Will be useful when there is a dedicated pooling/conv unit
	Signal bus_dim_we : Std_ulogic := '0';
	Signal bus_dim_data : Std_ulogic_vector(7 Downto 0) := (Others => '0');
	Signal pool_dim_we : Std_ulogic := '0';
	Signal pool_dim_data : Std_ulogic_vector(7 Downto 0) := (Others => '0');

	--Dense layer operation registers
	Signal weight_base_index : Std_ulogic_vector(31 Downto 0) := (Others => '0'); --Weight base element index in B
	Signal bias_index : Std_ulogic_vector(31 Downto 0) := (Others => '0'); --Bias element index in C
	Signal n_inputs_reg : Std_ulogic_vector(31 Downto 0) := (Others => '0'); --Number of inputs for dense layer
	Signal weight_base_reg : unsigned(15 Downto 0) := (Others => '0'); --weight base element index
	Signal bias_index_reg : unsigned(15 Downto 0) := (Others => '0'); --bias element index
	Signal n_inputs_lat : unsigned(15 Downto 0) := (Others => '0'); --number of inputs
	Signal mac_counter : unsigned(15 Downto 0) := (Others => '0'); --MAC loop counter
	Signal accumulator : signed(31 Downto 0) := (Others => '0'); --32-bit accumulator
	Signal bias_val_reg : signed(31 Downto 0) := (Others => '0'); --bias value. ALSO USED FOR CONV
	Signal prod	:	signed(63 downto 0);	--Intermediate product from dense_requantize
	Signal requantize_32 : signed(31 downto 0);	--Shift product according to requantization

	Signal input_byte_sel_lat : Natural Range 0 To 3; --input word byte offset (extracted from current_input_index). Reused for Conv
	Signal weight_byte_sel_lat : Natural Range 0 To 3; --weight word byte offset (extracted from current weight index). Reused for Conv

	Signal dense_lane_count : unsigned(2 Downto 0) := (Others => '0'); --how many lanes (pairs) of inputs and weights are valid this iteration (without crossing word boundary or into other neuron's weights) Upto 4

	Signal dense_output_index : unsigned(15 downto 0) := (Others => '0');



	--Copy R to A signals
	signal total_words_to_copy_reg : Std_ulogic_vector(31 Downto 0) := (Others => '0');	--total words to copy from R to A
	signal total_words_to_copy_lat : unsigned(31 Downto 0) := (Others => '0');	
	signal total_words_in_tensor : unsigned(31 Downto 0) := (Others => '0');	--total word elements in input/ouptut tensor 
	signal total_int8_elements_in_tensor : unsigned(31 Downto 0) := (Others => '0');	--total int8 elements in input/output tensor
																							--indices to move between start of consecutive input channels for conv

	--Pooling datapath registers (2x2 window and result)
	Signal num00_reg, num01_reg, num10_reg, num11_reg : signed(7 Downto 0) := (Others => '0');
	Signal r_8_reg : signed(7 Downto 0) := (Others => '0');

	--Registers for packed word operations
	Signal r_w_reg : Std_ulogic_vector(31 Downto 0) := (Others => '0');

	Signal read_index : unsigned(1 Downto 0) := (Others => '0');
	Signal read_index_lat : unsigned(1 Downto 0) := (Others => '0'); --latch variant for read_index
	Signal byte_sel_lat : unsigned(1 Downto 0) := (Others => '0'); --latch variant for byte_sel

	--Quantization helper registers
	Signal zero_point : std_ulogic_vector(31 downto 0) := (Others => '0');
	Signal quantized_multiplier : std_ulogic_vector(31 downto 0) := (Others => '0'); --(lhs_scale * rhs_scale / result_scale) from GEMMlowp's equation 5 is a real number. This multiplier register holds the quanztized version of the real multipler
	Signal quantized_multiplier_right_shift : std_ulogic_vector(31 downto 0) := (Others => '0'); --right shifs required to convert quantized multiplier to the real multiplier
	
	Signal zero_point_lat : signed(31 downto 0) := (Others => '0'); --zero point value latched
	Signal quantized_multiplier_lat : signed(31 downto 0) := (Others => '0'); --quantized multipier latched
	Signal quantized_multiplier_right_shift_lat : unsigned(7 downto 0) := (Others => '0'); --right shift latched
	
	--Conv helper registers

	signal n_outputs_reg : Std_ulogic_vector(31 Downto 0) := (Others => '0'); --number of output channels
	Signal n_outputs_lat : unsigned(15 Downto 0) := (Others => '0'); 
	signal conv_kernel_row : unsigned(1 downto 0) := (Others => '0');	--kernel row we are processing
	signal conv_kernel_col : unsigned(1 downto 0) := (Others => '0');	--kernel col we are processing
	signal conv_input_channel: unsigned (7 downto 0) := (Others => '0');	--Number of the input channel being processed
	signal conv_row_ptr : unsigned(15 Downto 0) := (Others => '0');	--Flat index of the first element in current row of input tensor/feature (index into A)
	signal conv_weight_counter : unsigned(31 Downto 0) := (Others => '0');	--Flat index in the kernel we are multiplying (in B). Go from 0 to 9
	
	Signal conv_lane_count : unsigned (2 downto 0);	--How many lanes (pairs) of input channel and filters can we process this iteration without crossing boundary/into the next input or filter (upto 4)
														--Same as dense logic
	Signal conv_filter_index : unsigned(15 downto 0);		--Keep track of the filter number we are processing
	Signal filter_weight_base : unsigned (31 downto 0);	--starting weight element index in tensor B for the current filter
	Signal filter_stride	: unsigned (15 downto 0);	--stride to jump from k'th filter to (k+1)'th filter (int8 jump, not word index jump)
	Signal out_filter_stride : unsigned (15 downto 0);	--stride to jump from where to write result for k'th filter to where to write result for(k+1)'th filter (int8 jump, not word index jump) 
	signal conv_out_row        : unsigned(7 downto 0) := (others => '0'); --current output row
	signal conv_out_col        : unsigned(7 downto 0) := (others => '0'); --current output col
	signal conv_n_out          : unsigned(7 downto 0) := (others => '0'); --N - 2 (output side length)
	signal conv_pixel_out_base : unsigned(15 downto 0) := (others => '0'); --output index for current pixel
	--Address helper: translate byte address to word offset within a tensor window
	Function get_tensor_offset(addr, base : Std_ulogic_vector(31 Downto 0)) Return Natural Is
		Variable offset : unsigned(31 Downto 0);
	Begin
		offset := unsigned(addr) - unsigned(base); --word + byte offset (relative position of element from base address)
		--return to_integer(offset(11 downto 2));        --just the word offset
		Return to_integer(shift_right(offset, 2)); --Right shift 2 removes they byte offset within a word. We are left with just the word index
	End Function;

	--Unified FSM state encoding
	Type state_t Is (
		S_IDLE, S_CAPTURE, S_PREPARE_PRODUCTS, S_OP_CODE_BRANCH,
		--Copy R to A
		S_COPY_R_REQ, S_COPY_R_WAIT, S_COPY_R_CAP, S_COPY_A_WRITE,
		--pooling path states (added a new state, request, to make BRAM inference possible)
		S_P_READ_REQ, S_P_READ_WAIT, S_P_READ_CAP, S_P_CALC, S_P_WRITE,
		--Activation path states
		S_ACT_READ_REQ, S_ACT_READ_WAIT, S_ACT_CALC, S_ACT_WRITE,
		--Dense path states
		S_DENSE_INIT, S_DENSE_BIAS_READ, S_DENSE_BIAS_WAIT, S_DENSE_FETCH, S_DENSE_FETCH_WAIT, S_DENSE_MAC, S_DENSE_REQUANT_PRODUCT, S_DENSE_REQUANT_SHIFT, S_DENSE_REQUANT_CLAMP, S_DENSE_WRITE,
		--Conv path states
		S_CONV_PREP, S_CONV_INIT, S_CONV_BIAS_READ, S_CONV_BIAS_WAIT, S_CONV_FETCH, S_CONV_FETCH_WAIT, S_CONV_MAC, S_CONV_REQUANT_PRODUCT, S_CONV_REQUANT_SHIFT, S_CONV_REQUANT_CLAMP, S_CONV_WRITE,
		S_DONE
	);
	Signal state : state_t := S_IDLE;

	--Latched operation parameters for the active command
	Signal op_code_reg : Std_ulogic_vector(4 Downto 0) := (Others => '0'); --opcode field
	Signal base_i_reg : unsigned(15 Downto 0) := (Others => '0'); --pooling base index
	Signal out_i_reg : unsigned(15 Downto 0) := (Others => '0'); --output index
	Signal din_reg : unsigned(7 Downto 0) := (Others => '0'); --N (tensor side length)
	Signal in_i_reg : unsigned(15 Downto 0) := (Others => '0'); --packed word index (int8-granular for dense, word-granular for act)
	Signal softmax_mode_latched : Std_ulogic := '0'; --latched softmax mode

Begin

	--Simple, non-stalling slave peripheral
	o_wb_stall <= '0';

	--Zero-extend N for bus readback
	dim_side_len_bus <= (31 Downto 8 => '0') & dim_side_len_8;

	--Expose NPU busy state
	npu_busy <= '1' When state /= S_IDLE Else '0';
	status_reg(0) <= '1' When state /= S_IDLE Else '0';

	--Generate a one-cycle start pulse when start=1 and not busy
	--Only trigger an operation (start_cmd = 1) when ctrl(0) is transitioning to 1 for the first time and status(0) = 0 (not busy)
	Process (clk)
	Begin
		If (rising_edge(clk)) Then
			If (reset = '1') Then
				start_cmd <= '0';
				ctrl0_prev <= '0';
			Else
				start_cmd <= '0';
				If (npu_busy = '0' And ctrl_reg(0) = '1' And (ctrl0_prev = '0')) Then
					start_cmd <= '1';
				End If;
				ctrl0_prev <= ctrl_reg(0);
			End If;
		End If;
	End Process;

	--DIM (N) register with two write sources: pooling path or bus write
	Process (clk)
	Begin
		If (rising_edge(clk)) Then
			If (reset = '1') Then
				dim_side_len_8 <= x"32"; --default N=50.
			Else
				If (pool_dim_we = '1') Then
					dim_side_len_8 <= pool_dim_data; --TODO: When there is a dedicated pooling unit with variable window sizes
				Elsif (bus_dim_we = '1') Then
					dim_side_len_8 <= bus_dim_data; --bus write-update
				End If;
			End If;
		End If;
	End Process;

	--Tensor window accesses are only acknowledged when npu_busy=0
	--The CPU can always access control/status registers
	--This change is to allow BRAM usage (inference) for the main four tensors

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
					If (i_wb_addr = CTRL_REG_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= ctrl_reg;
					Elsif (i_wb_addr = STATUS_REG_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= status_reg;
					Elsif (i_wb_addr = DIM_REG_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= dim_side_len_bus;
					Elsif (i_wb_addr = POOL_BASE_INDEX_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= pool_base_index;
					Elsif (i_wb_addr = R_OUT_INDEX_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= r_out_index;
					Elsif (i_wb_addr = WORD_INDEX_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= word_index_reg;
					Elsif (i_wb_addr = SOFTMAX_MODE_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= softmax_mode_reg;
					Elsif (i_wb_addr = WEIGHT_BASE_INDEX_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= weight_base_index;
					Elsif (i_wb_addr = BIAS_INDEX_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= bias_index;
					Elsif (i_wb_addr = N_INPUTS_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= n_inputs_reg;
					Elsif (i_wb_addr = SUM_REG_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= sum_param_reg;
					Elsif (i_wb_addr = ZERO_POINT_REG_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= zero_point;
					Elsif (i_wb_addr = QUANTIZED_MULTIPLIER_REG_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= quantized_multiplier;
					Elsif (i_wb_addr = QUANTIZED_MULTIPLIER_RIGHT_SHIFT_REG_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= quantized_multiplier_right_shift; 						
					Elsif (i_wb_addr = N_OUTPUTS_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= n_outputs_reg; 
					Elsif (i_wb_addr = WORDS_TO_COPY_FROM_R_TO_A_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= total_words_to_copy_reg;
					Elsif (i_wb_addr = REQUANT_PROD_HI_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= std_ulogic_vector(prod(63 downto 32));
					Elsif (i_wb_addr = REQUANT_PROD_LO_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= std_ulogic_vector(prod(31 downto 0));
					Elsif (i_wb_addr = REQUANT_RESULT_32_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= std_ulogic_vector(requantize_32);
					Elsif (i_wb_addr = REQUANT_RESULT_8_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= std_ulogic_vector(resize(r_8_reg,32));
					Elsif (i_wb_addr = ACCUMULATOR_ADDRESS) Then
						is_valid := '1';
						reg_rdata <= std_ulogic_vector(accumulator);
					--Tensor windows are valid only when idle (npu_busy='0')
					Elsif (unsigned(i_wb_addr) >= unsigned(TENSOR_A_BASE) And
						unsigned(i_wb_addr) < unsigned(TENSOR_A_BASE) + to_unsigned(TENSOR_A_BYTES, 32)) Then
						is_valid := '1';
						is_tensor := '1';
						wb_rsel <= "001";

					Elsif (unsigned(i_wb_addr) >= unsigned(TENSOR_B_BASE) And
						unsigned(i_wb_addr) < unsigned(TENSOR_B_BASE) + to_unsigned(TENSOR_B_BYTES, 32)) Then
						is_valid := '1';
						is_tensor := '1';
						wb_rsel <= "010";

					Elsif (unsigned(i_wb_addr) >= unsigned(TENSOR_C_BASE) And
						unsigned(i_wb_addr) < unsigned(TENSOR_C_BASE) + to_unsigned(TENSOR_C_BYTES, 32)) Then
						is_valid := '1';
						is_tensor := '1';
						wb_rsel <= "011";

					Elsif (unsigned(i_wb_addr) >= unsigned(TENSOR_R_BASE) And
						unsigned(i_wb_addr) < unsigned(TENSOR_R_BASE) + to_unsigned(TENSOR_R_BYTES, 32)) Then
						is_valid := '1';
						is_tensor := '1';
						wb_rsel <= "100";
					End If;

					--Gate tensor ACKs while NPU is busy
					If (is_valid = '1') Then
						If (is_tensor = '1' And npu_busy = '1') Then
							ack_r <= '0';
						Else
							ack_r <= '1';
						End If;
					End If;
				End If;
			End If;
		End If;
	End Process;

	o_wb_ack <= ack_r;

	With wb_rsel Select
		o_wb_data <= reg_rdata When "000",
		tensor_A_wb_rdata When "001",
		tensor_B_wb_rdata When "010",
		tensor_C_wb_rdata When "011",
		tensor_R_wb_rdata When Others;
	--Wishbone register write process
	Process (clk)
	Begin
		If (rising_edge(clk)) Then
			If (reset = '1') Then
				ctrl_reg <= (Others => '0');
				pool_base_index <= (Others => '0');
				r_out_index <= (Others => '0');
				word_index_reg <= (Others => '0');
				sum_param_reg <= (Others => '0');
				softmax_mode_reg <= (Others => '0');
				weight_base_index <= (Others => '0');
				bias_index <= (Others => '0');
				n_inputs_reg <= (Others => '0');
				n_outputs_reg <= (Others => '0');
				bus_dim_we <= '0';
				bus_dim_data <= (Others => '0');
				zero_point <= (Others => '0');
				quantized_multiplier <= (Others => '0');
				quantized_multiplier_right_shift <= (Others => '0');
				total_words_to_copy_reg <= (Others => '0');
			Else
				bus_dim_we <= '0';

				If (wb_req = '1' And i_wb_we = '1') Then
					--Registers are always writable (NPU busy status does not matter)
					If (i_wb_addr = CTRL_REG_ADDRESS) Then
						ctrl_reg <= i_wb_data;
					Elsif (i_wb_addr = DIM_REG_ADDRESS) Then
						bus_dim_we <= '1';
						bus_dim_data <= i_wb_data(7 Downto 0);
					Elsif (i_wb_addr = POOL_BASE_INDEX_ADDRESS) Then
						pool_base_index <= i_wb_data;
					Elsif (i_wb_addr = R_OUT_INDEX_ADDRESS) Then
						r_out_index <= i_wb_data;
					Elsif (i_wb_addr = WORD_INDEX_ADDRESS) Then
						word_index_reg <= i_wb_data;
					Elsif (i_wb_addr = SUM_REG_ADDRESS) Then
						sum_param_reg <= i_wb_data; --Ada writes calculated sum before Pass 2 of SoftMax
					Elsif (i_wb_addr = SOFTMAX_MODE_ADDRESS) Then
						softmax_mode_reg <= i_wb_data; --Ada sets mode: 0=EXP, 1=DIV
					Elsif (i_wb_addr = WEIGHT_BASE_INDEX_ADDRESS) Then
						weight_base_index <= i_wb_data;
					Elsif (i_wb_addr = BIAS_INDEX_ADDRESS) Then
						bias_index <= i_wb_data;
					Elsif (i_wb_addr = N_INPUTS_ADDRESS) Then
						n_inputs_reg <= i_wb_data;
					Elsif (i_wb_addr = ZERO_POINT_REG_ADDRESS) Then
						zero_point <= i_wb_data;
					Elsif (i_wb_addr = QUANTIZED_MULTIPLIER_REG_ADDRESS) Then
						quantized_multiplier <= i_wb_data;
					Elsif (i_wb_addr = QUANTIZED_MULTIPLIER_RIGHT_SHIFT_REG_ADDRESS) Then
						quantized_multiplier_right_shift <= i_wb_data;
					Elsif (i_wb_addr = N_OUTPUTS_ADDRESS) Then
						n_outputs_reg <= i_wb_data;
					Elsif (i_wb_addr = WORDS_TO_COPY_FROM_R_TO_A_ADDRESS) Then
						total_words_to_copy_reg <= i_wb_data;						
					End If;
				End If;
			End If;
		End If;
	End Process;

	--Unified FSM process
	Process (clk)
		Variable current_input_index : unsigned(15 Downto 0); --flat index in A (word index and byte offset)
		Variable word_index : unsigned(15 Downto 0); --32-bit word index (unsigned)
		Variable byte_sel : unsigned(1 Downto 0); --byte lane select 0 to 3
		Variable packed_word : Std_ulogic_vector(31 Downto 0); --fetched 32-bit word
		Variable sel_byte : Std_ulogic_vector(7 Downto 0); --Byte selected from word during pooling
		
		Variable current_weight_index : unsigned(15 Downto 0); --current weight index in B for dense and conv (word index and byte offset)

		Variable input_word_index : unsigned(15 Downto 0); --input word index (extracted from current_input_index)
		Variable weight_word_index : unsigned(15 Downto 0); --weight word index (extracted from current_weight_index)

		--Variable input_byte_off : Natural Range 0 To 3; --input word byte offset (extracted from current_input_index)
		--Variable weight_byte_off : Natural Range 0 To 3; --weight word byte offset (extracted from current weight index)

		Variable input_shift_bits : Natural; --input byte offset converted to bits
		Variable weight_shift_bits : Natural; --weight byte offset converted to bits

		Variable input_word_shifted : Std_ulogic_vector(31 Downto 0);
		Variable weight_word_shifted : Std_ulogic_vector(31 Downto 0);
		
		--Lane logic variables (reused in conv)
		Variable remaining_u : unsigned(15 Downto 0); --inputs left for this neuron
		Variable remaining_i : Integer; --inputs left for this neuron (integer)
		Variable lanes_i : Integer; --lanes used for this iteration (max 4)
		Variable lanes_av_in : Integer; --input lanes available before crossiing into next word
		Variable lanes_av_wt : Integer; --weight lanes available (before crossing into next word)
		Variable lanes_u : unsigned(15 Downto 0); --lanes_i but unsigned
		--lanes help calculate how many inputs and weights for a neuron can be calculated
		Variable next_count : unsigned(15 Downto 0); --number of inputs left to be processed. Helps with deciding if we want to continue with mac state or if we can add
		--Variable prod	:	signed(63 downto 0);	--Intermediate product from dense_requantize
		--the bias
		--also, new value for mac_counter
		
		--Conv variables
		--Variable conv_input_byte_sel : unsigned(1 Downto 0); --byte lane select 0 to 3 for input feature pixel word
		--Variable conv_weight_byte_sel : unsigned(1 Downto 0); --byte lane select 0 to 3 for weight word
		--Reuse lane variables from dense
		Variable conv_cols_left_in_row    : Integer;	--columns left in this row of kernel
		Variable row_i	: Integer;  --Integer version of row index
		Variable col_i	: Integer;	 --Integer version of column index
		
	Begin
		If (rising_edge(clk)) Then
			If (reset = '1') Then
				state <= S_IDLE;
				--status_reg <= (others => '0');
				op_code_reg <= (Others => '0');
				base_i_reg <= (Others => '0');
				out_i_reg <= (Others => '0');
				din_reg <= (Others => '0');
				in_i_reg <= (Others => '0');
				softmax_mode_latched <= '0';

				weight_base_reg <= (Others => '0');
				bias_index_reg <= (Others => '0');
				n_inputs_lat <= (Others => '0');
				mac_counter <= (Others => '0');
				accumulator <= (Others => '0');
				bias_val_reg <= (Others => '0');

				dense_lane_count <= (Others => '0');
				dense_output_index <= (Others => '0');
				
				pool_out_dim <= (Others => '0');
				pool_row_start_index <= (Others => '0');
				pool_out_row <= (Others => '0');
				pool_out_col <= (Others => '0');
				num00_reg <= (Others => '0');
				num01_reg <= (Others => '0');
				num10_reg <= (Others => '0');
				num11_reg <= (Others => '0');
				r_8_reg <= (Others => '0');

				r_w_reg <= (Others => '0');

				read_index <= (Others => '0');
				read_index_lat <= (Others => '0');
				byte_sel_lat <= (Others => '0');

				pool_dim_we <= '0';

				tensor_A_npu_addr <= (Others => '0');
				tensor_B_npu_addr <= (Others => '0');
				tensor_C_npu_addr <= (Others => '0');
				
				zero_point_lat <= (Others => '0');
				quantized_multiplier_lat <= (Others => '0');
				quantized_multiplier_right_shift_lat <= (Others => '0');
				prod <= (Others => '0');
				
				total_words_in_tensor <= (Others => '0');
				total_int8_elements_in_tensor <= (Others => '0');
				n_outputs_lat <= (Others => '0');
				out_filter_stride <= (Others => '0');
				conv_n_out          <= (Others => '0');
				conv_out_row        <= (others => '0');
				conv_out_col        <= (others => '0');

			Else
				pool_dim_we <= '0';

				Case state Is
					When S_IDLE =>
						--status_reg(0) <= '0';                --not busy
						If (start_cmd = '1') Then
							status_reg(1) <= '0'; --clear done
							state <= S_CAPTURE; --capture parameters
						End If;

					When S_CAPTURE =>
						--status_reg(0) <= '1'; --The NPU is marked busy once the capture stage begins
						op_code_reg <= ctrl_reg(5 Downto 1);
						din_reg <= unsigned(dim_side_len_8);
						base_i_reg <= unsigned(pool_base_index(15 Downto 0));
						out_i_reg <= unsigned(r_out_index (15 Downto 0));
						
						in_i_reg <= unsigned(word_index_reg(15 Downto 0));
						
						read_index <= (Others => '0');
						
						softmax_mode_latched <= softmax_mode_reg(0); --Latch softmax mode
						weight_base_reg <= unsigned(weight_base_index(15 Downto 0));
						bias_index_reg <= unsigned(bias_index(15 Downto 0));
						n_inputs_lat <= unsigned(n_inputs_reg(15 Downto 0));
						dense_output_index <= (Others => '0');
						zero_point_lat <= signed(zero_point);
						quantized_multiplier_lat <= signed(quantized_multiplier);
						quantized_multiplier_right_shift_lat <= unsigned(quantized_multiplier_right_shift (7 downto 0));
						
						conv_filter_index <= (Others => '0');
						n_outputs_lat <= unsigned(n_outputs_reg(15 downto 0));
						total_words_to_copy_lat <= unsigned (total_words_to_copy_reg);
						state <= S_PREPARE_PRODUCTS;
						
					When S_PREPARE_PRODUCTS =>
						pool_row_start_index <= base_i_reg;
					    pool_out_dim <= shift_right(din_reg,1);
						total_int8_elements_in_tensor <= resize(din_reg * din_reg, 32);	--total int8 elements in input/output tensor
																				--In conv: indices to move to start of next input channel = side lengt of input channel * side lengt of input channel (all elements in input channel)
						filter_stride <= resize(n_inputs_lat * 9, 16); --Number of filters * (3x3) = indices to move to next filter. 3x3 = size of filter
						conv_n_out          <= din_reg - 2;
						state <= S_OP_CODE_BRANCH;
						
					When S_OP_CODE_BRANCH =>
						--Decode opcode and branch to appropriate datapath
						total_words_in_tensor <= shift_right(total_int8_elements_in_tensor, 2);	--total words in input/output tensor
						If (op_code_reg = OP_NOP) Then
							state <= S_DONE;
						Elsif (op_code_reg = OP_MAXPOOL) Or (op_code_reg = OP_AVGPOOL) Then
							pool_out_row <= (Others => '0');
							pool_out_col <= (Others => '0');
							state <= S_P_READ_REQ;
						Elsif (op_code_reg = OP_SIGMOID) Or (op_code_reg = OP_RELU) Or (op_code_reg = OP_SOFTMAX) Then
							state <= S_ACT_READ_REQ;
						Elsif (op_code_reg = OP_DENSE) Then
							state <= S_DENSE_INIT;
						Elsif (op_code_reg = OP_COPY_R_TO_A) Then
							state <= S_COPY_R_REQ;
						Elsif (op_code_reg = OP_CONV) Then
							state <= S_CONV_PREP;
						Else
							--status_reg(0) <= '0';
							status_reg(1) <= '1';
							state <= S_IDLE;
						End If;

						--Copy R to A states-------------------------------------------------
						--Set output_i_reg to 0 in Ada call. out_i_reg is used to index into both, A and R
						--Set words to copy in Ada. Those many words will be copied over from R to A
						--S_COPY_R_REQ, S_COPY_R_WAIT, S_COPY_R_CAP, S_COPY_A_WRITE,
					When S_COPY_R_REQ =>
						tensor_R_npu_addr <= resize(out_i_reg, tensor_R_npu_addr'length); --int32 (word) index
						state <= S_COPY_R_WAIT;
					When S_COPY_R_WAIT =>
						state <= S_COPY_R_CAP;
					When S_COPY_R_CAP =>
						packed_word := tensor_R_npu_rdata;
						in_i_reg <= out_i_reg; --index in A
						r_w_reg <= packed_word;
						state <= S_COPY_A_WRITE;
					When S_COPY_A_WRITE =>
						if (out_i_reg < total_words_to_copy_lat - 1) then
								out_i_reg <= out_i_reg + 1;
								state <= S_COPY_R_REQ;
						Else
							state <= S_DONE;	
						End if;
						
						--Pooling States------------------------------------------------------						
					When S_P_READ_REQ =>

						current_input_index := compute_pooling_flat_index_2x2(read_index, base_i_reg, din_reg);
						--Request BRAM read for tensor_A word
						word_index := resize(current_input_index(15 Downto 2), word_index'length);
						byte_sel := current_input_index(1 Downto 0);

						tensor_A_npu_addr <= resize(word_index, tensor_A_npu_addr'length);

						--Latch which byte and which slot this read corresponds to
						byte_sel_lat <= byte_sel;
						read_index_lat <= read_index;
						state <= S_P_READ_WAIT;

						--Wait a clock cycle for BRAM read to complete
					When S_P_READ_WAIT =>
						state <= S_P_READ_CAP;

					When S_P_READ_CAP =>
						--Consume BRAM data (available 1 cycle after address request)
						packed_word := tensor_A_npu_rdata;
						sel_byte := Std_ulogic_vector(
							shift_right(unsigned(packed_word), to_integer(byte_sel_lat) * 8)(7 Downto 0)
							);
						--Store into the appropriate register
						Case read_index_lat Is
							When "00" => num00_reg <= signed(sel_byte);
							When "01" => num01_reg <= signed(sel_byte);
							When "10" => num10_reg <= signed(sel_byte);
							When Others => num11_reg <= signed(sel_byte);
						End Case;

						--Advance or move to compute
						If (read_index_lat = "11") Then
							state <= S_P_CALC;
						Else
							read_index <= read_index_lat + 1;
							state <= S_P_READ_REQ;
						End If;

					When S_P_CALC =>
						--Pooling compute: avg or max across 2x2, result in r_8_reg
						If (op_code_reg = OP_AVGPOOL) Then
							r_8_reg <= avgpool4(num00_reg, num01_reg, num10_reg, num11_reg);
						Else
							r_8_reg <= maxpool4(num00_reg, num01_reg, num10_reg, num11_reg);
						End If;
						state <= S_P_WRITE;

					When S_P_WRITE =>
						if(pool_out_col < pool_out_dim - 1) Then
							pool_out_col   <= pool_out_col + 1; --Move to next column in result
							base_i_reg <= base_i_reg + 2;	--Base index + 2 = top-left of next 2x2 window
							out_i_reg  <= out_i_reg  + 1;	
							read_index     <= "00";
							state <= S_P_READ_REQ;
						Elsif (pool_out_row < pool_out_dim - 1) Then
							pool_out_col <= (Others => '0'); --Reset output column index
							pool_out_row <= pool_out_row + 1;	--Increment output row index
							base_i_reg <= pool_row_start_index + (2 * din_reg);
							pool_row_start_index <= pool_row_start_index + (2 * din_reg);
							out_i_reg  <= out_i_reg + 1;
							read_index     <= "00";
							state <= S_P_READ_REQ; 
						Else
							state <= S_DONE;
						End if;

						--Actiation states--------------------------------------------

					When S_ACT_READ_REQ =>
						--Request tensor_A word
						tensor_A_npu_addr <= resize(in_i_reg, tensor_A_npu_addr'length);
						state <= S_ACT_READ_WAIT;
						--Wait a clock cycle for BRAM read to complete
					When S_ACT_READ_WAIT =>
						state <= S_ACT_CALC;

					When S_ACT_CALC =>

						--Select function based on opcode and softmax mode
						If (op_code_reg = OP_RELU) Then
							r_w_reg <= relu_packed_word(tensor_A_npu_rdata);
						Elsif (op_code_reg = OP_SIGMOID) Then
							r_w_reg <= sigmoid_packed_word(tensor_A_npu_rdata);
						Elsif (op_code_reg = OP_SOFTMAX) Then
							--Determine softmax mode determine for finding exponent (pass 1) or division (pass 2)
							If (softmax_mode_latched = '0') Then
								--Exponent phase (Pass 1)
								r_w_reg <= softmax_exponent_packed_word(tensor_A_npu_rdata);
							Else
								--Division phase (divide by sum) (Pass 2)
								r_w_reg <= softmax_div_by_sum_packed_word(tensor_A_npu_rdata, unsigned(sum_param_reg(15 Downto 0)));
							End If;
						End If;

						state <= S_ACT_WRITE;

					When S_ACT_WRITE =>
							if(in_i_reg < n_inputs_lat - 1) then
								in_i_reg <= in_i_reg + 1;
								state <= S_ACT_READ_REQ;
							else
								state <= S_DONE;
							end if;

						--Dense states----------------------------------------------------------

					When S_DENSE_INIT =>
						--Request bias word from tensor C
						--tensor_C_npu_rdata is available one cycle after tensor_C_npu_addr is set
						tensor_C_npu_addr <= resize(bias_index_reg, tensor_C_npu_addr'length); --int32 bias (word) index
						state <= S_DENSE_BIAS_WAIT;
					When S_DENSE_BIAS_WAIT =>
						state <= S_DENSE_BIAS_READ;
					When S_DENSE_BIAS_READ =>
						--Bias word is available from BRAM, so we read it
						--byte_sel := bias_index_reg(1 Downto 0);
						packed_word := tensor_C_npu_rdata;
						--bias_val_reg <= extract_byte_from_word(packed_word, to_integer(byte_sel));
						bias_val_reg <= signed(packed_word);
						--Reset dense accumulators/counters after bias has been captured (prep for next neuron)
						accumulator <= (Others => '0');
						mac_counter <= (Others => '0');
						dense_lane_count <= (Others => '0');

						state <= S_DENSE_FETCH;

					When S_DENSE_FETCH =>
						--Fetch a packed group of inputs/weights from A and B for multiplication and accumulation

						--Current element indices
						current_input_index := in_i_reg + mac_counter;
						current_weight_index := weight_base_reg + mac_counter;

						input_word_index := resize(current_input_index(15 Downto 2), input_word_index'length);
						weight_word_index := resize(current_weight_index(15 Downto 2), weight_word_index'length);

						--Request words from tensors A and B
						--tensor_A_npu_rdata and tensor_B_npu_rdata are available in the next cycle
						tensor_A_npu_addr <= resize(input_word_index, tensor_A_npu_addr'length);
						tensor_B_npu_addr <= resize(weight_word_index, tensor_B_npu_addr'length);

						--Byte offset inside the packed word
						input_byte_sel_lat <= to_integer(current_input_index(1 Downto 0));
						weight_byte_sel_lat <= to_integer(current_weight_index(1 Downto 0));

						--Compute how many lanes we can safely process this step
						--Don't want exceed remaining inputs
						--Don't want to cross a 32-bit word boundary
						remaining_u := n_inputs_lat - mac_counter; --remaining inputs to process = total inputs to process - inputs pricessed already
						remaining_i := to_integer(remaining_u);

						lanes_av_in := 4 - to_Integer(current_input_index(1 Downto 0));
						lanes_av_wt := 4 - to_Integer(current_weight_index(1 Downto 0));

						lanes_i := 4; --lanes_i = min(4,remaining_i,lanes_av_in,lanes_av_wt)
						If (remaining_i < lanes_i) Then
							lanes_i := remaining_i;
						End If;
						If (lanes_av_in < lanes_i) Then
							lanes_i := lanes_av_in;
						End If;
						If (lanes_av_wt < lanes_i) Then
							lanes_i := lanes_av_wt;
						End If;

						dense_lane_count <= to_unsigned(lanes_i, dense_lane_count'length);

						state <= S_DENSE_FETCH_WAIT;

					When S_DENSE_FETCH_WAIT =>
						state <= S_DENSE_MAC;

					When S_DENSE_MAC =>
						--Bring selected byte to the right of the word (shift lanes)
						--This part vecomes useful when index + mac_counter is not word aligned. That is, lanes left are < 4. Because we multiply bytes (lanes) starting from the right, we 
						--need to put those bytes to the right as well
						input_shift_bits := input_byte_sel_lat * 8;
						weight_shift_bits := weight_byte_sel_lat * 8;

						input_word_shifted := Std_ulogic_vector(shift_right(unsigned(tensor_A_npu_rdata), input_shift_bits));
						weight_word_shifted := Std_ulogic_vector(shift_right(unsigned(tensor_B_npu_rdata), weight_shift_bits));

						accumulator <= mac4(
							accumulator,
							input_word_shifted,
							weight_word_shifted,
							resize(zero_point_lat, 8),
							to_integer(dense_lane_count)
							);

						--Advance by the number of lanes processed this cycle
						lanes_u := resize(dense_lane_count, lanes_u'length);
						next_count := mac_counter + lanes_u; --next_count = number of inputs processed till the previous iteration + pairs process in this iteration 
						mac_counter <= next_count; --update number of inputs processed

						--Check if all N inputs processed
						--Loop until all inputs processed
						If (next_count >= n_inputs_lat) Then
							state <= S_DENSE_REQUANT_PRODUCT;
						Else
							state <= S_DENSE_FETCH;
						End If;

					When S_DENSE_REQUANT_PRODUCT =>
						--Add bias and saturate to Q0.7 range
						prod <= requantize_product(accumulator, bias_val_reg, quantized_multiplier_lat);
						state <= S_DENSE_REQUANT_SHIFT;
					When S_DENSE_REQUANT_SHIFT =>
						requantize_32 <= requantize_shift(prod,quantized_multiplier_right_shift_lat);
						state <= S_DENSE_REQUANT_CLAMP;
					When S_DENSE_REQUANT_CLAMP =>
						r_8_reg <= requantize_clamp(requantize_32);
						state <= S_DENSE_WRITE;

					When S_DENSE_WRITE =>
						if (dense_output_index < (n_outputs_lat - 1)) then	--There are neurons left to process
							dense_output_index <= dense_output_index + 1;	--Increment neurons processed
							weight_base_reg    <= weight_base_reg + n_inputs_lat;	--Shift weights base index to point ot next neuron's weights
							bias_index_reg     <= bias_index_reg + 1;	--Shift bias base to next word
							out_i_reg          <= out_i_reg + 1;	--Shift index in result tensor to write for next neuron
							state              <= S_DENSE_INIT;	--Restart dense cycle
						else
							state <= S_DONE;
						end if;



						--Conv states----------------------------------------------------------
						--In Conv2d, there are multiple kernels within a filter. One kernel for each input channel
						--Output channels = number of filters used
						--Added prep stat because out_filter_stride was causing timing errors
					When S_CONV_PREP =>
						conv_out_row        <= (others => '0');
						conv_out_col        <= (others => '0');
						out_filter_stride <= resize(conv_n_out * conv_n_out, 16);
						conv_pixel_out_base <= (others => '0');
						state <= S_CONV_INIT;
					When S_CONV_INIT =>
						--Request bias word from tensor C
						--tensor_C_npu_rdata is available one cycle after tensor_C_npu_addr is set
						tensor_C_npu_addr <= resize(bias_index_reg, tensor_C_npu_addr'length) + conv_filter_index; --int32 bias (word) index
						
						filter_weight_base <= weight_base_reg + conv_filter_index * filter_stride; --Filter base = start of all filters for this layer (weight_base_reg) + (filter we are processing)*indices to move to reach to next filter in this layer
						--Save where we want to write the pixel to(out_i_reg is used as the flat index in Tensor R write)
						--Only reset out_i_reg here if we are starting a new filter for a pixel, otherwise out_i_reg holds the correct index as updated in the conv_write state
						if(conv_filter_index = 0) then
							out_i_reg          <= conv_pixel_out_base;	
						end if;
						state <= S_CONV_BIAS_WAIT;
					When S_CONV_BIAS_WAIT =>
						state <= S_CONV_BIAS_READ;
					When S_CONV_BIAS_READ =>
						--Bias word is available from BRAM, so we read it
						--byte_sel := bias_index_reg(1 Downto 0);
						packed_word := tensor_C_npu_rdata;
						--bias_val_reg <= extract_byte_from_word(packed_word, to_integer(byte_sel));
						bias_val_reg <= signed(packed_word);
						
						accumulator   <= (Others => '0'); --Reset accumulator for this pixel
						--Reset kernel position counters
						conv_kernel_row       <= (Others => '0');
						conv_kernel_col       <= (Others => '0');
						conv_input_channel       <= (Others => '0');
						--Row pointer starts at the top-left of the input channel feature (set by Ada)
						conv_row_ptr  <= in_i_reg;
						--Weight pointer starts at the first weight for this output channel
						--current_weight_index variable is not used because conv_weight_counter is updated across states
						conv_weight_counter <= filter_weight_base; --Weight are stored in a contiguous fashion in B
						state <= S_CONV_FETCH;

					When S_CONV_FETCH =>
						--Fetch a packed group of inputs/weights from A and B for multiplication and accumulation

						--Current element indices
						current_input_index := conv_row_ptr + resize(conv_kernel_col, 16); --Select next column in the row (row entry + col)
						current_weight_index := resize(conv_weight_counter, 16);
						--Request words from tensors A and B
						--tensor_A_npu_rdata and tensor_B_npu_rdata are available in the next cycle
						tensor_A_npu_addr <= resize(current_input_index(15 Downto 2), tensor_A_npu_addr'length);
						tensor_B_npu_addr <= resize(current_weight_index(15 Downto 2), tensor_B_npu_addr'length);
						
						input_byte_sel_lat     <= to_integer(current_input_index(1 Downto 0));
						weight_byte_sel_lat     <= to_integer(current_weight_index(1 Downto 0));


						--Compute how many lanes we can safely process this step
						--Don't want exit the 3x3 input feature/filter
						--Don't want to cross a 32-bit word boundary
						col_i := to_integer(conv_kernel_col);
						lanes_av_in := 4 - to_integer(current_input_index(1 downto 0));
						lanes_av_wt := 4 - to_integer(current_weight_index(1 downto 0));
						conv_cols_left_in_row   := 3 - col_i;

						lanes_i := 4; 	--lanes_i = max(1, min(4, lanes_av_in, lanes_av_wt, conv_cols_left_in_row))
						if (lanes_av_in < lanes_i) Then 
							lanes_i := lanes_av_in; 
						end if;
						if (lanes_av_wt < lanes_i) Then 
							lanes_i := lanes_av_wt; 
						end if;
						if (conv_cols_left_in_row   < lanes_i) Then 
							lanes_i := conv_cols_left_in_row;   
						end if;
						if (lanes_i < 1) Then 
							lanes_i := 1; 
						end if;
					
						conv_lane_count <= to_unsigned(lanes_i, conv_lane_count'length);

						state <= S_CONV_FETCH_WAIT;

					When S_CONV_FETCH_WAIT =>
						state <= S_CONV_MAC;
						
					When S_CONV_MAC =>	--We process all the kernels for one input channel first. After we are done with processing one filter (set of kernels), we add the bias. We then proceed to repeat this process for all filters (output channels) for each pixel. 
										--We then repeat this process for all pixels
						--Bring selected byte to the right of the word (shift lanes)
						--This part vecomes useful when index + mac_counter is not word aligned. That is, lanes left are < 4. Because we multiply bytes (lanes) starting from the right, we 
						--need to put those bytes to the right as well
					    input_shift_bits  := input_byte_sel_lat  * 8;
						weight_shift_bits := weight_byte_sel_lat * 8;

						input_word_shifted  := std_ulogic_vector(shift_right(unsigned(tensor_A_npu_rdata), input_shift_bits));
						weight_word_shifted := std_ulogic_vector(shift_right(unsigned(tensor_B_npu_rdata), weight_shift_bits));
						
						--Reusing desne MAC
						accumulator <= mac4(
							accumulator,
							input_word_shifted,
							weight_word_shifted,
							resize(zero_point_lat, 8),
							to_integer(conv_lane_count)
						);

						--Advance by the number of lanes processed this cycle
						lanes_u := resize(conv_lane_count, lanes_u'length);
						conv_weight_counter <= conv_weight_counter + lanes_u;

						col_i   := to_integer(conv_kernel_col);
						row_i   := to_integer(conv_kernel_row);
						lanes_i := to_integer(conv_lane_count);

						if (col_i + lanes_i) < 3 then 						--There are still columns in a rown of 3x3 filter left to be processed
							conv_kernel_col <= to_unsigned(col_i + lanes_i, conv_kernel_col'length);	--Update column
							state <= S_CONV_FETCH;
						
						elsif (row_i < 2) then --there are still rows in the 3x3 filter left to be processed
							conv_kernel_row <= to_unsigned(row_i + 1, conv_kernel_row'length);	--Update row
							conv_kernel_col <= "00";	--Reset column to 0
							conv_row_ptr    <= conv_row_ptr + resize(din_reg, 16);	--Move to the next row
							state <= S_CONV_FETCH;
						
						elsif (conv_input_channel < resize(n_inputs_lat - 1, conv_input_channel'length)) then --there are more input channels left
							conv_kernel_row     <= "00";
							conv_kernel_col     <= "00";
							conv_input_channel  <= conv_input_channel + 1;
							conv_row_ptr <= resize (in_i_reg
										  + resize(conv_input_channel + 1, 16)
											* resize(total_int8_elements_in_tensor(15 downto 0), 16), 16);
							state <= S_CONV_FETCH;
						--Otherwise requantize the results (including adding the bias)
						else
							state <= S_CONV_REQUANT_PRODUCT;
							
							End If;

					When S_CONV_REQUANT_PRODUCT =>
						--Add bias and saturate to Q0.7 range
						prod <= requantize_product(accumulator, bias_val_reg, quantized_multiplier_lat);
						state <= S_CONV_REQUANT_SHIFT;
					When S_CONV_REQUANT_SHIFT =>
						requantize_32 <= requantize_shift(prod,quantized_multiplier_right_shift_lat);
						state <= S_CONV_REQUANT_CLAMP;
					When S_CONV_REQUANT_CLAMP =>
						r_8_reg <= requantize_clamp(requantize_32);
						state <= S_CONV_WRITE;

					When S_CONV_WRITE =>
						if (conv_filter_index < (n_outputs_lat - 1)) Then	--More filters remain for this pixel
							conv_filter_index <= conv_filter_index + 1;
							out_i_reg <= out_i_reg + out_filter_stride;	--Move index to where the pixel for this input and next filter wants to be written to
							state <= S_CONV_INIT;
						Else
						    --All filters done for this pixel
								conv_filter_index <= (others => '0');

							if (conv_out_col < conv_n_out - 1) then  --If column in result tensor is less tha result tensor dimension (dimension = col count since it is a square), contiue processing for next column
								conv_out_col        <= conv_out_col + 1; 	--Move right within the row
								in_i_reg          <= in_i_reg + 1;           --in_i_reg is the flat (int8 index) into A. Increment by 1 = next column
								conv_pixel_out_base <= conv_pixel_out_base + 1;	--Write to next pixel in output tensor
								state               <= S_CONV_INIT;

							elsif (conv_out_row < conv_n_out - 1) then	--Done with all column pixels in this row of result tensor, process next row
								conv_out_row        <= conv_out_row + 1; 	--Wrap to next row
								conv_out_col        <= (others => '0');	--Start at column 0
								in_i_reg          <= in_i_reg + 3;    --in_i_reg previously pointed at the last valid input column in the in the previous row (side length - 2). Since rows are stored continuosly in memory
																			--in_i_reg will skip the last two columns in the input tensor's previous row, and point to the first column of the current (next) row
								conv_pixel_out_base <= conv_pixel_out_base + 1;	--Write to next pixel in output tensor
								state               <= S_CONV_INIT;
							Else
								state <= S_DONE;
							End if;
						End if;
					When S_DONE =>
						--status_reg(0) <= '0';
						status_reg(1) <= '1';
						state <= S_IDLE;

				End Case;
			End If;
		End If;
	End Process;
	--When npu_busy='0': WB can read/write tensor windows.
	--When npu_busy='1': NPU owns the tensor memories.
	--Tensor A: WB R/W (when idle) + NPU read (+ NPU in-place write (Softmax EXP))
	Process (clk)
		Variable tensor_offset : Natural;
	Begin
		If (rising_edge(clk)) Then
			If (reset = '1') Then
				tensor_A_wb_rdata <= (Others => '0');
				tensor_A_npu_rdata <= (Others => '0');
			Else
				If (npu_busy = '0') Then
					--WB 
					If (wb_req = '1' And
						unsigned(i_wb_addr) >= unsigned(TENSOR_A_BASE) And unsigned(i_wb_addr) < unsigned(TENSOR_A_BASE) + to_unsigned(TENSOR_A_BYTES, 32)) Then
						tensor_offset := get_tensor_offset(i_wb_addr, TENSOR_A_BASE);
						If (tensor_offset < TENSOR_A_WORDS) Then
							If (i_wb_we = '1') Then
								tensor_A_mem(tensor_offset) <= i_wb_data;
							End If;
							tensor_A_wb_rdata <= tensor_A_mem(tensor_offset);
						Else
							tensor_A_wb_rdata <= (Others => '0');
						End If;
					End If;

				Else
					--NPU port (read)
					If (to_integer(tensor_A_npu_addr) < TENSOR_A_WORDS) Then
						tensor_A_npu_rdata <= tensor_A_mem(to_integer(tensor_A_npu_addr));
					Else
						tensor_A_npu_rdata <= (Others => '0');
					End If;

					--NPU in-place write for Softmax EXP (Pass 1)
					If (((state = S_ACT_WRITE) And (op_code_reg = OP_SOFTMAX) And (softmax_mode_latched = '0')) Or state = S_COPY_A_WRITE)  Then
						If (to_integer(in_i_reg) < TENSOR_A_WORDS) Then
							tensor_A_mem(to_integer(in_i_reg)) <= r_w_reg;
						End If;
					End If;
				End If;
			End If;
		End If;
	End Process;

	--Tensor B: WB R/W (when idle) + NPU read
	Process (clk)
		Variable tensor_offset : Natural;
	Begin
		If (rising_edge(clk)) Then
			If (reset = '1') Then
				tensor_B_wb_rdata <= (Others => '0');
				tensor_B_npu_rdata <= (Others => '0');
			Else
				If (npu_busy = '0') Then
					If (wb_req = '1' And
						unsigned(i_wb_addr) >= unsigned(TENSOR_B_BASE) And unsigned(i_wb_addr) < unsigned(TENSOR_B_BASE) + to_unsigned(TENSOR_B_BYTES, 32)) Then
						tensor_offset := get_tensor_offset(i_wb_addr, TENSOR_B_BASE);
						If (tensor_offset < TENSOR_B_WORDS) Then
							If (i_wb_we = '1') Then
								tensor_B_mem(tensor_offset) <= i_wb_data;
							End If;
							tensor_B_wb_rdata <= tensor_B_mem(tensor_offset);
						Else
							tensor_B_wb_rdata <= (Others => '0');
						End If;
					End If;
				Else
					If (to_integer(tensor_B_npu_addr) < TENSOR_B_WORDS) Then
						tensor_B_npu_rdata <= tensor_B_mem(to_integer(tensor_B_npu_addr));
					Else
						tensor_B_npu_rdata <= (Others => '0');
					End If;
				End If;
			End If;
		End If;
	End Process;

	--Tensor C: WB R/W (when idle) + NPU read
	Process (clk)
		Variable tensor_offset : Natural;
	Begin
		If (rising_edge(clk)) Then
			If (reset = '1') Then
				tensor_C_wb_rdata <= (Others => '0');
				tensor_C_npu_rdata <= (Others => '0');
			Else
				If (npu_busy = '0') Then
					If (wb_req = '1' And
						unsigned(i_wb_addr) >= unsigned(TENSOR_C_BASE) And unsigned(i_wb_addr) < unsigned(TENSOR_C_BASE) + to_unsigned(TENSOR_C_BYTES, 32)) Then
						tensor_offset := get_tensor_offset(i_wb_addr, TENSOR_C_BASE);
						If (tensor_offset < TENSOR_C_WORDS) Then
							If (i_wb_we = '1') Then
								tensor_C_mem(tensor_offset) <= i_wb_data;
							End If;
							tensor_C_wb_rdata <= tensor_C_mem(tensor_offset);
						Else
							tensor_C_wb_rdata <= (Others => '0');
						End If;
					End If;
				Else
					If (to_integer(tensor_C_npu_addr) < TENSOR_C_WORDS) Then
						tensor_C_npu_rdata <= tensor_C_mem(to_integer(tensor_C_npu_addr));
					Else
						tensor_C_npu_rdata <= (Others => '0');
					End If;
				End If;
			End If;
		End If;
	End Process;

	--Tensor R: WB read (when idle) + NPU write
	--NPU writes happen in these states:
	--S_ACT_WRITE     : write one packed word at word index in_i_reg (except Softmax EXP pass)
	--S_P_WRITE       : write one int8 at element index out_i_reg
	--S_DENSE_WRITE   : write one int8 at element index out_i_reg
	--S_CONV_WRITE    : write one int8 at element index out_i_reg
	Process (clk)
		Variable tensor_offset : Natural;
		Variable w_index : Natural;
		Variable byte_sel : Natural Range 0 To 3;
		Variable word_tmp : Std_ulogic_vector(31 Downto 0);
	Begin
		If (rising_edge(clk)) Then
			If (reset = '1') Then
				tensor_R_wb_rdata <= (Others => '0');
			Else
				If (npu_busy = '0') Then
					--WB read port
					If (wb_req = '1' And
						unsigned(i_wb_addr) >= unsigned(TENSOR_R_BASE) And unsigned(i_wb_addr) < unsigned(TENSOR_R_BASE) + to_unsigned(TENSOR_R_BYTES, 32)) Then
						tensor_offset := get_tensor_offset(i_wb_addr, TENSOR_R_BASE);
						If (tensor_offset < TENSOR_R_WORDS) Then
							tensor_R_wb_rdata <= tensor_R_mem(tensor_offset);
						Else
							tensor_R_wb_rdata <= (Others => '0');
						End If;
					End If;

				Else
					--NPU write
					If (state = S_P_WRITE or state = S_DENSE_WRITE or state = S_CONV_WRITE) Then
						--Write a single signed int8 into the packed word at element index out_i_reg
						w_index := to_integer(out_i_reg(15 Downto 2));
						byte_sel := to_integer(out_i_reg(1 Downto 0));
						If (w_index < TENSOR_R_WORDS) Then
							word_tmp := tensor_R_mem(w_index);
							Case byte_sel Is
								When 0 => word_tmp(7 Downto 0) := Std_ulogic_vector(r_8_reg);
								When 1 => word_tmp(15 Downto 8) := Std_ulogic_vector(r_8_reg);
								When 2 => word_tmp(23 Downto 16) := Std_ulogic_vector(r_8_reg);
								When Others => word_tmp(31 Downto 24) := Std_ulogic_vector(r_8_reg);
							End Case;
							tensor_R_mem(w_index) <= word_tmp;
						End If;

					Elsif (state = S_ACT_WRITE) Then
						--Softmax EXP is in-place on A, so only write to R for all other activation cases
						If (Not (op_code_reg = OP_SOFTMAX And softmax_mode_latched = '0')) Then
							If (to_integer(in_i_reg) < TENSOR_R_WORDS) Then
								tensor_R_mem(to_integer(in_i_reg)) <= r_w_reg;
							End If;
						End If;
					Else
						If (to_integer(tensor_R_npu_addr) < TENSOR_R_WORDS) Then
							tensor_R_npu_rdata <= tensor_R_mem(to_integer(tensor_R_npu_addr));
						Else
							tensor_R_npu_rdata <= (Others => '0');
						End if;
					End If;
				End If;
			End If;
		End If;
	End Process;

End Architecture;
