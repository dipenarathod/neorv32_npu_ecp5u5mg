Library ieee;
Use ieee.std_logic_1164.All;
Use ieee.numeric_std.All;

Package tensor_operations_dense Is

	Constant OP_DENSE : Std_ulogic_vector(4 Downto 0) := "00111"; --Dense layer opcode (5-bit)
	Constant OP_CONV : Std_ulogic_vector(4 Downto 0) := "01000"; --Conv layer opcode (5-bit)


	--4-lane MAC
	--Processes up to 4 consecutive input, weight pairs
	--valid_lanes inform how many lanes are to be processed
	--Modified to 32-bit width and for requantization
	Function mac4(
		accumulator       : signed(31 Downto 0);
		input_word        : Std_ulogic_vector(31 Downto 0);
		weight_word       : Std_ulogic_vector(31 Downto 0);
		weight_zero_point : signed(7 Downto 0);
		valid_lanes       : Natural Range 1 To 4
	) Return signed;

	--First part of requantization: get 64-bit product
	--Based on GEMMlowp's guide
	Function requantize_product(
		accumulator  						: signed(31 Downto 0); 
		bias								: signed(31 Downto 0);
		quantized_multiplier 				: signed(31 Downto 0)
	) return signed;
	
	--Second part of requantization: round and shift 64-bit product
	Function requantize_shift(
		product  								: signed(63 Downto 0);
		quantized_multiplier_right_shift	: unsigned(7 Downto 0)
	) return signed ;
	
	--Addign a Third part of requantization: clamp. Removing clamping from the the old function requantize_shift_clamp
	Function requantize_clamp(
		result_32  								: signed(31 Downto 0)
	) return signed;

	--Extract byte from packed word (0 to 3 index)
	Function extract_byte_from_word(
		word       : Std_ulogic_vector(31 Downto 0);
		byte_index : Natural Range 0 To 3
	) Return signed;

End Package tensor_operations_dense;

Package Body tensor_operations_dense Is

	--4-lane MAC
	--Processes up to 4 consecutive input, weight pairs
	--valid_lanes inform how many lanes are to be processed
	--Modified to 32-bit width and for requantization
	Function mac4(
		accumulator       : signed(31 Downto 0);
		input_word        : Std_ulogic_vector(31 Downto 0);
		weight_word       : Std_ulogic_vector(31 Downto 0);
		weight_zero_point : signed(7 Downto 0);
		valid_lanes       : Natural Range 1 To 4
	) Return signed Is
		Variable a0, a1, a2, a3 					: signed(7 Downto 0);
		Variable w0, w1, w2, w3 					: signed(7 Downto 0);
		Variable w_zp_9								: signed(8 downto 0);
		Variable w0_adj, w1_adj, w2_adj, w3_adj 	: signed(8 Downto 0);
		Variable p0, p1, p2, p3 					: signed(16 Downto 0);
		Variable sum_products 						: signed(31 Downto 0);
		Variable result 							: signed(31 Downto 0);
	Begin
		--Extract lane bytes
		a0 := extract_byte_from_word(input_word, 0);
		a1 := extract_byte_from_word(input_word, 1);
		a2 := extract_byte_from_word(input_word, 2);
		a3 := extract_byte_from_word(input_word, 3);
		--Equation 5 from GEMMLowp: result_quantized_value = result_zero_point +
		--(lhs_scale * rhs_scale / result_scale) *
        --Sum_over_i(
        --    (lhs_quantized_value[i] - lhs_zero_point) *
        --    (rhs_quantized_value[i] - rhs_zero_point)
        --)                                                  (5)
		--LHS = input matrix is assumed to already be in Q0.7 scale with a zero-point of zero
		--We will need to adjust the RHS (weights) by subtracting the zero-point
		--TODO: Test moving this adjustment step to the Python weight conversion step
		w_zp_9 := resize(weight_zero_point,9);
		w0 := extract_byte_from_word(weight_word, 0);
		w1 := extract_byte_from_word(weight_word, 1);
		w2 := extract_byte_from_word(weight_word, 2);
		w3 := extract_byte_from_word(weight_word, 3);

		w0_adj := resize(w0,9) - w_zp_9;
		w1_adj := resize(w1,9) - w_zp_9;
		w2_adj := resize(w2,9) - w_zp_9;
		w3_adj := resize(w3,9) - w_zp_9;
		
		--Multiply only the required lanes
		p0 := (Others => '0');
		p1 := (Others => '0');
		p2 := (Others => '0');
		p3 := (Others => '0');
		If (valid_lanes >= 1) Then
			p0 := a0 * w0_adj;
		End If;

		If (valid_lanes >= 2) Then
			p1 := a1 * w1_adj;
		End If;

		If (valid_lanes >= 3) Then
			p2 := a2 * w2_adj;
		End If;

		If (valid_lanes >= 4) Then
			p3 := a3 * w3_adj;
		End If;

		--Sum products in 32-bit precision
		sum_products := resize(p0, 32) + resize(p1, 32) + resize(p2, 32) + resize(p3, 32);

		--Accumulate
		result := accumulator + sum_products;
		Return result;
	End Function;

	--First part of requantization: get 64-bit product
	--Based on GEMMlowp's guide
	Function requantize_product(
		accumulator  						: signed(31 Downto 0); 
		bias								: signed(31 Downto 0);
		quantized_multiplier 				: signed(31 Downto 0)
	) return signed is
		Variable accumulator_with_bias : signed(31 Downto 0);
		Variable product 		: signed(63 Downto 0);
		Variable total_shift 	: Integer;
		Variable rounding 		: signed(63 Downto 0);
		Variable shifted 	: signed(63 Downto 0);
	Begin
		--Equation 7: result_quantized_value = result_zero_point +
		--			  (lhs_scale * rhs_scale / result_scale) * int32_accumulator 
		--(lhs_scale * rhs_scale / result_scale) is obtained using the quantized multiplier and the right shift
		--Result scale = Q0.7 scale and zero point = 0. lhs and result scale cancel out
		accumulator_with_bias := accumulator + bias;
		product := accumulator_with_bias * quantized_multiplier;
		Return product;
	end function;
		
	--Second part of requantization: round and shift 64-bit product
	Function requantize_shift(
		product  								: signed(63 Downto 0);
		quantized_multiplier_right_shift	: unsigned(7 Downto 0)

	) return signed is
		Variable total_shift 	: Integer;
		Variable rounding 		: signed(63 Downto 0);
		Variable shifted 	: signed(63 Downto 0);
		Variable shifted_32 	: signed(31 Downto 0);
		--Variable result_32  	: signed(31 Downto 0);
		--Variable result_8   	: signed(7 Downto 0);
	Begin
		total_shift := 31 + to_integer(quantized_multiplier_right_shift);	--Quantized multiplier is in Q0.31 (it was stored in int32)
		--The quantized multiplier is a number in range [-1,1) stored in a int32 number. It was left shifted by 31 bits. We need to shift it right by 31
		--to get the original multiplier back
		--https://www.embeddedrelated.com/showarticle/1015.php#:~:text=Runtime%20calculations%2C%20on%20the%20other,point%20equivalent%20of%200.5)%20first:
		--The right shift truncates a lot. It also floors the value (floor and ceil)
		--13/8=1.625. 13>>3 = 1. If we add a rounding(=half the divisor), we can get a better answer
		--(13+4)>>3 = 2
		
		rounding := shift_left(to_signed(1, 64), total_shift - 1);  --Number to be divided = total_shift/2
		if (product < 0) then
		  --apply round to abs value and then reapply sign
		  shifted := shift_right((product) + (rounding-1), total_shift);
		else
		  shifted := shift_right(product + rounding, total_shift);
		end if;
		shifted_32 := resize(shifted, 32);
		-- result_32 := shifted_32;
		-- --Saturate to int8 range [-128, 127]
		-- If (result_32 > to_signed(127, 32)) Then
		-- 	result_8 := to_signed(127, 8);
		-- Elsif (result_32 < to_signed(-128, 32)) Then
		-- 	result_8 := to_signed(-128, 8);
		-- Else
		-- 	result_8 := resize(result_32, 8);
		-- End If;
		-- Return result_8;
		return shifted_32;
	end function;

	--Addign a Third part of requantization: clamp. Removing clamping from the the old function requantize_shift_clamp
	Function requantize_clamp(
		result_32  								: signed(31 Downto 0)
	) return signed is
		Variable result_8   	: signed(7 Downto 0);
	Begin
		--Saturate to int8 range [-128, 127]
		If (result_32 > to_signed(127, 32)) Then
			result_8 := to_signed(127, 8);
		Elsif (result_32 < to_signed(-128, 32)) Then
			result_8 := to_signed(-128, 8);
		Else
			result_8 := resize(result_32, 8);
		End If;
		Return result_8;
	End requantize_clamp;


	--Extract signed byte from packed 32-bit word
	Function extract_byte_from_word(
		word       : Std_ulogic_vector(31 Downto 0);
		byte_index : Natural Range 0 To 3
	) Return signed Is
		Variable byte_val : Std_ulogic_vector(7 Downto 0);
	Begin
		Case byte_index Is
			When 0 => byte_val := word(7 Downto 0);
			When 1 => byte_val := word(15 Downto 8);
			When 2 => byte_val := word(23 Downto 16);
			When 3 => byte_val := word(31 Downto 24);
		End Case;
		Return signed(byte_val);
	End Function;

End Package Body tensor_operations_dense;
