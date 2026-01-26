Library ieee;
Use ieee.std_logic_1164.All;
Use ieee.numeric_std.All;

Package ov5640_image_buffer is

	--Tensor memory limits and packing
	Constant MAX_DIM : Natural := 100; 
	Constant TENSOR_WORDS : Natural := 2500; 
	
	Constant TENSOR_BYTES : Natural := TENSOR_WORDS * 4;
	Type tensor_mem_type Is Array (0 To TENSOR_WORDS - 1) Of Std_ulogic_vector(31 Downto 0);

End package ov5640_image_buffer;
