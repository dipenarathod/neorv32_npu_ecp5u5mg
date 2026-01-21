with System.Address_To_Access_Conversions;
with System.Storage_Elements;
with Interfaces;
with Ada.Text_IO; use Ada.Text_IO;

package body Ada_Ml is

   use Interfaces;

   --Volatile word
   type Volatile_Word is new Unsigned_32;
   --Reusing word from .ads seems to not work. New declaration for the same works?
   --Works in favor because now we have volatile and non-volatile words
   --Volatile because may change any time
   pragma Volatile_Full_Access (Volatile_Word);

   --Can't use the "use" clause. is new works for package rename as well
   package Convert is new System.Address_To_Access_Conversions (Volatile_Word);

   --Convert address to volatile word pointer
   --access = pointers
   function R32 (Addr : System.Address) return access Volatile_Word is
   begin
      return Convert.To_Pointer (Addr);
   end R32;

   --Add byte offset to an address
   --Refer https://learn.adacore.com/courses/intro-to-embedded-sys-prog/chapters/interacting_with_devices.html
   --System.address is a private type, making address math possible only via System.Storage_Elements
   --Pg 385 for address arithmetic: http://www.ada-auth.org/standards/22rm/RM-Final.pdf
   function Add_Byte_Offset
     (Address : System.Address; Offset : Unsigned_32) return System.Address is
   begin

      return
        System.Storage_Elements."+"
          (Address, System.Storage_Elements.Storage_Offset (Offset));
   end Add_Byte_Offset;

   --Write value to a register
   procedure Write_Reg (Addr : System.Address; Value : Word) is
   begin
      R32 (Addr).all := Volatile_Word (Value);
   end Write_Reg;

   --Read val from register (need to dereference the word)
   function Read_Reg (Addr : System.Address) return Word is
   begin
      return Word (R32 (Addr).all);
   end Read_Reg;

   --  --Pack four 8 bits into a 32-bit word
   --  function Pack_Four_Bytes (B0, B1, B2, B3 : Unsigned_Byte) return Word is
   --     U0 : constant Word := Word (Unsigned_32 (B0));
   --     U1 : constant Word := Word (Unsigned_32 (B1));
   --     U2 : constant Word := Word (Unsigned_32 (B2));
   --     U3 : constant Word := Word (Unsigned_32 (B3));
   --  begin
   --     return
   --       Word
   --         (U0
   --          or Shift_Left (U1, 8)
   --          or Shift_Left (U2, 16)
   --          or Shift_Left (U3, 24));
   --  end Pack_Four_Bytes;

   --Get an int8 at an index (offset) inside a 32-bit word
   --There are 4 bytes, W[7:0], W[15:8], W[23:16], W[31:24]
   --Depending on the index, we shift the word right index * 8 to force the desired byte to exist in positions 7:0
   function Unpack_Byte_At_Index
     (W : Word; Index : Natural) return Unsigned_Byte
   is
      Shift  : constant Natural := Index * 8;
      Byte32 : constant Unsigned_32 :=
        Unsigned_32 (Shift_Right (W, Shift) and 16#FF#); --0xFF = 1111 1111
      --Leading 24 bits are 0

   begin
      return Unsigned_Byte (Byte32);
   end Unpack_Byte_At_Index;

   --  --Reuse unpack byte at index
   --  procedure Unpack_Four_Bytes (W : Word; B0, B1, B2, B3 : out Unsigned_Byte)
   --  is
   --  begin
   --     B0 := Unpack_Byte_At_Index (W, 0);
   --     B1 := Unpack_Byte_At_Index (W, 1);
   --     B2 := Unpack_Byte_At_Index (W, 2);
   --     B3 := Unpack_Byte_At_Index (W, 3);
   --  end Unpack_Four_Bytes;

   --Get byte from tensor using word and byte index
   function Get_Byte_From_Tensor
     (Data : Word_Array; Index : Natural) return Unsigned_Byte
   is
      Word_Index : constant Natural := Index / 4;
      Byte_Index : constant Natural := Index mod 4;
   begin
      return Unpack_Byte_At_Index (Data (Word_Index), Byte_Index);
   end Get_Byte_From_Tensor;

   --Word count for a square N×N int8 tensor when 4 int8 are packed per 32-bit word
   function Tensor_Words
     (N : Natural; One_Dimensional : Boolean := False) return Natural
   is
      --Elements : constant Natural := N;
   begin

      if (One_Dimensional /= True) then
         return (N * N + 3) / 4;
      else
         return (N + 3) / 4;
      end if;

      --Why + 3 is necessary:
      --N*N = 9 *9  = 81 elements
      --81/4 = 20 words, but 20 words are insufficient to hold 81 bytes
      --84/4 = 21
      --+3 makes it possible that even partially filled words are counted
   end Tensor_Words;

   --DIM register only reads the right-most 8 bits. The other bits are ignored. Write word
   procedure Set_Dim (N : Natural) is
   begin
      Write_Reg (DIM_Addr, Word (Unsigned_32 (N)));
   end Set_Dim;

   --Set base index in A to perform pooling on
   procedure Set_Pool_Base_Index (Index : Natural) is
   begin
      Write_Reg (BASEI_Addr, Word (Unsigned_32 (Index)));
   end Set_Pool_Base_Index;

   --Set index in R to write result to for pooling and dense
   procedure Set_Out_Index (Index : Natural) is
   begin
      Write_Reg (OUTI_Addr, Word (Unsigned_32 (Index)));
   end Set_Out_Index;

   --Index in tensor to perform operation on, such as activation
   procedure Set_Word_Index (Index : Natural) is
   begin
      Write_Reg (WORDI_Addr, Word (Unsigned_32 (Index)));
   end Set_Word_Index;

   --Set softmax mode: 0=EXP phase, 1=DIV phase
   procedure Set_Softmax_Mode (Mode : Word) is
   begin
      Write_Reg (SOFTMAX_MODE_Addr, Mode);
   end Set_Softmax_Mode;

   --Set sum parameter for softmax DIV phase (Ada calculates sum)
   procedure Set_Sum_Param (Sum : Word) is
   begin
      Write_Reg (SUM_Addr, Sum);
   end Set_Sum_Param;

   --Set the base index in B (for from when the weights of this layer begin)
   procedure Set_Weight_Base_Index (Index : Natural) is
   begin
      Write_Reg (WEIGHT_BASE_INDEX_Addr, Word (Unsigned_32 (Index)));
   end Set_Weight_Base_Index;

   --Set the bias index in C (for from when the weights of this layer begin)
   procedure Set_Bias_Base_Index (Index : Natural) is
   begin
      Write_Reg (BIAS_BASE_INDEX_Addr, Word (Unsigned_32 (Index)));
   end Set_Bias_Base_Index;

   --Set number of inputs for the dense layer
   procedure Set_N_Inputs (N : Natural) is
   begin
      Write_Reg (N_INPUTS_Addr, Word (Unsigned_32 (N)));
   end Set_N_Inputs;

   --Set scale for requantization
   procedure Set_Scale_Register (Scale : Natural) is
   begin
      Write_Reg (SCALE_REG_Addr, Word (Unsigned_32 (Scale)));
   end;

   --Set zero point for requantization
   procedure Set_Zero_Point (Zero_Point : Integer) is
   begin
      Write_Reg (ZERO_POINT_REG_Addr, Word (Unsigned_32 (Zero_Point)));
   end;

   --Set quantized multiplier for requantization
   procedure Set_Quantized_Multiplier_Register (Multiplier : Integer) is
   begin
      Write_Reg
        (QUANTIZED_MULTIPLIER_REG_Addr, Word (Unsigned_32 (Multiplier)));
   end;

   --Set right shift for quantized multiplier for requantization
   procedure Set_Quantized_Multiplier_Right_Shift_Register
     (Right_Shift : Natural) is
   begin
      Write_Reg
        (QUANTIZED_MULTIPLIER_RIGHT_SHIFT_REG_Addr,
         Word (Unsigned_32 (Right_Shift)));
   end;

   --Perform operation
   procedure Perform_Op (Opcode : Word) is
      Final_Opcode : Word := Opcode;
      Val          : Word;
   begin
      --If input opcode > max allowed opcode, change opcode to nop
      --Unused opcodes are handled by VHDL
      if (Final_Opcode > MAX_ALLOWED_OPCODE) then
         Final_Opcode := OP_NOP;
      end if;

      Val := Shift_Left (Final_Opcode, Opcode_Shift) or Perform_Bit;

      Write_Reg (CTRL_Addr, Val);
   end Perform_Op;

   procedure Perform_Max_Pool is
   begin
      Perform_Op (OP_MAX);
   end Perform_Max_Pool;

   procedure Perform_Avg_Pool is
   begin
      Perform_Op (OP_AVG);
   end Perform_Avg_Pool;

   procedure Perform_Sigmoid is
   begin
      Perform_Op (OP_SIG);
   end Perform_Sigmoid;

   procedure Perform_ReLU is
   begin
      Perform_Op (OP_RELU);
   end Perform_ReLU;

   --Softmax operation (mode flag controls EXP vs DIV phase)
   procedure Perform_Softmax is
   begin
      Perform_Op (OP_SOFTMAX);
   end Perform_Softmax;

   --Softmax operation (mode flag controls EXP vs DIV phase)
   procedure Perform_Dense is
   begin
      Perform_Op (OP_DENSE);
   end Perform_Dense;

   --"/=" is the inequality operator in Ada, not !=
   --Read status_reg[0]
   function Is_Busy return Boolean is
   begin
      return (Read_Reg (STATUS_Addr) and Busy_Mask) /= 0;
   end Is_Busy;

   --Read status_reg[1]
   function Is_Done return Boolean is
   begin
      return (Read_Reg (STATUS_Addr) and Done_Mask) /= 0;
   end Is_Done;

   --Busy waiting
   procedure Wait_While_Busy is
   begin
      while Is_Busy loop
         null;
      end loop;
   end Wait_While_Busy;

   --Each word is 4 bytes apart
   --Base address + index * 4 = actual index of word
   --Applicable for both, A and R
   --Read/Write logic is the same. You read in one, and write in the other
   procedure Write_Word_In_A (Index : Natural; Value : Word) is
      Addr : constant System.Address :=
        Add_Byte_Offset (ABASE_Addr, Unsigned_32 (Index) * 4);
   begin
      Write_Reg (Addr, Value);
   end Write_Word_In_A;

   procedure Write_Words_In_A (Src : in Word_Array) is
      J : Natural := 0;
   begin
      for I in Src'Range loop
         Write_Word_In_A (J, Src (I));
         J := J + 1;
      end loop;
   end Write_Words_In_A;

   function Read_Word_From_A (Index : Natural) return Word is
      Addr : constant System.Address :=
        Add_Byte_Offset (ABASE_Addr, Unsigned_32 (Index) * 4);
   begin
      return Read_Reg (Addr);
   end Read_Word_From_A;

   procedure Read_Words_From_A (Dest : out Word_Array) is
      J : Natural := 0;
   begin
      for I in Dest'Range loop
         Dest (I) := Read_Word_From_A (J);
         J := J + 1;
      end loop;
   end Read_Words_From_A;

   procedure Write_Word_In_B (Index : Natural; Value : Word) is
      Addr : constant System.Address :=
        Add_Byte_Offset (BBASE_Addr, Unsigned_32 (Index) * 4);
   begin
      Write_Reg (Addr, Value);
   end Write_Word_In_B;

   procedure Write_Words_In_B (Src : in Word_Array) is
      J : Natural := 0;
   begin
      for I in Src'Range loop
         Write_Word_In_B (J, Src (I));
         J := J + 1;
      end loop;
   end Write_Words_In_B;

   function Read_Word_From_B (Index : Natural) return Word is
      Addr : constant System.Address :=
        Add_Byte_Offset (BBASE_Addr, Unsigned_32 (Index) * 4);
   begin
      return Read_Reg (Addr);
   end Read_Word_From_B;

   procedure Read_Words_From_B (Dest : out Word_Array) is
      J : Natural := 0;
   begin
      for I in Dest'Range loop
         Dest (I) := Read_Word_From_B (J);
         J := J + 1;
      end loop;
   end Read_Words_From_B;

   procedure Write_Word_In_C (Index : Natural; Value : Word) is
      Addr : constant System.Address :=
        Add_Byte_Offset (CBASE_Addr, Unsigned_32 (Index) * 4);
   begin
      Write_Reg (Addr, Value);
   end Write_Word_In_C;

   procedure Write_Words_In_C (Src : in Word_Array) is
      J : Natural := 0;
   begin
      for I in Src'Range loop
         Write_Word_In_C (J, Src (I));
         J := J + 1;
      end loop;
   end Write_Words_In_C;

   function Read_Word_From_C (Index : Natural) return Word is
      Addr : constant System.Address :=
        Add_Byte_Offset (CBASE_Addr, Unsigned_32 (Index) * 4);
   begin
      return Read_Reg (Addr);
   end Read_Word_From_C;

   procedure Read_Words_From_C (Dest : out Word_Array) is
      J : Natural := 0;
   begin
      for I in Dest'Range loop
         Dest (I) := Read_Word_From_C (J);
         J := J + 1;
      end loop;
   end Read_Words_From_C;

   function Read_Word_From_R (Index : Natural) return Word is
      Addr : constant System.Address :=
        Add_Byte_Offset (RBASE_Addr, Unsigned_32 (Index) * 4);
   begin
      return Read_Reg (Addr);
   end Read_Word_From_R;

   procedure Read_Words_From_R (Dest : out Word_Array) is
      J : Natural := 0;
   begin
      for I in Dest'Range loop
         Dest (I) := Read_Word_From_R (J);
         J := J + 1;
      end loop;
   end Read_Words_From_R;


   --Procedures to Apply ReLU and Sigmoid
   --Translated test C code
   --Sigmoid and ReLU are very similar (because they are activation functions)
   procedure Apply_ReLU_All_Words
     (N : Natural; One_Dimensional : Boolean := False)
   is
      Words : constant Natural := Tensor_Words (N, One_Dimensional);
   begin
      for I in 0 .. Words - 1 loop
         Set_Word_Index (I);
         Perform_ReLU;
         Wait_While_Busy;
         Write_Reg (CTRL_Addr, 0); --De-assert start
      end loop;
   end Apply_ReLU_All_Words;

   procedure Apply_Sigmoid_All_Words
     (N : Natural; One_Dimensional : Boolean := False)
   is
      Words : constant Natural := Tensor_Words (N, One_Dimensional);
   begin
      for I in 0 .. Words - 1 loop
         Set_Word_Index (I);
         Perform_Sigmoid;
         Wait_While_Busy;
         Write_Reg (CTRL_Addr, 0); --De-assert start
      end loop;
   end Apply_Sigmoid_All_Words;


   --2x2 max pooling over entire tensor
   --Produces (N/2) x (N/2) outputs in R
   procedure Apply_MaxPool_2x2_All_Words (N : Natural) is
      Out_N     : constant Natural := N / 2;  --floor division for odd N
      Base      : Natural;
      Out_Index : Natural;
   begin
      Set_Dim (N);   --Value in DIM is required by the VHDL
      for r in 0 .. Out_N - 1 loop
         for c in 0 .. Out_N - 1 loop
            Base := (2 * r) * N + (2 * c);     --top-left of 2x2 window in A
            -- '*2' because stride = 2
            -- '*N' to make it a flat index.
            Out_Index := r * Out_N + c;        --flat index into R
            -- '*Out_N' to make it a flat index
            Set_Pool_Base_Index (Base);
            Set_Out_Index (Out_Index);
            Perform_Max_Pool;
            Wait_While_Busy;
            Write_Reg (CTRL_Addr, 0); --De-assert start
         end loop;
      end loop;
   end Apply_MaxPool_2x2_All_Words;

   --2x2 average pooling over entire tensor
   --Produces (N/2) x (N/2) outputs in R
   procedure Apply_AvgPool_2x2_All_Words (N : Natural) is
      Out_N     : constant Natural := N / 2;  --floor division for odd N
      Base      : Natural;
      Out_Index : Natural;
   begin
      Set_Dim (N);
      for r in 0 .. Out_N - 1 loop
         for c in 0 .. Out_N - 1 loop
            Base := (2 * r) * N + (2 * c);     --top-left of 2x2 window in A
            -- '*2' because stride = 2
            -- '*N' to make it a flat index.
            Out_Index := r * Out_N + c;        --flat index into R
            -- '*Out_N' to make it a flat index
            Set_Pool_Base_Index (Base);
            Set_Out_Index (Out_Index);
            Perform_Avg_Pool;
            Wait_While_Busy;
            Write_Reg (CTRL_Addr, 0); --De-assert start
         end loop;
      end loop;
   end Apply_AvgPool_2x2_All_Words;

   --  --Apply Softmax to entire N×N tensor using two-pass algorithm
   --  --Pass 1: Compute exponents for all elements (NPU writes to A in-place)
   --  --Pass 2: Ada calculates sum (inverted sum), then VHDL divides each element by sum
   --  procedure Apply_Softmax_All_Words
   --    (N : Natural; One_Dimensional : Boolean := False)
   --  is
   --     Words          : constant Natural := Tensor_Words (N, One_Dimensional);
   --     Sum            : Unsigned_32 := 0;
   --     Inverted_Sum   : Unsigned_32 := 0;
   --     W              : Word;
   --     B0, B1, B2, B3 : Unsigned_Byte;

   --     --How many elements are actually valid in A for this softmax call
   --     Total_Elements : constant Natural :=
   --       (if One_Dimensional then N else N * N);

   --     --How many words are fully valid
   --     Full_Words : constant Natural := Total_Elements / 4;

   --     --Valid bytes in the last word: 0 to 3
   --     Left_Over_Bytes : constant Natural := Total_Elements mod 4;
   --  begin
   --     --Pass 1: Compute exponents (VHDL writes to A in-place)
   --     Set_Softmax_Mode (SOFTMAX_MODE_EXP);  --Set mode to EXP

   --     for I in 0 .. Words - 1 loop
   --        Set_Word_Index (I);
   --        Perform_Softmax;
   --        Wait_While_Busy;
   --        Write_Reg (CTRL_Addr, 0); --De-assert start
   --     end loop;

   --     --Mask unused lanes in the last partial word (if any)
   --     --Prevent extra lanes from influening  probability
   --     if Left_Over_Bytes /= 0 then
   --        --For a partial tail, Words = Full_Words + 1, so last word index is Full_Words
   --        W := Read_Word_From_A (Full_Words);
   --        Unpack_Four_Bytes (W, B0, B1, B2, B3);

   --        case Left_Over_Bytes is
   --           when 1      =>
   --              B1 := 0;
   --              B2 := 0;
   --              B3 := 0;

   --           when 2      =>
   --              B2 := 0;
   --              B3 := 0;

   --           when 3      =>
   --              B3 := 0;

   --           when others =>
   --              --errors for missizng case values
   --              null;
   --        end case;

   --        W := Pack_Four_Bytes (B0, B1, B2, B3);
   --        Write_Word_In_A (Full_Words, W);
   --     end if;

   --     --Calculate sum here because mantaing an automatic sum register (accumulator) in the NPU is difficult
   --     --If the NPU accumulates the sum, then we have multiple drivers problems as the Ada program needs to reset the sum
   --     --It is also expensive from an LUT usage/inelegant to put sum reg write in the NPU FSM
   --     --
   --     --Sum only the valid elements (not the padded bytes in the last packed word).
   --     if Full_Words /= 0 then
   --        for I in 0 .. Full_Words - 1 loop
   --           W := Read_Word_From_A (I);
   --           Unpack_Four_Bytes (W, B0, B1, B2, B3);
   --           Sum :=
   --             Sum
   --             + Unsigned_32 (B0)
   --             + Unsigned_32 (B1)
   --             + Unsigned_32 (B2)
   --             + Unsigned_32 (B3);
   --        end loop;
   --     end if;

   --     if Left_Over_Bytes /= 0 then
   --        W := Read_Word_From_A (Full_Words);
   --        Unpack_Four_Bytes (W, B0, B1, B2, B3);
   --        case Left_Over_Bytes is
   --           when 1      =>
   --              Sum := Sum + Unsigned_32 (B0);

   --           when 2      =>
   --              Sum := Sum + Unsigned_32 (B0) + Unsigned_32 (B1);

   --           when 3      =>
   --              Sum :=
   --                Sum + Unsigned_32 (B0) + Unsigned_32 (B1) + Unsigned_32 (B2);

   --           when others =>
   --              --errors for missizng case values
   --              null;
   --        end case;
   --     end if;

   --     --Calculate inverted sum: (2^16) / sum
   --     --This allows hardware to do fast multiplication instead of division
   --     if Sum > 0 then
   --        Inverted_Sum :=
   --          (2 ** 16) / Sum;  --Fixed-point reciprocal scaled by 2^16

   --     end if;

   --     --Pass 2: Provide inverted sum to hardware for multiplication-based division
   --     Set_Sum_Param (Word (Inverted_Sum));  --Write inverted sum parameter
   --     Set_Softmax_Mode (SOFTMAX_MODE_DIV); --Set mode to DIV

   --     for I in 0 .. Words - 1 loop
   --        Set_Word_Index (I);
   --        Perform_Softmax;
   --        Wait_While_Busy;
   --        Write_Reg (CTRL_Addr, 0); --De-assert start
   --     end loop;
   --  end Apply_Softmax_All_Words;


   --Dense function
   --Computes Neurons outputs (1 output per NPU command) and stores them in R
   procedure Apply_Dense_All_Words
     (Inputs                           : Natural;
      Neurons                          : Natural;
      Weight_Base_Index                : Natural;
      Bias_Base_Index                  : Natural;
      Scale                            : Natural;
      Zero_Point                       : Integer;
      Quantized_Multiplier             : Integer;
      Quantized_Multiplier_Right_Shift : Natural)
   is
      Input_Base_Index : constant Natural := 0;

      W_Base  : Natural;
      B_Index : Natural;
   begin
      Set_N_Inputs (Inputs); --Set number of inputs to dense layer
      Set_Scale_Register (Scale);
      Set_Zero_Point (Zero_Point);
      Set_Quantized_Multiplier_Register (Quantized_Multiplier);
      Set_Quantized_Multiplier_Right_Shift_Register
        (Quantized_Multiplier_Right_Shift);
      --Put_Line ("Wrote N register. Starting loop");
      --One NPU dense command computes exactly one neuron output and writes one element to R
      for N in 0 .. Neurons - 1 loop
         --Per-neuron addressing
         --The FSM loops to multiply all inputs of a neuron with the connection weights internally
         W_Base :=
           Weight_Base_Index
           + (N * Inputs); --Multiple input weights for a neuron
         B_Index := Bias_Base_Index + (N); --One bias for a neuron.

         Set_Word_Index (Input_Base_Index); --A input index
         Set_Weight_Base_Index (W_Base);    --B weight base index
         Set_Bias_Base_Index (B_Index);       --C bias index
         Set_Out_Index (N);                 --R output index
         --Put_Line ("Set word index, weight base index, bias base index, and out index. Performing dense");
         Perform_Dense;
         --Put_Line("Started dense. Waiting now");
         Wait_While_Busy;
         --Put_Line ("Done waiting");
         Write_Reg
           (CTRL_Addr, 0); --De-assert start so next command can trigger
      end loop;
   end Apply_Dense_All_Words;


   

   --Q0.7 conversion
   --Range: [-1.0, 0.992) mapped to unsigned 0-255
   --Signed int8 = -128 to 127
   --If unsigned variant is <128, then number is positive
   --If unsigned num is >=128, then Q0.7 number is negative
   function Q07_To_Float (Value : Unsigned_Byte) return Float is
      Byte_Val : constant Unsigned_8 := Unsigned_8 (Value);
   begin
      if (Byte_Val < 128) then
         return Float (Byte_Val) / 128.0;
      else
         return Float (Integer (Byte_Val) - 256) / 128.0;
      end if;
   end Q07_To_Float;

   --Float should be [-1, 0.992) or [-1,1)
   --In signed int, -128 to 127. Multiply by 128 to convert float to int8 and then uint8
   --We need to use a normal int because * 128 makes it cross the limits -128 and 127
   --We can clamp this to -128 to 127. Similar logic to clipping in NumPy for quantization.
   --Float -> int8 -> uint8
   function Float_To_Q07 (Value : Float) return Unsigned_Byte is
      Scaled : Integer := Integer (Value * 128.0);
   begin
      if (Scaled <= -128) then
         Scaled := -128;
      elsif (Scaled > 127) then
         Scaled := 127;
      end if;

      if (Scaled < 0) then
         return Unsigned_Byte (Unsigned_8 (256 + Scaled));
      else
         return Unsigned_Byte (Unsigned_8 (Scaled));
      end if;
   end Float_To_Q07;


   --int8 -> uint8
   function Int_To_Q07 (Value : Integer) return Unsigned_Byte is
   begin
      if (Value <= -128) then
         return Unsigned_Byte (128);
      elsif (Value >= 127) then
         return Unsigned_Byte (127);
      elsif (Value < 0) then
         return Unsigned_Byte (Unsigned_8 (256 + Value));
      else
         return Unsigned_Byte (Unsigned_8 (Value));
      end if;
   end Int_To_Q07;

   --uint8 -> int8
   function Q07_To_Int (Value : Unsigned_Byte) return Integer is
      Byte_Val : constant Unsigned_8 := Unsigned_8 (Value);
   begin
      if (Byte_Val < 128) then
         return Integer (Byte_Val);
      else
         return Integer (Byte_Val) - 256;
      end if;
   end Q07_To_Int;



   

   --  --Create a Word_Array from an Integer_Array
   --  --Loops over an integer array to pack elements in an word array for writing to tensors
   --  procedure Create_Word_Array_From_Integer_Array
   --    (Integer_Source : in Integer_Array; Result_Word_Array : out Word_Array)
   --  is
   --     Result_Tensor_Words : constant Natural :=
   --       Tensor_Words
   --         (Integer_Source'Length, One_Dimensional => True); -- (N+3)/4

   --     --  Result_Word_Array : Word_Array (0 .. Result_Tensor_Words - 1) :=
   --     --    (others => 0);

   --     Left_Over_Bytes : constant Natural := Integer_Source'Length mod 4;
   --     Full_Words      : constant Natural :=
   --       (Integer_Source'Length - Left_Over_Bytes) / 4;

   --     W     : Word;
   --     Index : Integer := 0;
   --  begin
   --     if Result_Word_Array'Length < Result_Tensor_Words then
   --        Put_Line ("Result_Word_Array too small");
   --        return;
   --     end if;
   --     --Full 4-byte words
   --     if (Full_Words > 0) then
   --        for W_Index in 0 .. Full_Words - 1 loop
   --           W :=
   --             Pack_Four_Bytes
   --               (B0 => Int_To_Q07 (Integer_Source (Index)),
   --                B1 => Int_To_Q07 (Integer_Source (Index + 1)),
   --                B2 => Int_To_Q07 (Integer_Source (Index + 2)),
   --                B3 => Int_To_Q07 (Integer_Source (Index + 3)));
   --           Result_Word_Array (W_Index) := W;
   --           Index := Index + 4;
   --        end loop;
   --     end if;

   --     --Leftover partial word only if needed
   --     if Left_Over_Bytes /= 0 then
   --        declare
   --           B0 : constant Unsigned_Byte := Int_To_Q07 (Integer_Source (Index));
   --           B1 : constant Unsigned_Byte :=
   --             (if Left_Over_Bytes >= 2
   --              then Int_To_Q07 (Integer_Source (Index + 1))
   --              else 0);
   --           B2 : constant Unsigned_Byte :=
   --             (if Left_Over_Bytes >= 3
   --              then Int_To_Q07 (Integer_Source (Index + 2))
   --              else 0);
   --           B3 : constant Unsigned_Byte := 0;
   --        begin
   --           Result_Word_Array (Full_Words) := Pack_Four_Bytes (B0, B1, B2, B3);
   --        end;
   --     end if;
   --  end Create_Word_Array_From_Integer_Array;

end Ada_Ml;
