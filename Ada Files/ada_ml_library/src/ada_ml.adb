with System.Address_To_Access_Conversions;
with System.Storage_Elements;
with Interfaces;    use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;

package body Ada_Ml is


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


end Ada_Ml;
