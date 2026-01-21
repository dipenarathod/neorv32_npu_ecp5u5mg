with Ada_Ml;      use Ada_Ml;
with Ada_Ml.Debug;use Ada_Ml.Debug;
with Ada_Ml.Utils; use Ada_Ml.Utils;
with Ada_Ml.Activation; use Ada_Ml.Activation;
with Ada_Ml.Pooling; use Ada_Ml.Pooling;
with Ada_Ml.Dense; use Ada_Ml.Dense;
with Interfaces;          use Interfaces;
with Ada.Text_IO;         use Ada.Text_IO;
with Uart0;
with Runtime_Support;
with neorv32;             use neorv32;
with RISCV.CSR;           use RISCV.CSR;
with riscv.CSR_Generic;   use riscv.CSR_Generic;
--with Ada.Real_Time;  use Ada.Real_Time;
with System.Machine_Code; use System.Machine_Code;

procedure Test_Cases_Neorv32 is

   Clock_Hz     : constant Unsigned_64 := 72_000_000;
   Start_Cycles : Unsigned_64;
   End_Cycles   : Unsigned_64;
   Delta_Cycles : Unsigned_64;
   --Read 64-bit mcycle counter
   --Copied Read_CSR from riscvcsr_generic.adb because I can't use that directly here (as it is a generic subprogram)
   function Read_Cycle return Unsigned_64 is
      Low  : Unsigned_32;
      High : Unsigned_32;
   begin
      --Read low 32 bits
      Asm
        ("csrr %0, mcycle",
         Outputs  => Unsigned_32'Asm_Output ("=r", Low),
         Volatile => True);

      --Read high 32 bits
      Asm
        ("csrr %0, mcycleh",
         Outputs  => Unsigned_32'Asm_Output ("=r", High),
         Volatile => True);

      return Shift_Left (Unsigned_64 (High), 32) or Unsigned_64 (Low);
   end Read_Cycle;


   procedure Print_Time (Name : String; Cycles : Unsigned_64) is
      Microseconds : constant Unsigned_64 := (Cycles * 1_000_000) / Clock_Hz;
   begin
      Put_Line (Name & " cycles =" & Unsigned_64'Image (Cycles));
      Put_Line (Name & " time (us) =" & Unsigned_64'Image (Microseconds));
   end Print_Time;


   --Test pass or fail result print
   procedure Print_Result (Name : String; Passed : Boolean) is
   begin
      if Passed then
         Put_Line (Name & " PASS");
      else
         Put_Line (Name & " FAIL");
      end if;
   end Print_Result;

   --Same generator as the C tests
   procedure Build_Tensor (Words : Natural; Out_W : out Word_Array) is
   begin
      for i in 0 .. Words - 1 loop
         declare
            base : constant Integer := Integer (i) * 4 - 40;
            b0   : constant Unsigned_Byte := Int_To_Q07 (base);
            b1   : constant Unsigned_Byte := Int_To_Q07 (base + 16);
            b2   : constant Unsigned_Byte := Int_To_Q07 (base + 32);
            b3   : constant Unsigned_Byte := Int_To_Q07 (base + 48);
         begin
            Out_W (i) := Pack_Four_Bytes (b0, b1, b2, b3);
         end;
      end loop;
   end Build_Tensor;


   --Software ReLU
   function ReLU_Sw (X : Integer) return Integer is
   begin
      if (X < 0) then
         return 0;
      else
         return X;
      end if;
   end ReLU_Sw;

   --Software Sigmoid
   function Sigmoid_Sw (X : Integer) return Integer is
      Y : Integer := 64 + (X / 4);  --0.5 + x/4 in Q0.7 => 64 + (x>>2)
   begin
      if (Y < 0) then
         Y := 0;
      elsif (Y > 127) then
         Y := 127;
      end if;
      return Y;
   end Sigmoid_Sw;

   --1)write/read A must match
   procedure Test_A_Window_Echo_4x4 is
      N     : constant Natural := 4;
      Words : constant Natural := Tensor_Words (N);
      Tx    : Word_Array (0 .. Words - 1) := (others => 0);
      Rx    : Word_Array (0 .. Words - 1) := (others => 0);
      Same  : Boolean := True;
   begin
      Build_Tensor (Words, Tx);

      Start_Cycles := Read_Cycle;
      Write_Words_In_A (Tx);
      End_Cycles := Read_Cycle;
      Delta_Cycles := End_Cycles - Start_Cycles;
      Print_Time ("Time taken to write words to A:", Delta_Cycles);

      for i in 0 .. Words - 1 loop
         Rx (i) := Read_Word_From_A (i);
         if (Rx (i) /= Tx (i)) then
            Same := False;
            exit;
         end if;
      end loop;
      --Print_Tensor_Q07 (Name => "Input Tensor", Data => Tx, Dimension => N);
      --Print_Tensor_Q07 (Name => "Read Tensor", Data => Rx, Dimension => N);
      Print_Result ("Words written == words read from A", Same);
   end Test_A_Window_Echo_4x4;

   --Invalid opcode should keep R unchanged
   procedure Test_Invalid_Opcode_Result is
      N                  : constant Natural := 4;
      Words              : constant Natural := Tensor_Words (N);
      Invalid_Opcode     : constant Word := 99;
      OB0, OB1, OB2, OB3 : Unsigned_Byte :=
        0; --Bytes extracted from a word (original R)
      B0, B1, B2, B3     : Unsigned_Byte := 0; --Bytes extracted from a word
      Original           : Word_Array (0 .. Words - 1) := (others => 0);
      Rx                 : Word_Array (0 .. Words - 1) := (others => 0);
      OK                 : Boolean := True;
   begin
      Start_Cycles := Read_Cycle;
      Read_Words_From_R (Original);
      End_Cycles := Read_Cycle;
      Delta_Cycles := End_Cycles - Start_Cycles;
      Print_Time ("Time taken to read words from R:", Delta_Cycles);
      Set_Dim (N);
      Perform_Op (Invalid_Opcode);
      Wait_While_Busy;
      Write_Reg (CTRL_Addr, 0); --De-assert start
      Read_Words_From_R (Rx);
      for I in Rx'Range loop
         Unpack_Four_Bytes
           (Original (i), B0 => OB0, B1 => OB1, B2 => OB2, B3 => OB3);
         Unpack_Four_Bytes
           (W => Rx (i), B0 => B0, B1 => B1, B2 => B2, B3 => B3);
         if (B0 /= OB0 or B1 /= OB1 or B2 /= OB2 or B3 /= OB3) then
            OK := False;
            exit;
         end if;
      end loop;
      --Print_Tensor_Q07 ("Original Result Tesnsor", Original, N);
      --Print_Tensor_Q07 ("Result Tensor", Rx, N);
      Print_Result ("Invalid opcode should keeps R unchanged", OK);
   end Test_Invalid_Opcode_Result;


   --Test ReLU in 8x8 on some values
   procedure Test_ReLU_8x8 is
      N               : constant Natural := 8;
      Words           : constant Natural := Tensor_Words (N);
      Src             : Word_Array (0 .. Words - 1) := (others => 0);
      Out_Word_Tensor : Word_Array (0 .. Words - 1) := (others => 0);
      OK              : Boolean := True;
      --Test only some
      Samples         : constant array (Natural range <>) of Natural :=
        (0, 7, 15, 31, 48, 63);
   begin
      Build_Tensor (Words, Src);
      --Set_Dim (N);
      Write_Words_In_A (Src);
      Start_Cycles := Read_Cycle;
      Apply_ReLU_All_Words (N);
      End_Cycles := Read_Cycle;
      Delta_Cycles := End_Cycles - Start_Cycles;
      Print_Time ("Time taken to apply ReLU to A:", Delta_Cycles);
      Read_Words_From_R (Out_Word_Tensor);

      for S of Samples loop
         declare
            A_b : constant Unsigned_Byte := Get_Byte_From_Tensor (Src, S);
            R_b : constant Unsigned_Byte :=
              Get_Byte_From_Tensor (Out_Word_Tensor, S);
            A_i : constant Integer := Q07_To_Int (A_b);
            R_i : constant Integer := Q07_To_Int (R_b);
         begin
            if (R_i /= ReLU_Sw (A_i)) then
               OK := False;
               exit;
            end if;
         end;
      end loop;
      --Print_Tensor_Q07 (Name => "Input Tensor", Data => Src, Dimension => N);
      --Print_Tensor_Q07
      -- (Name => "Result ReLU 8x8", Data => Out_Word_Tensor, Dimension => N);
      Print_Result ("ReLU 8x8 samples match", OK);
   end Test_ReLU_8x8;

   --3) Test sigmoid in 8x8 tensor (on some samples)
   procedure Test_Sigmoid_8x8 is
      N               : constant Natural := 8;
      Words           : constant Natural := Tensor_Words (N);
      Src             : Word_Array (0 .. Words - 1) := (others => 0);
      Out_Word_Tensor : Word_Array (0 .. Words - 1) := (others => 0);
      OK              : Boolean := True;
      Samples         : constant array (Natural range <>) of Natural :=
        (0, 7, 15, 31, 48, 63);
   begin
      Build_Tensor (Words, Src);
      --Set_Dim (N);
      Write_Words_In_A (Src);
      Start_Cycles := Read_Cycle;
      Apply_Sigmoid_All_Words (N);
      End_Cycles := Read_Cycle;
      Delta_Cycles := End_Cycles - Start_Cycles;
      Print_Time ("Time taken to apply Sigmoid to A:", Delta_Cycles);
      Read_Words_From_R (Out_Word_Tensor);

      for S of Samples loop
         declare
            A_b : constant Unsigned_Byte := Get_Byte_From_Tensor (Src, S);
            R_b : constant Unsigned_Byte :=
              Get_Byte_From_Tensor (Out_Word_Tensor, S);
            A_i : constant Integer := Q07_To_Int (A_b);
            R_i : constant Integer := Q07_To_Int (R_b);
         begin
            if (R_i /= Sigmoid_Sw (A_i)) then
               OK := False;
               exit;
            end if;
         end;
      end loop;
      --Print_Tensor_Q07 (Name => "Input Tensor", Data => Src, Dimension => N);
      --Print_Tensor_Q07
      -- (Name      => "Result Sigmoid 8x8",
      --  Data      => Out_Word_Tensor,
      --  Dimension => N);
      Print_Result ("Sigmoid 8x8 samples match", OK);
   end Test_Sigmoid_8x8;

   --4) Test ReLU on a larger tensor to show logic works for tensors larger than 8x8
   procedure Test_ReLU_16x16 is
      N               : constant Natural := 16;
      Words           : constant Natural := Tensor_Words (N);
      Src             : Word_Array (0 .. Words - 1) := (others => 0);
      Out_Word_Tensor : Word_Array (0 .. Words - 1) := (others => 0);
      OK              : Boolean := True;
      Samples         : constant array (Natural range <>) of Natural :=
        (0, 90, 124, 220);
   begin
      Build_Tensor (Words, Src);
      --Set_Dim (N);
      Write_Words_In_A (Src);
      Start_Cycles := Read_Cycle;
      Apply_ReLU_All_Words (N);
      End_Cycles := Read_Cycle;
      Delta_Cycles := End_Cycles - Start_Cycles;
      Print_Time ("Time taken to apply 16x16 ReLU to A:", Delta_Cycles);
      Read_Words_From_R (Out_Word_Tensor);

      for S of Samples loop
         declare
            A_b : constant Unsigned_Byte := Get_Byte_From_Tensor (Src, S);
            R_b : constant Unsigned_Byte :=
              Get_Byte_From_Tensor (Out_Word_Tensor, S);
            A_i : constant Integer := Q07_To_Int (A_b);
            R_i : constant Integer := Q07_To_Int (R_b);
         begin
            if (R_i /= ReLU_Sw (A_i)) then
               OK := False;
               exit;
            end if;
         end;
      end loop;
      -- Print_Tensor_Q07 (Name => "Input Tensor", Data => Src, Dimension => N);
      -- Print_Tensor_Q07
      --   (Name => "Result ReLU 16x16", Data => Out_Word_Tensor, Dimension => N);
      Print_Result ("ReLU 16x16 samples match", OK);
   end Test_ReLU_16x16;

   --5) Test 2x2 MaxPool on a hard-coded 4x4 tensor
   procedure Test_MaxPool_2x2_8x8 is
      N        : constant Natural := 8;
      Words_A  : constant Natural := Tensor_Words (N);
      --Hard-coded 8x8 tensor (row-major), int8 values mapped to Q0.7
      --Rows:
      --[  4,   8,  -12,  -4,   4,  8,  -12,  -4]
      --[  0,   4,   8,   12,   0,  4,   8,   12]
      --[ -16, -12,  16,  20, -16, -12,  16,  20]
      --[  -8,  -4,  24,  28,  -8,  -4,  24,  28]
      --[ 120, 121,  64, 127, 120, 121,  64,  127]
      --[ 80,   81,  75,  82,  80,  81,  75,  82]
      --[ 90,   84,  74,  28, -90, -84, -74, -28]
      --[  8,   -4, -24,  -8. -80, -81, -75, -82]
      A_Tensor : constant Word_Array (0 .. Words_A - 1) :=
        (0  =>
           Pack_Four_Bytes
             (Int_To_Q07 (4),
              Int_To_Q07 (8),
              Int_To_Q07 (-12),
              Int_To_Q07 (-4)),
         1  =>
           Pack_Four_Bytes
             (Int_To_Q07 (4),
              Int_To_Q07 (8),
              Int_To_Q07 (-12),
              Int_To_Q07 (-4)),
         2  =>
           Pack_Four_Bytes
             (Int_To_Q07 (0), Int_To_Q07 (4), Int_To_Q07 (8), Int_To_Q07 (12)),
         3  =>
           Pack_Four_Bytes
             (Int_To_Q07 (0), Int_To_Q07 (4), Int_To_Q07 (8), Int_To_Q07 (12)),
         4  =>
           Pack_Four_Bytes
             (Int_To_Q07 (-16),
              Int_To_Q07 (-12),
              Int_To_Q07 (16),
              Int_To_Q07 (20)),
         5  =>
           Pack_Four_Bytes
             (Int_To_Q07 (-16),
              Int_To_Q07 (-12),
              Int_To_Q07 (16),
              Int_To_Q07 (20)),
         6  =>
           Pack_Four_Bytes
             (Int_To_Q07 (-8),
              Int_To_Q07 (-4),
              Int_To_Q07 (24),
              Int_To_Q07 (28)),
         7  =>
           Pack_Four_Bytes
             (Int_To_Q07 (-8),
              Int_To_Q07 (-4),
              Int_To_Q07 (24),
              Int_To_Q07 (28)),
         8  =>
           Pack_Four_Bytes
             (Int_To_Q07 (120),
              Int_To_Q07 (121),
              Int_To_Q07 (64),
              Int_To_Q07 (127)),
         9  =>
           Pack_Four_Bytes
             (Int_To_Q07 (120),
              Int_To_Q07 (121),
              Int_To_Q07 (64),
              Int_To_Q07 (127)),
         10 =>
           Pack_Four_Bytes
             (Int_To_Q07 (80),
              Int_To_Q07 (81),
              Int_To_Q07 (75),
              Int_To_Q07 (82)),
         11 =>
           Pack_Four_Bytes
             (Int_To_Q07 (80),
              Int_To_Q07 (81),
              Int_To_Q07 (75),
              Int_To_Q07 (82)),
         12 =>
           Pack_Four_Bytes
             (Int_To_Q07 (90),
              Int_To_Q07 (84),
              Int_To_Q07 (74),
              Int_To_Q07 (28)),
         13 =>
           Pack_Four_Bytes
             (Int_To_Q07 (-90),
              Int_To_Q07 (-84),
              Int_To_Q07 (-74),
              Int_To_Q07 (-28)),
         14 =>
           Pack_Four_Bytes
             (Int_To_Q07 (8),
              Int_To_Q07 (-4),
              Int_To_Q07 (-24),
              Int_To_Q07 (-8)),
         15 =>
           Pack_Four_Bytes
             (Int_To_Q07 (-80),
              Int_To_Q07 (-81),
              Int_To_Q07 (-75),
              Int_To_Q07 (-82)));
      Out_N    : constant Natural := N / 2; --Resulting tensor dimensions
      Words_R  : constant Natural := Tensor_Words (Out_N); --Words in tensor R
      R_Tensor : Word_Array (0 .. Words_R - 1) := (others => 0);
      OK       : Boolean := True;

      --Expected MaxPool 4x4 result:
      Expected : constant array (Natural range 0 .. 15) of Integer :=
        (8, 12, 8, 12, -4, 28, -4, 28, 121, 127, 121, 127, 90, 74, -80, -28);
   begin
      Set_Dim (N);
      Write_Words_In_A (A_Tensor);
      Start_Cycles := Read_Cycle;
      Apply_MaxPool_2x2_All_Words (N);
      End_Cycles := Read_Cycle;
      Delta_Cycles := End_Cycles - Start_Cycles;
      Print_Time ("Time taken to apply 2x2 Maxpool to 8x8 A:", Delta_Cycles);
      Read_Words_From_R (R_Tensor);

      --Verify all 16 outputs
      for index in 0 .. 15 loop
         declare
            rb : constant Unsigned_Byte :=
              Get_Byte_From_Tensor (R_Tensor, index);
            ri : constant Integer := Q07_To_Int (rb);
         begin
            if (ri /= Expected (index)) then
               OK := False;
               exit;
            end if;
         end;
      end loop;

      --Print_Tensor_Q07 ("Input 8x8", A_Tensor, N);
      --Print_Tensor_Q07 ("MaxPool 2x2 -> 4x4", R_Tensor, Out_N);
      Print_Result ("MaxPool 2x2 on hard-coded 8x8", OK);
   end Test_MaxPool_2x2_8x8;

   --6) Test 2x2 AvgPool on the same hard-coded 4x4 tensor
   procedure Test_AvgPool_2x2_4x4 is
      N        : constant Natural := 4;
      Words_A  : constant Natural := Tensor_Words (N);
      A_Tensor : constant Word_Array (0 .. Words_A - 1) :=
        (0 =>
           Pack_Four_Bytes
             (Int_To_Q07 (4),
              Int_To_Q07 (8),
              Int_To_Q07 (-12),
              Int_To_Q07 (-4)),
         1 =>
           Pack_Four_Bytes
             (Int_To_Q07 (0), Int_To_Q07 (4), Int_To_Q07 (8), Int_To_Q07 (12)),
         2 =>
           Pack_Four_Bytes
             (Int_To_Q07 (-16),
              Int_To_Q07 (-12),
              Int_To_Q07 (16),
              Int_To_Q07 (20)),
         3 =>
           Pack_Four_Bytes
             (Int_To_Q07 (-8),
              Int_To_Q07 (-4),
              Int_To_Q07 (24),
              Int_To_Q07 (28)));
      Out_N    : constant Natural := N / 2; --2
      Words_R  : constant Natural := Tensor_Words (Out_N); --1
      R_Tensor : Word_Array (0 .. Words_R - 1) := (others => 0);
      OK       : Boolean := True;

      --Expected AvgPool 2x2 result
      Expected : constant array (Natural range 0 .. 3) of Integer :=
        (4, 1, -10, 22);
   begin
      Set_Dim (N);
      Write_Words_In_A (A_Tensor);
      Start_Cycles := Read_Cycle;
      Apply_AvgPool_2x2_All_Words (N);
      End_Cycles := Read_Cycle;
      Delta_Cycles := End_Cycles - Start_Cycles;
      Print_Time ("Time taken to apply 2x2 Maxpool to 4x4 A:", Delta_Cycles);
      Read_Words_From_R (R_Tensor);

      --Verify all 4 outputs
      for index in 0 .. 3 loop
         declare
            rb : constant Unsigned_Byte :=
              Get_Byte_From_Tensor (R_Tensor, index);
            ri : constant Integer := Q07_To_Int (rb);
         begin
            if (ri /= Expected (index)) then
               OK := False;
               exit;
            end if;
         end;
      end loop;

      --Print_Tensor_Q07 ("Input 4x4", A_Tensor, N);
      --Print_Tensor_Q07 ("AvgPool 2x2 -> 2x2", R_Tensor, Out_N);
      Print_Result ("AvgPool 2x2 on hard-coded 4x4", OK);
   end Test_AvgPool_2x2_4x4;


   --7) Test Softmax on a hard-coded 4x4 tensor
   --Softmax uses two-pass algorithm:
   --Pass 1: Compute e^x (using linear approx e^x = 1+x) for all elements
   --Pass 2: Divide each exponent by sum to normalize
   procedure Test_Softmax_4x4 is
      N       : constant Natural := 4;
      Words_A : constant Natural := Tensor_Words (N);  --4 words for 4x4

      --Hard-coded 4x4 input tensor in Q0.7 signed int8 format
      --Input values: [0, 32, 64, 96],
      --             [0, 32, 64, 96],
      --             [0, 32, 64, 96],
      --             [0, 32, 64, 96],
      --In float: [0.0, 0.25, 0.5, 0.75]
      A_Tensor : constant Word_Array (0 .. Words_A - 1) :=
        (0 =>
           Pack_Four_Bytes
             (Int_To_Q07 (0),
              Int_To_Q07 (32),
              Int_To_Q07 (64),
              Int_To_Q07 (96)),
         1 =>
           Pack_Four_Bytes
             (Int_To_Q07 (0),
              Int_To_Q07 (32),
              Int_To_Q07 (64),
              Int_To_Q07 (96)),
         2 =>
           Pack_Four_Bytes
             (Int_To_Q07 (0),
              Int_To_Q07 (32),
              Int_To_Q07 (64),
              Int_To_Q07 (96)),
         3 =>
           Pack_Four_Bytes
             (Int_To_Q07 (0),
              Int_To_Q07 (32),
              Int_To_Q07 (64),
              Int_To_Q07 (96)));

      R_Tensor : Word_Array (0 .. Words_A - 1) := (others => 0);
      OK       : Boolean := True;

      --Expected Softmax output in Q0.7 unsigned format (0-127 range)
      --e^0 = 1.0 = 128, e^0.25 = 1.25 = 160, e^0.5 = 1.5 = 192, e^0.75 = 1.75 = 224
      --Sum of exponents = (128+160+192+224) × 4 = 2816
      --After division by sum and scaling by 128:
      --128*128/2816 = 5, 160*128/2816 = 7, 192*128/2816 = 8, 224*128/2816 = 10
      --In the output:
      --5 is rounded to 6 = 4.68750E-02
      --7 stays the same = 5.46875E-02
      --8 is rounded to 9 = 7.03125E-02
      --10 stays the same = 7.81250E-02
      Expected : constant array (Natural range 0 .. 15) of Unsigned_Byte :=
        (5, 7, 8, 10, 5, 7, 8, 10, 5, 7, 8, 10, 5, 7, 8, 10);
   begin
      --Write input tensor to A
      Write_Words_In_A (A_Tensor);

      --Apply softmax (two-pass algorithm with Ada calculating sum)
      Start_Cycles := Read_Cycle;
      Apply_Softmax_All_Words (N);
      End_Cycles := Read_Cycle;
      Delta_Cycles := End_Cycles - Start_Cycles;
      Print_Time ("Time taken to apply Softmax to 4x4 A:", Delta_Cycles);

      --Read result from R (softmax writes normalized output to R)
      Read_Words_From_R (R_Tensor);

      --Verify all 16 outputs
      for index in 0 .. 15 loop
         declare
            result_byte : constant Unsigned_Byte :=
              Get_Byte_From_Tensor (R_Tensor, index);
         begin
            --Allow +/- 1 tolerance due to rounding in fixed-point division
            --Before the stack overflow rounding implementation, there was a lot more rounding error (expected)
            if (result_byte < Expected (index) - 1
                or result_byte > Expected (index) + 1)
            then
               OK := False;
            end if;
         end;
      end loop;

      Print_Tensor_Q07 ("Input 4x4", A_Tensor, N);
      Print_Tensor_Q07 ("Softmax 4x4", R_Tensor, N);

      Print_Result ("Softmax on hard-coded 4x4", OK);
   end Test_Softmax_4x4;


   --9 Inputs, 5 neurons (odd numbers > than a multiple of to test lane logic)
   --Inputs = (1,2,3,4,5,6,7,8,9)
   --Weights=(16,16,16,16,16,16,16,16,16,
   --        16,16,16,16, 0,0,0,0,0,
   --         0,0,0,0,0,0,0,0,0,
   --       -16,-16,-16,-16,-16,-16,-16,-16,-16,
   --       32,32,32,32,32,32,32,32,32)
   --Biases=(0,0,2,1,-1)
   --Expected=(720,160,2,-719,1440)
   -- procedure Test_Dense is
   --    Inputs   : constant Natural := 9;
   --    Neurons  : constant Natural := 5;
   --    Words_A  : Natural := Tensor_Words (Inputs, True);
   --    A_Tensor : constant Word_Array (0 .. Words_A - 1) :=   --Input tensor
   --      (0 =>
   --         Pack_Four_Bytes
   --           (Int_To_Q07 (1), Int_To_Q07 (2), Int_To_Q07 (3), Int_To_Q07 (4)),
   --       1 =>
   --         Pack_Four_Bytes
   --           (Int_To_Q07 (5), Int_To_Q07 (6), Int_To_Q07 (7), Int_To_Q07 (8)),
   --       2 =>
   --         Pack_Four_Bytes
   --           (Int_To_Q07 (9), Int_To_Q07 (0), Int_To_Q07 (0), Int_To_Q07 (0)));
   --    Words_B  : Natural := Tensor_Words (45, True);
   --    B_Tensor : constant Word_Array (0 .. Words_B - 1) :=
   --      (0  =>
   --         Pack_Four_Bytes
   --           (Int_To_Q07 (16),
   --            Int_To_Q07 (16),
   --            Int_To_Q07 (16),
   --            Int_To_Q07 (16)),
   --       1  =>
   --         Pack_Four_Bytes
   --           (Int_To_Q07 (16),
   --            Int_To_Q07 (16),
   --            Int_To_Q07 (16),
   --            Int_To_Q07 (16)),
   --       2  =>
   --         Pack_Four_Bytes
   --           (Int_To_Q07 (16),
   --            Int_To_Q07 (16),
   --            Int_To_Q07 (16),
   --            Int_To_Q07 (16)),
   --       3  =>
   --         Pack_Four_Bytes
   --           (Int_To_Q07 (16), Int_To_Q07 (0), Int_To_Q07 (0), Int_To_Q07 (0)),
   --       4  =>
   --         Pack_Four_Bytes
   --           (Int_To_Q07 (0), Int_To_Q07 (0), Int_To_Q07 (0), Int_To_Q07 (0)),
   --       5  =>
   --         Pack_Four_Bytes
   --           (Int_To_Q07 (0), Int_To_Q07 (0), Int_To_Q07 (0), Int_To_Q07 (0)),
   --       6  =>
   --         Pack_Four_Bytes
   --           (Int_To_Q07 (0),
   --            Int_To_Q07 (0),
   --            Int_To_Q07 (0),
   --            Int_To_Q07 (-16)),
   --       7  =>
   --         Pack_Four_Bytes
   --           (Int_To_Q07 (-16),
   --            Int_To_Q07 (-16),
   --            Int_To_Q07 (-16),
   --            Int_To_Q07 (-16)),
   --       8  =>
   --         Pack_Four_Bytes
   --           (Int_To_Q07 (-16),
   --            Int_To_Q07 (-16),
   --            Int_To_Q07 (-16),
   --            Int_To_Q07 (-16)),
   --       9  =>
   --         Pack_Four_Bytes
   --           (Int_To_Q07 (32),
   --            Int_To_Q07 (32),
   --            Int_To_Q07 (32),
   --            Int_To_Q07 (32)),
   --       10 =>
   --         Pack_Four_Bytes
   --           (Int_To_Q07 (32),
   --            Int_To_Q07 (32),
   --            Int_To_Q07 (32),
   --            Int_To_Q07 (32)),
   --       11 =>
   --         Pack_Four_Bytes
   --           (Int_To_Q07 (32),
   --            Int_To_Q07 (0),
   --            Int_To_Q07 (0),
   --            Int_To_Q07 (0)));
   --    Words_C  : Natural := Tensor_Words (5, True);
   --    C_Tensor : constant Word_Array (0 .. Words_C - 1) :=
   --      (0 =>
   --         Pack_Four_Bytes
   --           (Int_To_Q07 (0), Int_To_Q07 (0), Int_To_Q07 (2), Int_To_Q07 (1)),
   --       1 =>
   --         Pack_Four_Bytes
   --           (Int_To_Q07 (-1),
   --            Int_To_Q07 (0),
   --            Int_To_Q07 (0),
   --            Int_To_Q07 (0)));
   --    R_Tensor : Word_Array (0 .. Words_C - 1) := (others => 0);
   --    OK       : Boolean := True;
   --    Expected : constant array (Natural range 0 .. Neurons - 1) of Integer :=
   --      (5, 1, 2, -5, 10);
   -- begin
   --    --Put_Line("Starting dense test");
   --    Write_Words_In_A (A_Tensor);
   --    --Put_Line("Wrote words in A");
   --    Write_Words_In_B (B_Tensor);
   --    --Put_Line("Wrote words in B");
   --    Write_Words_In_C (C_Tensor);
   --    --Put_Line("Wrote words in C");
   --    Start_Cycles := Read_Cycle;
   --    Apply_Dense_All_Words (Inputs, Neurons, 0, 0);
   --    End_Cycles := Read_Cycle;
   --    Delta_Cycles := End_Cycles - Start_Cycles;
   --    Print_Time ("Time taken to apply Dense:", Delta_Cycles);


   --    --Read result from R (softmax writes normalized output to R)
   --    Read_Words_From_R (R_Tensor);

   --    --Verify all 5 outputs
   --    -- for index in 0 .. Neurons - 1 loop
   --    --    declare
   --    --       rb : constant Unsigned_Byte :=
   --    --         Get_Byte_From_Tensor (R_Tensor, index);
   --    --       ri : constant Integer := Q07_To_Int (rb);
   --    --    begin
   --    --       if (ri /= Expected (index)) then
   --    --          OK := False;
   --    --          exit;
   --    --       end if;
   --    --    end;
   --    -- end loop;
   --    Print_Vector_Q07 ("Dense Result", R_Tensor, Neurons);
   --    Print_Result ("Dense on hard-coded tensors", OK);
   -- end Test_Dense;

 

   --8) Dense 5x5 test with int32 biases in tensor C + requantization + clamping.
   procedure Test_Dense_5x5_Requant_Clamp is
      Inputs  : constant Natural := 5;
      Neurons : constant Natural := 5;

      --Input vector x = [1,2,3,4,5]
      X : constant Integer_Array (0 .. Inputs - 1) := (1, 2, 3, 4, 5);

      --25 weights (5 per neuron), stored sequentially in B:
      --N0: all 10
      --N1: all -10
      --N2: all 50
      --N3: all 100
      --N4: all -100
      W        : constant Integer_Array (0 .. (Inputs * Neurons) - 1) :=
        (10,
         10,
         10,
         10,
         10,
         -10,
         -10,
         -10,
         -10,
         -10,
         50,
         50,
         50,
         50,
         50,
         100,
         100,
         100,
         100,
         100,
         -100,
         -100,
         -100,
         -100,
         -100);
      Words_A  : Natural := Tensor_Words (X'Length, True);
      A_Tensor : Word_Array (0 .. Words_A - 1) := (others => 0);
      Words_B  : Natural := Tensor_Words (W'Length, True);
      B_Tensor : Word_Array (0 .. Words_B - 1) := (others => 0);

      Words_R  : Natural := Tensor_Words (Neurons, One_Dimensional => True);
      R_Tensor : Word_Array (0 .. Words_R - 1) := (others => 0);

      --Biases as true int32 words in C at word indices 0..4
      Biases : constant array (Natural range 0 .. Neurons - 1) of Integer :=
        (0, 0, 10_000, 40_000, -40_000);

      --Quantized multiplier and shift:
      --qm = 2^30, shift = 7 => output = round((acc+bias)/256), then saturate to int8.
      Quant_Mult  : constant Integer := 16#4000_0000#;--16x4000_0000 = 2^30
      Right_Shift : constant Natural := 7;

      --N0 acc: 1*10 + 2*10 + 3*10 + 4*10 + 5*10 = 150. Add bias = 150 + 0 = 150
      --Multiply by quantized multiplier = 150 * 2^30. Right shift (31+7=38) = 150 * 2^(30-38) = 150/256 = 0.5859 rounded to 1 (int8) 
      --N1 acc: 1*-10 + 2*-10 + 3*-10 + 4*-10 + 5*-10 = -150. Add bias = -150 + 0 = -150
      --Multiply by quantized multiplier = -150 * 2^30. Right shift (31+7=38) = -150 * 2^(30-38) = -150/256 = -0.5859 rounded to -1 (int8) 
      --N2 acc: 1*50 + 2*50 + 3*50 + 4*50 + 5*50 = 500. Add bias = 500 + 10000 = 10500
      --Multiply by quantized multiplier = 10500 * 2^30. Right shift (31+7=38) = 10500 * 2^(30-38) = 10500/256 = 41 rounded to 42 (int8) 
      --N3 acc: 1*100 + 2*100 + 3*100 + 4*100 + 5*100 = 1000. Add bias = 1000 + 40000 = 41000
      --Multiply by quantized multiplier = 41000 * 2^30. Right shift (31+7=38) = 41000 * 2^(30-38) = 41000/256 = 160 clamped to 127 (int8) 
      --N4 acc: 1*-100 + 2*-100 + 3*-100 + 4*-100 + 5*-100 = -1000. Add bias = -1000 + -40000 = -41000
      --Multiply by quantized multiplier = -41000 * 2^30. Right shift (31+7=38) = -41000 * 2^(30-38) = -41000/256 = -160 clamped to -128 (int8)
      Expected : constant array (Natural range 0 .. Neurons - 1) of Integer :=
        (1, -1, 42, 127, -128);
      OK : Boolean := True;
   begin
      --Load input + weights

      Create_Word_Array_From_Integer_Array (X, A_Tensor);
      Create_Word_Array_From_Integer_Array (W, B_Tensor);
      Write_Words_In_A (A_Tensor);
      Write_Words_In_B (B_Tensor);

      --Write int32 bias words into tensor C
      for N in 0 .. Neurons - 1 loop
         Write_Word_In_C (N, Word (Interfaces.Unsigned_32'Mod (Biases (N))));
      end loop;

      Start_Cycles := Read_Cycle;
      Apply_Dense_All_Words
        (Inputs                           => Inputs,
         Neurons                          => Neurons,
         Weight_Base_Index                => 0,
         Bias_Base_Index                  => 0,
         Scale                            => 0,
         Zero_Point                       => 0,
         Quantized_Multiplier             => Quant_Mult,
         Quantized_Multiplier_Right_Shift => Right_Shift);
      End_Cycles := Read_Cycle;

      Print_Time
        ("Time taken to apply Dense 5x5 (requant+clamp):",
         End_Cycles - Start_Cycles);

      Read_Words_From_R (R_Tensor);

      for N in 0 .. Neurons - 1 loop
         declare
            RB : constant Unsigned_Byte := Get_Byte_From_Tensor (R_Tensor, N);
            RI : constant Integer := Q07_To_Int (RB);
         begin
            if RI /= Expected (N) then
               OK := False;
               Put_Line
                 ("Dense mismatch neuron"
                  & Natural'Image (N)
                  & " got"
                  & Integer'Image (RI)
                  & " expected"
                  & Integer'Image (Expected (N)));
            end if;
         end;
      end loop;

      Print_Vector_Q07 ("Dense 5x5 outputs (Q0.7)", R_Tensor, Neurons);
      Print_Result ("Dense 5x5 requantization + clamp", OK);
   end Test_Dense_5x5_Requant_Clamp;


begin
   Uart0.Init (19200);
   Put_Line ("Reunning Test Cases----------------");
   Test_A_Window_Echo_4x4;
   Test_Invalid_Opcode_Result;
   Test_ReLU_8x8;
   Test_Sigmoid_8x8;
   Test_MaxPool_2x2_8x8;
   Test_AvgPool_2x2_4x4;
   Test_Softmax_4x4;
   --Test_Dense;
   Test_Dense_5x5_Requant_Clamp;
   Put_Line ("Tests Done-------------------------");
   loop
      null;
   end loop;
end Test_Cases_Neorv32;
