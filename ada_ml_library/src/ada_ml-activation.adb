with Ada_Ml.Utils; use Ada_Ml.Utils;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;


package body Ada_Ml.Activation is

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

   --Apply Softmax to entire N×N tensor using two-pass algorithm
   --Pass 1: Compute exponents for all elements (NPU writes to A in-place)
   --Pass 2: Ada calculates sum (inverted sum), then VHDL divides each element by sum
   procedure Apply_Softmax_All_Words
     (N : Natural; One_Dimensional : Boolean := False)
   is
      Words          : constant Natural := Tensor_Words (N, One_Dimensional);
      Sum            : Word := 0;
      Inverted_Sum   : Word := 0;
      W              : Word;
      B0, B1, B2, B3 : Unsigned_Byte;

      --How many elements are actually valid in A for this softmax call
      Total_Elements : constant Natural :=
        (if One_Dimensional then N else N * N);

      --How many words are fully valid
      Full_Words : constant Natural := Total_Elements / 4;

      --Valid bytes in the last word: 0 to 3
      Left_Over_Bytes : constant Natural := Total_Elements mod 4;
   begin
      --Pass 1: Compute exponents (VHDL writes to A in-place)
      Set_Softmax_Mode (SOFTMAX_MODE_EXP);  --Set mode to EXP

      for I in 0 .. Words - 1 loop
         Set_Word_Index (I);
         Perform_Softmax;
         Wait_While_Busy;
         Write_Reg (CTRL_Addr, 0); --De-assert start
      end loop;

      --Mask unused lanes in the last partial word (if any)
      --Prevent extra lanes from influening  probability
      if Left_Over_Bytes /= 0 then
         --For a partial tail, Words = Full_Words + 1, so last word index is Full_Words
         W := Read_Word_From_A (Full_Words);
         Unpack_Four_Bytes (W, B0, B1, B2, B3);

         case Left_Over_Bytes is
            when 1      =>
               B1 := 0;
               B2 := 0;
               B3 := 0;

            when 2      =>
               B2 := 0;
               B3 := 0;

            when 3      =>
               B3 := 0;

            when others =>
               --errors for missizng case values
               null;
         end case;

         W := Pack_Four_Bytes (B0, B1, B2, B3);
         Write_Word_In_A (Full_Words, W);
      end if;

      --Calculate sum here because mantaing an automatic sum register (accumulator) in the NPU is difficult
      --If the NPU accumulates the sum, then we have multiple drivers problems as the Ada program needs to reset the sum
      --It is also expensive from an LUT usage/inelegant to put sum reg write in the NPU FSM
      --
      --Sum only the valid elements (not the padded bytes in the last packed word).
      if Full_Words /= 0 then
         for I in 0 .. Full_Words - 1 loop
            W := Read_Word_From_A (I);
            Unpack_Four_Bytes (W, B0, B1, B2, B3);
            Sum :=
              Sum
              + Word (B0)
              + Word (B1)
              + Word (B2)
              + Word (B3);
         end loop;
      end if;

      if Left_Over_Bytes /= 0 then
         W := Read_Word_From_A (Full_Words);
         Unpack_Four_Bytes (W, B0, B1, B2, B3);
         case Left_Over_Bytes is
            when 1      =>
               Sum := Sum + Word (B0);

            when 2      =>
               Sum := Sum + Word (B0) + Word (B1);

            when 3      =>
               Sum :=
                 Sum + Word (B0) + Word (B1) + Word (B2);

            when others =>
               --errors for missizng case values
               null;
         end case;
      end if;

      --Calculate inverted sum: (2^16) / sum
      --This allows hardware to do fast multiplication instead of division
      if Sum > 0 then
         Inverted_Sum :=
           (2 ** 16) / Sum;  --Fixed-point reciprocal scaled by 2^16

      end if;

      --Pass 2: Provide inverted sum to hardware for multiplication-based division
      Set_Sum_Param (Word (Inverted_Sum));  --Write inverted sum parameter
      Set_Softmax_Mode (SOFTMAX_MODE_DIV); --Set mode to DIV

      for I in 0 .. Words - 1 loop
         Set_Word_Index (I);
         Perform_Softmax;
         Wait_While_Busy;
         Write_Reg (CTRL_Addr, 0); --De-assert start
      end loop;
   end Apply_Softmax_All_Words;



end Ada_Ml.Activation;