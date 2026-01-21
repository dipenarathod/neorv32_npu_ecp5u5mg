with interfaces;use interfaces;
package body Ada_Ml.Utils is

   --Pack four 8 bits into a 32-bit word
   function Pack_Four_Bytes (B0, B1, B2, B3 : Unsigned_Byte) return Word is
      U0 : constant Word := Word (B0);
      U1 : constant Word := Word (B1);
      U2 : constant Word := Word (B2);
      U3 : constant Word := Word (B3);
   begin
      return
        -- Word
          (U0
           or Shift_Left (Unsigned_32(U1), 8)
           or Shift_Left (Unsigned_32(U2), 16)
           or Shift_Left (Unsigned_32(U3), 24));
   end Pack_Four_Bytes;


   --Reuse unpack byte at index
   procedure Unpack_Four_Bytes (W : Word; B0, B1, B2, B3 : out Unsigned_Byte)
   is
   begin
      B0 := Unpack_Byte_At_Index (W, 0);
      B1 := Unpack_Byte_At_Index (W, 1);
      B2 := Unpack_Byte_At_Index (W, 2);
      B3 := Unpack_Byte_At_Index (W, 3);
   end Unpack_Four_Bytes;




   --Create a Word_Array from an Integer_Array
   --Loops over an integer array to pack elements in an word array for writing to tensors
   procedure Create_Word_Array_From_Integer_Array
     (Integer_Source : in Integer_Array; Result_Word_Array : out Word_Array)
   is
      Result_Tensor_Words : constant Natural :=
        Tensor_Words
          (Integer_Source'Length, One_Dimensional => True); -- (N+3)/4

      --  Result_Word_Array : Word_Array (0 .. Result_Tensor_Words - 1) :=
      --    (others => 0);

      Left_Over_Bytes : constant Natural := Integer_Source'Length mod 4;
      Full_Words      : constant Natural :=
        (Integer_Source'Length - Left_Over_Bytes) / 4;

      W     : Word;
      Index : Integer := 0;
   begin

      --Full 4-byte words
      if (Full_Words > 0) then
         for W_Index in 0 .. Full_Words - 1 loop
            W :=
              Pack_Four_Bytes
                (B0 => Int_To_Q07 (Integer_Source (Index)),
                 B1 => Int_To_Q07 (Integer_Source (Index + 1)),
                 B2 => Int_To_Q07 (Integer_Source (Index + 2)),
                 B3 => Int_To_Q07 (Integer_Source (Index + 3)));
            Result_Word_Array (W_Index) := W;
            Index := Index + 4;
         end loop;
      end if;

      --Leftover partial word only if needed
      if Left_Over_Bytes /= 0 then
         declare
            B0 : constant Unsigned_Byte := Int_To_Q07 (Integer_Source (Index));
            B1 : constant Unsigned_Byte :=
              (if Left_Over_Bytes >= 2
               then Int_To_Q07 (Integer_Source (Index + 1))
               else 0);
            B2 : constant Unsigned_Byte :=
              (if Left_Over_Bytes >= 3
               then Int_To_Q07 (Integer_Source (Index + 2))
               else 0);
            B3 : constant Unsigned_Byte := 0;
         begin
            Result_Word_Array (Full_Words) := Pack_Four_Bytes (B0, B1, B2, B3);
         end;
      end if;
   end Create_Word_Array_From_Integer_Array;
end Ada_Ml.Utils;