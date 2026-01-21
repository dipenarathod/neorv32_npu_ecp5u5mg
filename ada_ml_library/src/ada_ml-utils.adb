
package body Ada_Ml.Utils is

   --Pack four 8 bits into a 32-bit word
   function Pack_Four_Bytes (B0, B1, B2, B3 : U8_T) return Unsigned_32 is
      U0 : constant Unsigned_32 := Unsigned_32 (B0);
      U1 : constant Unsigned_32 := Unsigned_32 (B1);
      U2 : constant Unsigned_32 := Unsigned_32 (B2);
      U3 : constant Unsigned_32 := Unsigned_32 (B3);
   begin
      return
        -- Word
          (U0
           or Shift_Left (U1, 8)
           or Shift_Left (U2, 16)
           or Shift_Left (U3, 24));
   end Pack_Four_Bytes;


   --Reuse unpack byte at index
   procedure Unpack_Four_Bytes (W : Unsigned_32; B0, B1, B2, B3 : out U8_T)
   is
   begin
      B0 := Unpack_Byte_At_Index (W, 0);
      B1 := Unpack_Byte_At_Index (W, 1);
      B2 := Unpack_Byte_At_Index (W, 2);
      B3 := Unpack_Byte_At_Index (W, 3);
   end Unpack_Four_Bytes;

end Ada_Ml.Utils;