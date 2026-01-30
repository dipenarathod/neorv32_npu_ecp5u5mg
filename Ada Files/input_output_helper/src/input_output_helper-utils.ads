package Input_Output_Helper.Utils is

   function Pack_Four_Bytes (B0, B1, B2, B3 : Unsigned_Byte) return Word;
   procedure Unpack_Four_Bytes (W : Word; B0, B1, B2, B3 : out Unsigned_Byte);
   procedure Create_Word_Array_From_Integer_Array
     (Integer_Source : in Integer_Array; Result_Word_Array : out Word_Array);

end Input_Output_Helper.Utils;
