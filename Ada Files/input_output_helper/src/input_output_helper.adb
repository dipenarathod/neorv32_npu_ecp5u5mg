with System.Address_To_Access_Conversions;
with System.Storage_Elements;
with Interfaces;    use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;

package body Input_Output_Helper is

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


end Input_Output_Helper;