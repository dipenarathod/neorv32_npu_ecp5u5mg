with Interfaces; use Interfaces;
with System;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;
package Input_Output_Helper is

  --Rename types for easier use
  --Search https://www.google.com/search?q=how+to+give+a+type+another+name+in+ada&oq=how+to+give+a+type+another+name+in+&gs_lcrp=EgZjaHJvbWUqBwgBECEYoAEyBggAEEUYOTIHCAEQIRigATIHCAIQIRifBTIHCAMQIRifBdIBCDc4NDNqMGo3qAIAsAIA&client=ubuntu-chr&sourceid=chrome&ie=UTF-8

  subtype Unsigned_Byte is Interfaces.Unsigned_8;
  --subtype U8_T is Interfaces.Unsigned_8;

  subtype Word is Interfaces.Unsigned_32;
  --subtype U32_T is Interfaces.Unsigned_32;

  --Make types which are actually arrays(https://learn.adacore.com/courses/intro-to-ada/chapters/arrays.html)
  type Unsigned_Byte_Array is array (Natural range <>) of Unsigned_Byte;
  --type Signed_Byte_Array is array (Natural range <>) of Signed_Byte;

  type Word_Array is array (Natural range <>) of Word;
  --type U32_Array_T is array (Natural range <>) of U32_T;

  type Integer_Array is array (Natural range <>) of Integer;

   --Volatile word
   type Volatile_Word is new Unsigned_32;
   --Can't be dropped in favor of subtype because pragma does not work with subtype
   --Reusing word from .ads seems to not work. New declaration for the same works?
   --Works in favor because now we have volatile and non-volatile words
   --Volatile because may change any time
   pragma Volatile_Full_Access (Volatile_Word);

   --Can't use the "use" clause. is new works for package rename as well
   package Convert is new System.Address_To_Access_Conversions (Volatile_Word);

   --Convert address to volatile word pointer
   --access = pointers
   function R32 (Addr : System.Address) return access Volatile_Word;

   --Add byte offset to an address
   function Add_Byte_Offset
     (Address : System.Address; Offset : Unsigned_32) return System.Address;

   --Write value to a register
   procedure Write_Reg (Addr : System.Address; Value : Word);

   --Read val from register (need to dereference the word)
   function Read_Reg (Addr : System.Address) return Word;

   --Get an int8 at an index (offset) inside a 32-bit word
   --There are 4 bytes, W[7:0], W[15:8], W[23:16], W[31:24]
   --Depending on the index, we shift the word right index * 8 to force the desired byte to exist in positions 7:0
   function Unpack_Byte_At_Index
     (W : Word; Index : Natural) return Unsigned_Byte;



   --Get byte from tensor using word and byte index
   function Get_Byte_From_Tensor
     (Data : Word_Array; Index : Natural) return Unsigned_Byte;

   function Tensor_Words
     (N : Natural; One_Dimensional : Boolean := False) return Natural;


  --Q0.7 Fixed Point Conversions
  --Q0.7: 1 sign bit + 7 fractional bits
  --Range: [-1.0, 1.0) represented as 0-255
  function Float_To_Q07 (Value : Float) return Unsigned_Byte;
  function Q07_To_Float (Value : Unsigned_Byte) return Float;
  function Int_To_Q07 (Value : Integer) return Unsigned_Byte;
  function Q07_To_Int (Value : Unsigned_Byte) return Integer;

end Input_Output_Helper;
