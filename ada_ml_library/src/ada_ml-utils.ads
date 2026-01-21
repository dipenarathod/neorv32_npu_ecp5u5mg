with Interfaces; use Interfaces;

package Ada_Ml.Utils is
   function Pack_Four_Bytes (B0, B1, B2, B3 : U8_T) return Unsigned_32;
   procedure Unpack_Four_Bytes (W : Unsigned_32; B0, B1, B2, B3 : out U8_T);
end Ada_Ml.Utils;