with Ada.Text_IO; use Ada.Text_IO;

with Interfaces; use Interfaces;

with Ada_Ml.Utils; use Ada_Ml.Utils;

package body Ada_Ml.Debug is


   --Print current register values to understand what is going on
   --should be useful (or not)
   procedure Print_Registers is
      CTRL_Val   : constant Word := Read_Reg (CTRL_Addr);
      STATUS_Val : constant Word := Read_Reg (STATUS_Addr);
      DIM_Val    : constant Word := Read_Reg (DIM_Addr);
      WORDI_Val  : constant Word := Read_Reg (WORDI_Addr);
   begin
      Put ("CTRL=");
      Put (Word'Image (Word (CTRL_Val)));
      New_Line;

      Put ("STATUS=");
      Put (Word'Image (Word (STATUS_Val)));
      New_Line;

      Put ("DIM=");
      Put (U32_T'Image (U32_T (DIM_Val)));
      New_Line;

      Put ("WORDI=");
      Put (U32_T'Image (U32_T (WORDI_Val)));
      New_Line;
   end Print_Registers;


      --Print a 2D tensor
   procedure Print_Tensor_Q07
     (Name : String; Data : Word_Array; Dimension : Natural)
   is
      B0, B1, B2, B3  : Unsigned_Byte := 0; --Bytes extracted from a word
      Float_Val       : Float; --Float to store float representation
      Last_Word_Index : Natural := Natural'Last; --Index of last word
   begin
      Put_Line (Name);
      for Row in 0 .. Dimension - 1 loop
         --Traverse rows
         Put (" [");
         for Col in 0 .. Dimension - 1 loop
            --Traverse columns
            declare
               Index      : constant Natural :=
                 Row
                 * Dimension
                 + Col;  --2D index modded to work with 1D representations
               Word_Index : constant Natural := Index / 4;  --Word index
               Byte_Sel   : constant Natural :=
                 Index mod 4;   --Byte index within word
            begin

               --if Word_Index /= Last_Word_Index then
               Unpack_Four_Bytes (Data (Word_Index), B0, B1, B2, B3);
               Last_Word_Index := Word_Index;
               --end if;

               case Byte_Sel is
                  when 0      =>
                     Float_Val := Q07_To_Float (B0);

                  when 1      =>
                     Float_Val := Q07_To_Float (B1);

                  when 2      =>
                     Float_Val := Q07_To_Float (B2);

                  when 3      =>
                     Float_Val := Q07_To_Float (B3);

                  when others =>
                     Float_Val := 0.0;
               end case;

               Put (" ");
               Put (Float'Image (Float_Val));
               Put (", ");
            end;
         end loop;
         Put_Line ("]");
      end loop;
      New_Line;
   end Print_Tensor_Q07;


   --Print a 1D tensor (vector)
   procedure Print_Vector_Q07
     (Name : String; Data : Word_Array; Vector_Length : Natural)
   is
      B0, B1, B2, B3  : Unsigned_Byte := 0; --Bytes extracted from a word
      Float_Val       : Float; --Float to store float representation
      Last_Word_Index : Natural := Natural'Last; --Index of last word
   begin
      Put_Line (Name);
      Put (" [");
      for Index in 0 .. Vector_Length - 1 loop
         --Traverse vector
         declare
            Word_Index : constant Natural := Index / 4;  --Word index
            Byte_Sel   : constant Natural :=
              Index mod 4;   --Byte index within word
         begin

            --if Word_Index /= Last_Word_Index then
            Unpack_Four_Bytes (Data (Word_Index), B0, B1, B2, B3);
            Last_Word_Index := Word_Index;
            --end if;

            case Byte_Sel is
               when 0      =>
                  Float_Val := Q07_To_Float (B0);

               when 1      =>
                  Float_Val := Q07_To_Float (B1);

               when 2      =>
                  Float_Val := Q07_To_Float (B2);

               when 3      =>
                  Float_Val := Q07_To_Float (B3);

               when others =>
                  Float_Val := 0.0;
            end case;

            Put (" ");
            Put (Float'Image (Float_Val));
            Put (", ");
         end;
      end loop;
      Put_Line ("]");
      New_Line;
   end Print_Vector_Q07;


end Ada_Ml.Debug;