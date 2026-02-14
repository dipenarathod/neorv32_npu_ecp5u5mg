with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;

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
      Put (Word'Image (Word (DIM_Val)));
      New_Line;

      Put ("WORDI=");
      Put (Word'Image (Word (WORDI_Val)));
      New_Line;
   end Print_Registers;


end Ada_Ml.Debug;