with Ada.Text_IO; use Ada.Text_IO;
with Uart0;
with neorv32;             use neorv32;
with RISCV.CSR;           use RISCV.CSR;
with riscv.CSR_Generic;   use riscv.CSR_Generic;
--with Ada.Real_Time;  use Ada.Real_Time;
with System.Machine_Code; use System.Machine_Code;

package Input_Output_Helper.Time_Measurements is

   Clock_Hz     : constant Unsigned_64 := 72_000_000;
   --  Start_Cycles : Unsigned_64;
   --  End_Cycles   : Unsigned_64;
   --  Delta_Cycles : Unsigned_64;
   --Read 64-bit mcycle counter
   --Copied Read_CSR from riscvcsr_generic.adb because I can't use that directly here (as it is a generic subprogram)
   function Read_Cycle return Unsigned_64;
   procedure Print_Time (Name : String; Cycles : Unsigned_64);

end;