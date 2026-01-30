package body Input_Output_Helper.Time_Measurements is

   function Read_Cycle return Unsigned_64 is
      Low  : Unsigned_32;
      High : Unsigned_32;
   begin
      --Read low 32 bits
      Asm
        ("csrr %0, mcycle",
         Outputs  => Unsigned_32'Asm_Output ("=r", Low),
         Volatile => True);

      --Read high 32 bits
      Asm
        ("csrr %0, mcycleh",
         Outputs  => Unsigned_32'Asm_Output ("=r", High),
         Volatile => True);

      return Shift_Left (Unsigned_64 (High), 32) or Unsigned_64 (Low);
   end Read_Cycle;


   procedure Print_Time (Name : String; Cycles : Unsigned_64) is
      Microseconds : constant Unsigned_64 := (Cycles * 1_000_000) / Clock_Hz;
   begin
      Put_Line (Name & " cycles =" & Unsigned_64'Image (Cycles));
      Put_Line (Name & " time (us) =" & Unsigned_64'Image (Microseconds));
   end Print_Time;

end Input_Output_Helper.Time_Measurements;
