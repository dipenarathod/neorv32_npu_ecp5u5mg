with Input_Output_Helper; use Input_Output_Helper;
with Input_Output_Helper.Debug; use Input_Output_Helper.Debug;
with Input_Output_Helper.Utils; use Input_Output_Helper.Utils;
with Input_Output_Helper.Time_Measurements; use Input_Output_Helper.Time_Measurements;

with Interfaces;          use Interfaces;
with Ada.Text_IO;         use Ada.Text_IO;
with Uart0;
with Runtime_Support;
with Wb_Ov5640_Helper;    use Wb_Ov5640_Helper;

procedure Wb_Ov5640_Tests is

   Image_Words : Natural := Tensor_Words (50, False);
   Captured_Image : Word_Array (0..Image_Words - 1);

   --Test pass or fail result print
   procedure Print_Result (Name : String; Passed : Boolean) is
   begin
      if Passed then
         Put_Line (Name & " PASS");
      else
         Put_Line (Name & " FAIL");
      end if;
   end Print_Result;

   procedure Test_Camera_SCCB_Status is
      Result : Boolean;
   begin
      --  for Count in 1..10 loop
      --     Put_Line (Word'Image(Read_Reg(SCCB_PROGRAM_STATUS_REG_Addr)));
      --  end loop;
      Result := Is_Camera_Programmed;
      Print_Result ("Camera Programmed Test: ", Result);
   end Test_Camera_SCCB_Status;

   procedure Start_And_Read_Captured_Image is
      Result : Boolean;
   begin
      Put_Line ("Starting test 2");
      Start_Capturing_Image;
      --Wait_While_Camera_Becomes_Busy;
      --Put_Line ("Camera became busy");
      Wait_While_Camera_Done;
      Read_Words_From_Image_Buffer (Captured_Image);
      Print_Tensor_Q07 ("Image", Captured_Image, 50);
   end;
begin
   Uart0.Init (19200);
   Put_Line ("Reunning Test Cases----------------");
   Test_Camera_SCCB_Status;
   Start_And_Read_Captured_Image;
   loop
      null;
   end loop;

end Wb_Ov5640_Tests;
