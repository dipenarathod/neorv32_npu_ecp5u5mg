with Interfaces;
with System;
with input_output_helper; use input_output_helper;

package Wb_Ov5640_Helper is

   --Register addresses

   CTRL_Addr                    : constant System.Address :=
     System'To_Address
       (16#90010000#); --Camera Control register. [0] = 1 from master, capture image
   STATUS_Addr                  : constant System.Address :=
     System'To_Address
       (16#90010004#); --Camera status register. [0] = 1 = busy, [1] = 1 = done
   IMAGE_FORMAT_Addr            : constant System.Address :=
     System'To_Address (16#90010008#); --Image Format address (not used)
   IMAGE_RESOLUTION_Addr        : constant System.Address :=
     System'To_Address
       (16#9001000C#); --[15:0] = Image width [31:16] = Image height
   MASTER_WORDS_TO_READ_Addr    : constant System.Address :=
     System'To_Address
       (16#90010010#); --32-bit words the master has to read to gather the complete image (most likely not used)
   SCCB_PROGRAM_STATUS_REG_Addr : constant System.Address :=
     System'To_Address
       (16#90010014#); --Register to show SCCB programmer status. [0] = start latched.
   --[1] = program started. [2] = wrapper busy. [3] = done. [4] = error
   IMAGE_BUFFER_BASE_Addr       : constant System.Address :=
     System'To_Address (16#90011000#); --Image Buffer Base Address

   procedure Set_Image_Resolution
     (Image_Width  : in Natural;
      Image_Height : in Natural); --Set image resolution register

   function Get_Image_Buffer_Words_To_Read
      return Natural; --Read MASTER_WORDS_TO_READ register

   procedure Start_Capturing_Image; --Sets control register[0] to 1 to order camera to capture an image

   function Is_Camera_Busy
      return Boolean;   --Check status register to see if camera is busy
   function Is_Camera_Done
      return Boolean;   --Check status register to see if camera is done
   procedure Wait_While_Camera_Busy;         --Loop until camera is not busy
   procedure Wait_While_Camera_Becomes_Busy; --Loop until camera is busy
   procedure Wait_While_Camera_Done;         --Loop until camera is done
   function Is_Camera_Programmed
      return Boolean; --Check SCCB status register to see if camera is programmed

   function Read_Word_From_Image_Buffer (Index : Natural) return Word;
   procedure Read_Words_From_Image_Buffer (Dest : out Word_Array);

end Wb_Ov5640_Helper;
