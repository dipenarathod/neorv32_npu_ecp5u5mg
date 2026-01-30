with System.Address_To_Access_Conversions;
with System.Storage_Elements;
with Interfaces;  use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;

package body Wb_Ov5640_Helper is

   --Set image resolution register
   procedure Set_Image_Resolution
     (Image_Width : in Natural; Image_Height : in Natural)
   is
      resolution : Word;
   begin
      resolution := Word (Image_Width);
      resolution := Shift_Left (Unsigned_32 (Image_Height), 16) or resolution;
      Write_Reg (IMAGE_RESOLUTION_Addr, resolution);
   end Set_Image_Resolution;

   --Read MASTER_WORDS_TO_READ register
   function Get_Image_Buffer_Words_To_Read return Natural is
   begin
      return Natural (Read_Reg (MASTER_WORDS_TO_READ_Addr));
   end Get_Image_Buffer_Words_To_Read;

   --Sets control register[0] to 1 to order camera to capture an image
   procedure Start_Capturing_Image is
   begin
      Write_Reg (CTRL_Addr, Word (1));
      Wait_While_Camera_Becomes_Busy;
      --Put_Line ("Camera became busy");
      Write_Reg (CTRL_Addr, Word (0));
   end Start_Capturing_Image;

   --Check status register to see if camera is busy
   function Is_Camera_Busy return Boolean is
   begin
      return
        (Read_Reg (STATUS_Addr) and Word (1))
        /= 0; --1 = 0b0001. Check if last bit is 0 or not
   end Is_Camera_Busy;

   --Check status register to see if camera is done
   function Is_Camera_Done return Boolean is
   begin
      return
        (Read_Reg (STATUS_Addr) and Word (2))
        /= 0; --1 = 0b0010. Check if the second-last bit is 0 or not
   end Is_Camera_Done;

   --Loop until camera is not busy
   procedure Wait_While_Camera_Busy is
   begin
      while Is_Camera_Busy loop
         null;
      end loop;
   end Wait_While_Camera_Busy;

   --Loop until camera is busy
   procedure Wait_While_Camera_Becomes_Busy is
   begin
      while (Is_Camera_Busy = False) loop
         null;
      end loop;
   end Wait_While_Camera_Becomes_Busy;
   --Loop until camera is done
   procedure Wait_While_Camera_Done is
   begin
      while Is_Camera_Done = False loop
         null;
      end loop;
   end Wait_While_Camera_Done;

   --Check SCCB status register to see if camera is programmed
   function Is_Camera_Programmed return Boolean is
   begin
      return
        (Read_Reg (SCCB_PROGRAM_STATUS_REG_Addr)
         = Word
             (15)); --SCCB register will have 15 if programmed correctly, and 7 or 31 otherwise
   end Is_Camera_Programmed;

   function Read_Word_From_Image_Buffer (Index : Natural) return Word is
      Addr : constant System.Address :=
        Add_Byte_Offset (IMAGE_BUFFER_BASE_Addr, Unsigned_32 (Index) * 4);
   begin
      return Read_Reg (Addr);
   end Read_Word_From_Image_Buffer;


   procedure Read_Words_From_Image_Buffer (Dest : out Word_Array) is
      J : Natural := 0;
   begin
      for I in Dest'Range loop
         Dest (I) := Read_Word_From_Image_Buffer (J);
         J := J + 1;
      end loop;
   end Read_Words_From_Image_Buffer;

end Wb_Ov5640_Helper;
