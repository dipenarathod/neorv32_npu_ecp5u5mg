pragma Warnings (Off);
pragma Ada_95;
pragma Restrictions (No_Exception_Propagation);
with System;
with System.Parameters;
with System.Secondary_Stack;
package ada_main is


   GNAT_Version : constant String :=
                    "GNAT Version: 14.2.0" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   GNAT_Version_Address : constant System.Address := GNAT_Version'Address;
   pragma Export (C, GNAT_Version_Address, "__gnat_version_address");

   Ada_Main_Program_Name : constant String := "_ada_npu_layer_worst_case_timings" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure main;
   pragma Export (C, main, "main");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.latin_1%s
   --  interfaces%s
   --  system%s
   --  ada.exceptions%s
   --  ada.exceptions%b
   --  ada.numerics%s
   --  ada.numerics.big_numbers%s
   --  ada.strings%s
   --  system.img_char%s
   --  system.img_char%b
   --  system.machine_code%s
   --  system.parameters%s
   --  system.storage_elements%s
   --  system.secondary_stack%s
   --  system.secondary_stack%b
   --  interfaces.c%s
   --  interfaces.c%b
   --  system.text_io%s
   --  system.text_io%b
   --  system.unsigned_types%s
   --  ada.text_io%s
   --  ada.text_io%b
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  ada.strings.maps%s
   --  ada.strings.maps%b
   --  ada.strings.search%s
   --  ada.strings.search%b
   --  ada.strings.fixed%s
   --  ada.strings.fixed%b
   --  system.img_int%s
   --  system.img_llu%s
   --  neorv32%s
   --  neorv32.uart0%s
   --  neorv32_hal_config%s
   --  input_output_helper%s
   --  input_output_helper%b
   --  input_output_helper.utils%s
   --  input_output_helper.utils%b
   --  riscv%s
   --  riscv.csr_generic%s
   --  riscv.csr_generic%b
   --  riscv.types%s
   --  riscv.csr%s
   --  interrupts%s
   --  interrupts%b
   --  runtime_support%s
   --  runtime_support%b
   --  uart0%s
   --  uart0%b
   --  input_output_helper.time_measurements%s
   --  input_output_helper.time_measurements%b
   --  wb_npu_address_map%s
   --  wb_npu_helper%s
   --  wb_npu_helper%b
   --  wb_npu_helper.activation%s
   --  wb_npu_helper.activation%b
   --  wb_npu_helper.conv2d%s
   --  wb_npu_helper.conv2d%b
   --  wb_npu_helper.dense%s
   --  wb_npu_helper.dense%b
   --  wb_npu_helper.pooling%s
   --  wb_npu_helper.pooling%b
   --  npu_layer_worst_case_timings%b
   --  END ELABORATION ORDER

end ada_main;
