pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__npu_layer_worst_case_timings.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__npu_layer_worst_case_timings.adb");
pragma Suppress (Overflow_Check);

package body ada_main is

   E23 : Short_Integer; pragma Import (Ada, E23, "ada__text_io_E");
   E10 : Short_Integer; pragma Import (Ada, E10, "ada__strings__maps_E");
   E27 : Short_Integer; pragma Import (Ada, E27, "input_output_helper_E");
   E50 : Short_Integer; pragma Import (Ada, E50, "input_output_helper__utils_E");
   E39 : Short_Integer; pragma Import (Ada, E39, "riscv__csr_generic_E");
   E54 : Short_Integer; pragma Import (Ada, E54, "interrupts_E");
   E52 : Short_Integer; pragma Import (Ada, E52, "runtime_support_E");
   E42 : Short_Integer; pragma Import (Ada, E42, "uart0_E");
   E30 : Short_Integer; pragma Import (Ada, E30, "input_output_helper__time_measurements_E");
   E57 : Short_Integer; pragma Import (Ada, E57, "wb_npu_helper_E");
   E60 : Short_Integer; pragma Import (Ada, E60, "wb_npu_helper__activation_E");
   E62 : Short_Integer; pragma Import (Ada, E62, "wb_npu_helper__conv2d_E");
   E64 : Short_Integer; pragma Import (Ada, E64, "wb_npu_helper__dense_E");
   E66 : Short_Integer; pragma Import (Ada, E66, "wb_npu_helper__pooling_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);


   procedure adainit is
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");

      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      null;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;


      Ada.Text_Io'Elab_Body;
      E23 := E23 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E10 := E10 + 1;
      E27 := E27 + 1;
      E50 := E50 + 1;
      E39 := E39 + 1;
      Interrupts'Elab_Body;
      E54 := E54 + 1;
      E52 := E52 + 1;
      E42 := E42 + 1;
      E30 := E30 + 1;
      E57 := E57 + 1;
      E60 := E60 + 1;
      E62 := E62 + 1;
      E64 := E64 + 1;
      E66 := E66 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_npu_layer_worst_case_timings");

   procedure main is
      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      adainit;
      Ada_Main_Program;
   end;

--  BEGIN Object file/option list
   --   C:\Users\redra\OneDrive\Desktop\repos\neorv32_basys3_scripts_and_steps\Ada-Files\NPU-Tests\npu_layer_worst_case_timings\obj\development\runtime_support.o
   --   C:\Users\redra\OneDrive\Desktop\repos\neorv32_basys3_scripts_and_steps\Ada-Files\NPU-Tests\npu_layer_worst_case_timings\obj\development\npu_layer_worst_case_timings.o
   --   -LC:\Users\redra\OneDrive\Desktop\repos\neorv32_basys3_scripts_and_steps\Ada-Files\NPU-Tests\npu_layer_worst_case_timings\obj\development\
   --   -LC:\Users\redra\OneDrive\Desktop\repos\neorv32_basys3_scripts_and_steps\Ada-Files\NPU-Tests\npu_layer_worst_case_timings\obj\development\
   --   -LC:\Users\redra\AppData\Local\alire\cache\builds\bare_runtime_14.0.0_095db6f0\282b01b920f0d5bb2bac604ac6d9e811f26d175144bc99af963e0381e797ee94\adalib\
   --   -LC:\Users\redra\OneDrive\Desktop\repos\neorv32_basys3_scripts_and_steps\Ada-Files\NPU-Tests\input_output_helper\lib\
   --   -LC:\Users\redra\OneDrive\Desktop\repos\neorv32_basys3_scripts_and_steps\Ada-Files\NPU-Tests\npu_layer_worst_case_timings\alire\cache\pins\neorv32_hal\lib\
   --   -LC:\Users\redra\OneDrive\Desktop\repos\neorv32_basys3_scripts_and_steps\Ada-Files\NPU-Library\wb_npu_helper\lib\
   --   -static
   --   -lgnat
--  END Object file/option list   

end ada_main;
