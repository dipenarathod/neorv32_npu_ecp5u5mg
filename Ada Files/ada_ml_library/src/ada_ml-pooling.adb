package body Ada_Ml.pooling is

   --2x2 max pooling over entire tensor
   --Produces (N/2) x (N/2) outputs in R
   procedure Apply_MaxPool_2x2_All_Words (N : Natural) is
      Out_N     : constant Natural := N / 2;  --floor division for odd N
      Base      : Natural;
      Out_Index : Natural;
   begin
      Set_Dim (N);   --Value in DIM is required by the VHDL
      for r in 0 .. Out_N - 1 loop
         for c in 0 .. Out_N - 1 loop
            Base := (2 * r) * N + (2 * c);     --top-left of 2x2 window in A
            -- '*2' because stride = 2
            -- '*N' to make it a flat index.
            Out_Index := r * Out_N + c;        --flat index into R
            -- '*Out_N' to make it a flat index
            Set_Pool_Base_Index (Base);
            Set_Out_Index (Out_Index);
            Perform_Max_Pool;
            Wait_While_Busy;
            Write_Reg (CTRL_Addr, 0); --De-assert start
         end loop;
      end loop;
   end Apply_MaxPool_2x2_All_Words;

   --2x2 average pooling over entire tensor
   --Produces (N/2) x (N/2) outputs in R
   procedure Apply_AvgPool_2x2_All_Words (N : Natural) is
      Out_N     : constant Natural := N / 2;  --floor division for odd N
      Base      : Natural;
      Out_Index : Natural;
   begin
      Set_Dim (N);
      for r in 0 .. Out_N - 1 loop
         for c in 0 .. Out_N - 1 loop
            Base := (2 * r) * N + (2 * c);     --top-left of 2x2 window in A
            -- '*2' because stride = 2
            -- '*N' to make it a flat index.
            Out_Index := r * Out_N + c;        --flat index into R
            -- '*Out_N' to make it a flat index
            Set_Pool_Base_Index (Base);
            Set_Out_Index (Out_Index);
            Perform_Avg_Pool;
            Wait_While_Busy;
            Write_Reg (CTRL_Addr, 0); --De-assert start
         end loop;
      end loop;
   end Apply_AvgPool_2x2_All_Words;

end Ada_Ml.pooling;
