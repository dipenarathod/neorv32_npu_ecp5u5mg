package Input_Output_Helper.Debug is 
   --Print a 2D tensor
   procedure Print_Tensor_Q07
     (Name : String; Data : Word_Array; Dimension : Natural);

   --Print a 1D tensor (vector)
   procedure Print_Vector_Q07
     (Name : String; Data : Word_Array; Vector_Length : Natural);

end Input_Output_Helper.Debug;