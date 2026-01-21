package Ada_Ml.Debug is

   procedure Print_Registers;

   --Q0.7 Fixed Point Conversions
   --Q0.7: 1 sign bit + 7 fractional bits
   --Range: [-1.0, 1.0) represented as 0-255

   --Print a 2D tensor
   procedure Print_Tensor_Q07
     (Name : String; Data : Word_Array; Dimension : Natural);

   --Print a 1D tensor (vector)
   procedure Print_Vector_Q07
     (Name : String; Data : Word_Array; Vector_Length : Natural);

end Ada_Ml.Debug;