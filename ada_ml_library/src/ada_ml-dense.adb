package body Ada_Ml.Dense is

   --Dense function
   --Computes Neurons outputs (1 output per NPU command) and stores them in R
   procedure Apply_Dense_All_Words
     (Inputs                           : Natural;
      Neurons                          : Natural;
      Weight_Base_Index                : Natural;
      Bias_Base_Index                  : Natural;
      Scale                            : Natural;
      Zero_Point                       : Integer;
      Quantized_Multiplier             : Integer;
      Quantized_Multiplier_Right_Shift : Natural)
   is
      Input_Base_Index : constant Natural := 0;

      W_Base  : Natural;
      B_Index : Natural;
   begin
      Set_N_Inputs (Inputs); --Set number of inputs to dense layer
      Set_Scale_Register (Scale);
      Set_Zero_Point (Zero_Point);
      Set_Quantized_Multiplier_Register (Quantized_Multiplier);
      Set_Quantized_Multiplier_Right_Shift_Register
        (Quantized_Multiplier_Right_Shift);
      --Put_Line ("Wrote N register. Starting loop");
      --One NPU dense command computes exactly one neuron output and writes one element to R
      for N in 0 .. Neurons - 1 loop
         --Per-neuron addressing
         --The FSM loops to multiply all inputs of a neuron with the connection weights internally
         W_Base :=
           Weight_Base_Index
           + (N * Inputs); --Multiple input weights for a neuron
         B_Index := Bias_Base_Index + (N); --One bias for a neuron.

         Set_Word_Index (Input_Base_Index); --A input index
         Set_Weight_Base_Index (W_Base);    --B weight base index
         Set_Bias_Base_Index (B_Index);       --C bias index
         Set_Out_Index (N);                 --R output index
         --Put_Line ("Set word index, weight base index, bias base index, and out index. Performing dense");
         Perform_Dense;
         --Put_Line("Started dense. Waiting now");
         Wait_While_Busy;
         --Put_Line ("Done waiting");
         Write_Reg
           (CTRL_Addr, 0); --De-assert start so next command can trigger
      end loop;
   end Apply_Dense_All_Words;

end Ada_Ml.Dense;
