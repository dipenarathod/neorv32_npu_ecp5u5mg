with Input_Output_Helper;                   use Input_Output_Helper;
with Input_Output_Helper.Debug;             use Input_Output_Helper.Debug;
with Input_Output_Helper.Utils;             use Input_Output_Helper.Utils;
with Input_Output_Helper.Time_Measurements;
use Input_Output_Helper.Time_Measurements;
with Wb_Npu_Helper;                         use Wb_Npu_Helper;
with Wb_Npu_Address_Map;                    use Wb_Npu_Address_Map;
with Wb_Npu_Helper.Debug;                   use Wb_Npu_Helper.Debug;
with Wb_Npu_Helper.Activation;              use Wb_Npu_Helper.Activation;
with Wb_Npu_Helper.Pooling;                 use Wb_Npu_Helper.Pooling;
with Wb_Npu_Helper.Dense;                   use Wb_Npu_Helper.Dense;
with Wb_Npu_Helper.Conv2D;                  use Wb_Npu_Helper.Conv2D;
with Interfaces;                            use Interfaces;
with Ada.Text_IO;                           use Ada.Text_IO;
with Uart0;
with Runtime_Support;
with neorv32;                               use neorv32;
with RISCV.CSR;                             use RISCV.CSR;
with riscv.CSR_Generic;                     use riscv.CSR_Generic;
--with Ada.Real_Time;  use Ada.Real_Time;
with System.Machine_Code;                   use System.Machine_Code;

with tensors_mnist_28x28_words; use tensors_mnist_28x28_words;
with mnist_test_samples_28x28;  use mnist_test_samples_28x28;

procedure Mnist_Test_28x28 is

   Tensor_A_Words : Natural := Tensor_Words (Samples (0)'Length, True);
   Tensor_A       : Word_Array (0 .. Tensor_A_Words - 1);

   Predicted_Label          : Natural;
   Matches                  : Natural;
   Total_Samples            : constant Natural := Labels'Length;
   Accuracy                 : Float;
   Clock_Hz                 : constant Unsigned_64 := 72_000_000;
   Start_Cycles             : Unsigned_64;
   End_Cycles               : Unsigned_64;
   Delta_Cycles             : Unsigned_64;
   Weight_Bias_Write_Cycles : Unsigned_64;
   Total_Cycles             : Unsigned_64;

   Best_Total_Cycles  : Unsigned_64 := Unsigned_64'Last;
   Worst_Total_Cycles : Unsigned_64 := 0;
   Sum_Total_Cycles   : Unsigned_64 := 0;
   Best_Sample_Index  : Natural := 0;
   Worst_Sample_Index : Natural := 0;
   Stage_Cycles       : Unsigned_64;

   --Words each layer will produce (words that need to be copied from R to A)
   --For 3x3 Conv2D with valid padding and stride (1, 1):
   --Output side len = (Input side len - Kernel side len) / Stride + 1
   Conv_Kernel_Side_Len                 : constant Natural := 3;
   Conv_Stride                          : constant Natural := 1;
   --Layer 1 Conv
   Input_Len_First_Conv                 : constant Natural := 28;
   Output_Len_First_Conv                : constant Natural :=
     (Input_Len_First_Conv - Conv_Kernel_Side_Len) + 1;
   Number_Of_Input_Channels_First_Conv  : constant Natural := 1;
   Number_Of_Output_Channels_First_Conv : constant Natural := 14;
   Total_Bytes_Produced_First_Conv      : constant Natural :=
     (Output_Len_First_Conv ** 2) * Number_Of_Output_Channels_First_Conv;
   Total_Words_Produced_First_Conv      : constant Natural :=
     Tensor_Words (Total_Bytes_Produced_First_Conv, One_Dimensional => True);

   --Layer 2 ReLU
   --Tensor_First_Conv : Word_Array (0..Total_Words_Produced_First_Conv-1);
   --Layer 3 2D Max Pooling
   Total_Bytes_Produced_First_MaxPool : constant Natural :=
     ((Output_Len_First_Conv / 2) ** 2) * Number_Of_Output_Channels_First_Conv;
   Total_Words_Produced_First_MaxPool : constant Natural :=
     Tensor_Words
       (Total_Bytes_Produced_First_MaxPool, One_Dimensional => True);

   --Layer 4 Conv
   Input_Len_Second_Conv                 : constant Natural := 13;
   Output_Len_Second_Conv                : constant Natural :=
     (Input_Len_Second_Conv - Conv_Kernel_Side_Len) + 1;
   Number_Of_Input_Channels_Second_Conv  : constant Natural := 14;
   Number_Of_Output_Channels_Second_Conv : constant Natural := 32;
   Total_Bytes_Produced_Second_Conv      : constant Natural :=
     (Output_Len_Second_Conv ** 2) * Number_Of_Output_Channels_Second_Conv;
   Total_Words_Produced_Second_Conv      : constant Natural :=
     Tensor_Words (Total_Bytes_Produced_Second_Conv, One_Dimensional => True);

   --Layer 5 ReLU
   --Tensor_Second_Conv : Word_Array (0..Total_Words_Produced_Second_Conv-1);
   --Layer 6 2D Max Pooling
   Total_Bytes_Produced_Second_MaxPool : constant Natural :=
     ((Output_Len_Second_Conv / 2) ** 2)
     * Number_Of_Output_Channels_Second_Conv;
   Total_Words_Produced_Second_MaxPool : constant Natural :=
     Tensor_Words
       (Total_Bytes_Produced_Second_MaxPool, One_Dimensional => True);

   Tensor_Second_MaxPool :
     Word_Array (0 .. Total_Words_Produced_Second_MaxPool - 1);
   --  Tensor_Second_MaxPool :
   --   Word_Array (0 .. Total_Words_Produced_Second_MaxPool - 1) := (Others => 0);

   --Layer 7 Dense
   Inputs_First_Dense        : constant Natural :=
     ((Output_Len_Second_Conv / 2) ** 2)
     * Number_Of_Output_Channels_Second_Conv;
   Neurons_First_Dense       : constant Natural := 10;
   Neurons_First_Dense_Words : constant Natural :=
     Tensor_Words
       (Neurons_First_Dense, One_Dimensional => True); --(10+3)/4 = 3
   Result_Dense_Tensor       : Word_Array (0 .. Neurons_First_Dense_Words - 1);

   --Weights and biases offset
   Weights_Base_First_Conv       : constant Natural := 0; --0..71
   Weights_Base_Second_Conv      : constant Natural :=
     layer_0_conv2d_Weights_Words'Length; --72
   Weights_Base_Second_Conv_INT8 : constant Natural :=
     Weights_Base_Second_Conv * 4; --72
   Weights_Base_First_Dense      : constant Natural :=
     Weights_Base_Second_Conv
     + layer_5_conv2d_1_Weights_Words'Length; --72 + 4608= 4680
   Weights_Base_First_Dense_INT8 : constant Natural :=
     Weights_Base_First_Dense * 4;

   Biases_Base_First_Conv  : constant Natural := 0;
   Biases_Base_Second_Conv : constant Natural :=
     layer_0_conv2d_Bias_Words'Length;
   Biases_Base_First_Dense : constant Natural :=
     Biases_Base_Second_Conv + layer_5_conv2d_1_Bias_Words'Length;

   --Find the largest probability label in the word array
   function Largest_Probability
     (Input_Word_Array : in Word_Array; Classes : Natural) return Natural
   is
      Best_Class : Natural := 0;
      Best_Prob  : Integer := Integer'First;
   begin
      for I in 0 .. Classes - 1 loop
         declare
            Q07_Prob : Unsigned_Byte;
            Prob     : Integer;
         begin
            Q07_Prob := Get_Byte_From_Tensor (Input_Word_Array, I);
            Prob := Q07_To_Int (Q07_Prob);
            if (Prob > Best_Prob) then
               Best_Prob := Prob;
               Best_Class := I;
            end if;
         end;
      end loop;
      return Best_Class;
   end Largest_Probability;

begin
   --Tensor_Second_MaxPool (0) := 127;
   Write_Words_In_B (layer_0_conv2d_Weights_Words);
   Write_Words_In_B (layer_5_conv2d_1_Weights_Words, Weights_Base_Second_Conv);
   Write_Words_In_B (layer_12_dense_Weights_Words, Weights_Base_First_Dense);

   Write_Words_In_C (layer_0_conv2d_Bias_Words);
   Write_Words_In_C (layer_5_conv2d_1_Bias_Words, Biases_Base_Second_Conv);
   Write_Words_In_C (layer_12_dense_Bias_Words, Biases_Base_First_Dense);

   --  Put_Line
   --   ("Weights_Base_First_Conv: " & Natural'Image (Weights_Base_First_Conv));
   --  Put_Line
   --   ("Weights_Base_Second_Conv: " & Natural'Image (Weights_Base_Second_Conv));
   --  Put_Line
   --   ("Weights_Base_Second_Conv_INT8: "
   --    & Natural'Image (Weights_Base_Second_Conv_INT8));
   --  Put_Line
   --   ("Weights_Base_First_Dense: " & Natural'Image (Weights_Base_First_Dense));
   --  Put_Line
   --   ("Weights_Base_First_Dense_INT8: "
   --    & Natural'Image (Weights_Base_First_Dense_INT8));

   --  Put_Line
   --   ("Output_Len_First_Conv: "
   --    & Natural'Image (Output_Len_First_Conv));
   --  Put_Line
   --   ("Output_Len_Second_Conv: "
   --    & Natural'Image (Output_Len_Second_Conv));
   --  Put_Line
   --   ("Total_Words_Produced_First_Conv: "
   --    & Natural'Image (Total_Words_Produced_First_Conv));
   --  Put_Line
   --   ("Total_Bytes_Produced_First_Conv: "
   --    & Natural'Image (Total_Bytes_Produced_First_Conv));
   --  Put_Line
   --   ("Total_Words_Produced_First_MaxPool: "
   --    & Natural'Image (Total_Words_Produced_First_MaxPool));
   --  Put_Line
   --   ("Total_Bytes_Produced_First_MaxPool: "
   --    & Natural'Image (Total_Bytes_Produced_First_MaxPool));
   --  Put_Line
   --   ("Total_Words_Produced_Second_Conv: "
   --    & Natural'Image (Total_Words_Produced_Second_Conv));
   --  Put_Line
   --   ("Total_Bytes_Produced_Second_Conv: "
   --    & Natural'Image (Total_Bytes_Produced_Second_Conv));
   --  Put_Line
   --   ("Total_Words_Produced_Second_MaxPool: "
   --    & Natural'Image (Total_Words_Produced_Second_MaxPool));
   --  Put_Line
   --   ("Total_Bytes_Produced_Second_MaxPool: "
   --    & Natural'Image (Total_Bytes_Produced_Second_MaxPool));
   --  Put_Line
   --   ("Neurons_First_Dense_Words: " & Natural'Image (Neurons_First_Dense_Words));

   for S in Samples'Range loop
      --Try inference on all samples
      declare
         Pred : Natural;
      begin
         Put_Line ("--------------------------------");
         Put_Line
           ("Sample"
            & Natural'Image (S)
            & " expected = "
            & Integer'Image (Labels (S)));
         Total_Cycles := 0;

         Start_Cycles := Read_Cycle;
         Create_Word_Array_From_Integer_Array (Samples (S), Tensor_A);
         Write_Words_In_A (Tensor_A);
         End_Cycles := Read_Cycle;
         --  Read_Words_From_A (Tensor_A);
         --  Print_Tensor_Q07 (Name => "A:", Data => Tensor_A, Dimension => 28);
         Print_Time
           ("Time taken to write image to A after conversion = ",
            End_Cycles - Start_Cycles);
         Total_Cycles := Total_Cycles + End_Cycles - Start_Cycles;

         --Layer 1 Conv
         Start_Cycles := Read_Cycle;
         Apply_Conv2D_All_Words
           (N                                => Input_Len_First_Conv,
            Input_Channels                   =>
              Number_Of_Input_Channels_First_Conv,
            Filters                          =>
              Number_Of_Output_Channels_First_Conv,
            Weight_Base_Index                => Weights_Base_First_Conv,
            Bias_Base_Index                  => Biases_Base_First_Conv,
            Zero_Point                       => layer_0_conv2d_WZP,
            Quantized_Multiplier             =>
              layer_0_conv2d_Quantized_Multiplier,
            Quantized_Multiplier_Right_Shift =>
              layer_0_conv2d_Quantized_Right_Shift);

         End_Cycles := Read_Cycle;
         Print_Time
           ("Time take for First Conv Layer = ", End_Cycles - Start_Cycles);

         Total_Cycles := Total_Cycles + End_Cycles - Start_Cycles;

         --  Put_Line ("Going to Copy R to A");
         --  Put_Line
         --   ("Words to copy: " & Natural'Image (Total_Words_Produced_First_Conv));
         Start_Cycles := Read_Cycle;
         Copy_Result_To_Input (Total_Words_Produced_First_Conv);
         End_Cycles := Read_Cycle;
         Print_Time
           ("Time taken to copy R to A = ", End_Cycles - Start_Cycles);

         Total_Cycles := Total_Cycles + End_Cycles - Start_Cycles;

         --Layer 2 ReLU
         Start_Cycles := Read_Cycle;
         Apply_ReLU_All_Words
           (Total_Bytes_Produced_First_Conv, One_Dimensional => True);
         End_Cycles := Read_Cycle;
         Print_Time ("Time taken for ReLU = ", End_Cycles - Start_Cycles);

         Total_Cycles := Total_Cycles + End_Cycles - Start_Cycles;

         --  Put_Line ("Going to Copy R to A");
         --  Put_Line
         --   ("Words to copy: " & Natural'Image (Total_Words_Produced_First_Conv));
         Start_Cycles := Read_Cycle;
         Copy_Result_To_Input (Total_Words_Produced_First_Conv);
         End_Cycles := Read_Cycle;
         Print_Time
           ("Time taken to copy R to A = ", End_Cycles - Start_Cycles);

         --  Read_Words_From_A (Tensor_First_Conv);
         --  Print_Vector_Q07 (Name => "A:", Data => Tensor_First_Conv, Vector_Length => Total_Bytes_Produced_First_Conv);

         Total_Cycles := Total_Cycles + End_Cycles - Start_Cycles;

         --Layer 3 Max Pooling
         Start_Cycles := Read_Cycle;
         Apply_MaxPool_Multi_Channel
           (Output_Len_First_Conv, Number_Of_Output_Channels_First_Conv);
         End_Cycles := Read_Cycle;
         Print_Time
           ("Time taken to apply max pooling= ", End_Cycles - Start_Cycles);

         Total_Cycles := Total_Cycles + End_Cycles - Start_Cycles;

         --  Put_Line ("Going to Copy R to A");
         --  Put_Line
         --   ("Words to copy: "
         --    & Natural'Image (Total_Words_Produced_First_MaxPool));
         Start_Cycles := Read_Cycle;
         Copy_Result_To_Input (Total_Words_Produced_First_MaxPool);
         End_Cycles := Read_Cycle;
         Print_Time
           ("Time taken to copy R to A = ", End_Cycles - Start_Cycles);

         Total_Cycles := Total_Cycles + End_Cycles - Start_Cycles;

         --Put_Line ("About to start Conv2");

         --Layer 4 Conv
         Start_Cycles := Read_Cycle;
         Apply_Conv2D_All_Words
           (N                                => Input_Len_Second_Conv,
            Input_Channels                   =>
              Number_Of_Input_Channels_Second_Conv,
            Filters                          =>
              Number_Of_Output_Channels_Second_Conv,
            Weight_Base_Index                => Weights_Base_Second_Conv_INT8,
            Bias_Base_Index                  => Biases_Base_Second_Conv,
            Zero_Point                       => layer_5_conv2d_1_WZP,
            Quantized_Multiplier             =>
              layer_5_conv2d_1_Quantized_Multiplier,
            Quantized_Multiplier_Right_Shift =>
              layer_5_conv2d_1_Quantized_Right_Shift);

         End_Cycles := Read_Cycle;
         Print_Time
           ("Time take for Second Conv Layer = ", End_Cycles - Start_Cycles);

         Total_Cycles := Total_Cycles + End_Cycles - Start_Cycles;

         --  Put_Line ("Going to Copy R to A");
         --  Put_Line
         --   ("Words to copy: " & Natural'Image (Total_Words_Produced_Second_Conv));
         Start_Cycles := Read_Cycle;
         Copy_Result_To_Input (Total_Words_Produced_Second_Conv);
         End_Cycles := Read_Cycle;
         Print_Time
           ("Time taken to copy R to A = ", End_Cycles - Start_Cycles);
         Total_Cycles := Total_Cycles + End_Cycles - Start_Cycles;

         --Layer 5 ReLU
         Start_Cycles := Read_Cycle;
         Apply_ReLU_All_Words
           (Total_Bytes_Produced_Second_Conv, One_Dimensional => True);
         End_Cycles := Read_Cycle;
         Print_Time ("Time taken for ReLU = ", End_Cycles - Start_Cycles);

         Total_Cycles := Total_Cycles + End_Cycles - Start_Cycles;

         --  Put_Line ("Going to Copy R to A");
         --  Put_Line
         --   ("Words to copy: " & Natural'Image (Total_Words_Produced_Second_Conv));
         Start_Cycles := Read_Cycle;
         Copy_Result_To_Input (Total_Words_Produced_Second_Conv);
         End_Cycles := Read_Cycle;
         Print_Time
           ("Time taken to copy R to A = ", End_Cycles - Start_Cycles);
         Total_Cycles := Total_Cycles + End_Cycles - Start_Cycles;
         --  Read_Words_From_A (Tensor_Second_Conv);
         --  Print_Vector_Q07 (Name => "A:", Data => Tensor_Second_Conv, Vector_Length => Total_Bytes_Produced_Second_Conv);

         --Layer 6 Max Pooling
         Start_Cycles := Read_Cycle;
         Apply_MaxPool_Multi_Channel
           (Output_Len_Second_Conv, Number_Of_Output_Channels_Second_Conv);
         End_Cycles := Read_Cycle;
         Print_Time
           ("Time taken to apply max pooling= ", End_Cycles - Start_Cycles);

         --  Put_Line ("Going to Copy R to A");
         --  Put_Line
         --   ("Words to copy: "
         --    & Natural'Image (Total_Words_Produced_Second_MaxPool));
         Start_Cycles := Read_Cycle;
         Copy_Result_To_Input (Total_Words_Produced_Second_MaxPool);
         End_Cycles := Read_Cycle;
         Print_Time
           ("Time taken to copy R to A = ", End_Cycles - Start_Cycles);

         Total_Cycles := Total_Cycles + End_Cycles - Start_Cycles;

         --  Read_Words_From_A (Tensor_Second_MaxPool);
         --  Print_Vector_Q07
         --   (Name          => "A:",
         --    Data          => Tensor_Second_MaxPool,
         --    Vector_Length => Total_Bytes_Produced_Second_MaxPool);

         --Write_Words_In_A (Tensor_Second_MaxPool);

         --Layer 7 Dense
         Start_Cycles := Read_Cycle;
         Apply_Dense_All_Words
           (Inputs                           => Inputs_First_Dense,
            Neurons                          => Neurons_First_Dense,
            Weight_Base_Index                => Weights_Base_First_Dense_INT8,
            Bias_Base_Index                  => Biases_Base_First_Dense,
            Zero_Point                       => layer_12_dense_WZP,
            Quantized_Multiplier             =>
              layer_12_dense_Quantized_Multiplier,
            Quantized_Multiplier_Right_Shift =>
              Natural (layer_12_dense_Quantized_Right_Shift));

         End_Cycles := Read_Cycle;
         Print_Time
           ("Time take for Dense First Layer = ", End_Cycles - Start_Cycles);

         Total_Cycles := Total_Cycles + End_Cycles - Start_Cycles;
         --  Read_Words_From_R (Result_Dense_Tensor);
         --  Total_Cycles := Total_Cycles + End_Cycles - Start_Cycles;
         --  Print_Vector_Q07 ("Pre softmax:", Result_Dense_Tensor, 10);

         --  Put_Line ("Going to Copy R to A");
         --  Put_Line ("Words to copy: " & Natural'Image (Neurons_First_Dense_Words));
         Start_Cycles := Read_Cycle;
         Copy_Result_To_Input (Neurons_First_Dense_Words);
         End_Cycles := Read_Cycle;
         Print_Time
           ("Time taken to copy R to A = ", End_Cycles - Start_Cycles);

         --Layer 8 SoftMax
         Start_Cycles := Read_Cycle;
         Apply_SoftMax_All_Words
           (Neurons_First_Dense, One_Dimensional => True);
         End_Cycles := Read_Cycle;
         Print_Time ("Time taken for SoftMax = ", End_Cycles - Start_Cycles);

         --  Put_Line ("Going to Copy R to A");
         --  Put_Line ("Words to copy: " & Natural'Image (Neurons_First_Dense_Words));
         Start_Cycles := Read_Cycle;
         Copy_Result_To_Input (Neurons_First_Dense_Words);
         End_Cycles := Read_Cycle;
         Print_Time
           ("Time taken to copy R to A = ", End_Cycles - Start_Cycles);
         Total_Cycles := Total_Cycles + End_Cycles - Start_Cycles;

         Start_Cycles := Read_Cycle;
         Read_Words_From_R (Result_Dense_Tensor);
         End_Cycles := Read_Cycle;
         Print_Time
           ("Time taken to Read R to predict label = ",
            End_Cycles - Start_Cycles);
         Total_Cycles := Total_Cycles + End_Cycles - Start_Cycles;
         --  Print_Vector_Q07 ("Post-softmax:", Result_Dense_Tensor, 10);
         Put_Line ("Probabilities:");
         for I in 0 .. Neurons_First_Dense - 1 loop
            declare
               B : constant Unsigned_Byte :=
                 Get_Byte_From_Tensor (Result_Dense_Tensor, I);
            begin
               Put_Line
                 (Natural'Image (I) & " = " & Float'Image (Q07_To_Float (B)));
            end;
         end loop;

         Start_Cycles := Read_Cycle;
         Predicted_Label :=
           Largest_Probability (Result_Dense_Tensor, Neurons_First_Dense);
         End_Cycles := Read_Cycle;
         Print_Time
           ("Time taken to select highest probability = ",
            End_Cycles - Start_Cycles);
         Total_Cycles := Total_Cycles + End_Cycles - Start_Cycles;

         Put_Line ("Predicted Label: " & Natural'Image (Predicted_Label));
         if (Labels (S) = Predicted_Label) then
            Put_Line ("Matched");
            Matches := Matches + 1;
         else
            Put_Line ("Failed");
         end if;
         Print_Time ("Time taken this iteration = ", Total_Cycles);

         Sum_Total_Cycles := Sum_Total_Cycles + Total_Cycles;

         if Total_Cycles < Best_Total_Cycles then
            Best_Total_Cycles := Total_Cycles;
            Best_Sample_Index := S;
         end if;

         if Total_Cycles > Worst_Total_Cycles then
            Worst_Total_Cycles := Total_Cycles;
            Worst_Sample_Index := S;
         end if;

      end;
   end loop;

   Put_Line ("--------------------------------");
   Put_Line ("Total Matches = " & Natural'Image (Matches));
   Accuracy := Float (Matches) / Float (Total_Samples);
   Put_Line ("Accuracy = " & Float'Image (Accuracy));
   Put_Line ("Done");

   Put_Line ("--------------------------------");
   Put_Line ("Timing Summary Across All 28x28 Samples");
   Print_Time ("Best-case total inference time = ", Best_Total_Cycles);
   Put_Line
     ("Best-case sample index = "
      & Natural'Image (Best_Sample_Index)
      & ", label = "
      & Integer'Image (Labels (Best_Sample_Index)));
   Print_Time ("Worst-case total inference time = ", Worst_Total_Cycles);
   Put_Line
     ("Worst-case sample index = "
      & Natural'Image (Worst_Sample_Index)
      & ", label = "
      & Integer'Image (Labels (Worst_Sample_Index)));
   Print_Time
     ("Average total inference time = ",
      Sum_Total_Cycles / Unsigned_64 (Total_Samples));
   Put_Line ("Done");

   loop
      null;
   end loop;
end Mnist_Test_28x28;
