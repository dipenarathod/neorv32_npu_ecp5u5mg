with Interfaces;
with System;

package Ada_Ml is

   --Rename types for easier use
   --Search https://www.google.com/search?q=how+to+give+a+type+another+name+in+ada&oq=how+to+give+a+type+another+name+in+&gs_lcrp=EgZjaHJvbWUqBwgBECEYoAEyBggAEEUYOTIHCAEQIRigATIHCAIQIRifBTIHCAMQIRifBdIBCDc4NDNqMGo3qAIAsAIA&client=ubuntu-chr&sourceid=chrome&ie=UTF-8

   subtype Unsigned_Byte is Interfaces.Unsigned_8;
   subtype U8_T is Interfaces.Unsigned_8;



   subtype Word is Interfaces.Unsigned_32;



   subtype U32_T is Interfaces.Unsigned_32;




   --Make types which are actually arrays(https://learn.adacore.com/courses/intro-to-ada/chapters/arrays.html)
   type Unsigned_Byte_Array is array (Natural range <>) of Unsigned_Byte;
   --type Signed_Byte_Array is array (Natural range <>) of Signed_Byte;


   type Word_Array is array (Natural range <>) of Word;
   type U32_Array_T is array (Natural range <>) of U32_T;



   type Integer_Array is array (Natural range <>) of Integer;
   --Address is a type in Ada
   --Register addresses
   CTRL_Addr                                 : constant System.Address :=
     System'To_Address
       (16#90000008#); --Control register. [0]= start flag. [5:1]=opcode
   STATUS_Addr                               : constant System.Address :=
     System'To_Address
       (16#9000000C#); --Status register. [0] = busy. [1] = done
   DIM_Addr                                  : constant System.Address :=
     System'To_Address
       (16#90000010#); --Address to store dimensions(side length) of square tensor
   BASEI_Addr                                : constant System.Address :=
     System'To_Address (16#90000014#); --top-left idx in A
   OUTI_Addr                                 : constant System.Address :=
     System'To_Address (16#90000018#); --out idx in R
   WORDI_Addr                                : constant System.Address :=
     System'To_Address (16#9000001C#); --word index for operations
   SUM_Addr                                  : constant System.Address :=
     System'To_Address (16#90000020#); --Softmax sum parameter (write-only)
   SOFTMAX_MODE_Addr                         : constant System.Address :=
     System'To_Address (16#90000024#); --Softmax mode: 0=EXP, 1=DIV
   WEIGHT_BASE_INDEX_Addr                    : constant System.Address :=
     System'To_Address (16#90000028#); --Dense: weight base index in B
   BIAS_BASE_INDEX_Addr                      : constant System.Address :=
     System'To_Address (16#9000002C#); --Dense: bias index in C
   N_INPUTS_Addr                             : constant System.Address :=
     System'To_Address (16#90000030#); --Dense: number of inputs N
   SCALE_REG_Addr                            : constant System.Address :=
     System'To_Address (16#90000034#); --Scale register
   ZERO_POINT_REG_Addr                       : constant System.Address :=
     System'To_Address (16#9000003C#); --Zero point register
   QUANTIZED_MULTIPLIER_REG_Addr             : constant System.Address :=
     System'To_Address (16#90000040#); --Quantized multiplier
   QUANTIZED_MULTIPLIER_RIGHT_SHIFT_REG_Addr : constant System.Address :=
     System'To_Address
       (16#90000044#); --Quantized multiplier right shift register
   ABASE_Addr                                : constant System.Address :=
     System'To_Address (16#90001000#); --Tensor A address
   BBASE_Addr                                : constant System.Address :=
     System'To_Address (16#90004000#); --Tensor B address
   CBASE_Addr                                : constant System.Address :=
     System'To_Address (16#90007000#); --Tensor C address
   RBASE_Addr                                : constant System.Address :=
     System'To_Address (16#9000A000#); --Tensor R(result) address

   --Opcodes
   --00 and 01 are reserved for add/sub, which are not used
   OP_MAX     : constant Word := 16#02#; --Max pooling
   OP_AVG     : constant Word := 16#03#; --Average pooling
   OP_SIG     : constant Word := 16#04#; --Sigmoid activation
   OP_RELU    : constant Word := 16#05#; --ReLU activation
   OP_SOFTMAX : constant Word :=
     16#06#; --Softmax (mode flag controls EXP vs DIV)

   OP_DENSE           : constant Word := 16#07#; --Dense
   OP_NOP             : constant Word := 16#31#; --NOP
   MAX_ALLOWED_OPCODE : constant Word := 31;  --Largest opcode possible

   --Softmax mode values
   SOFTMAX_MODE_EXP : constant Word := 0; --Exponent phase
   SOFTMAX_MODE_DIV : constant Word := 1; --Division phase

   --CTRL/STATUS bit masks
   Perform_Bit  : constant Word := 1;      --CTRL[0]
   Opcode_Shift : constant Natural :=
     1;   --Bits to shift to place an opcode in CTRL[5:1] = 1
   Busy_Mask    : constant Word := 1;      --STATUS[0] = 0b1
   Done_Mask    : constant Word := 2;      --STATUS[1] = 0b10

   --Input/Output helpers
   procedure Write_Reg (Addr : System.Address; Value : Word);
   function Read_Reg (Addr : System.Address) return Word;

   function Unpack_Byte_At_Index
     (W : Word; Index : Natural) return Unsigned_Byte; --Index 0..3

   function Get_Byte_From_Tensor
     (Data : Word_Array; Index : Natural) return Unsigned_Byte;
   --Word count for a square N×N int8 tensor when 4 int8 are packed per 32-bit word
   function Tensor_Words
     (N : Natural; One_Dimensional : Boolean := False) return Natural;

   --Register setters
   procedure Set_Dim (N : Natural); --N in LSB 8 bits
   procedure Set_Pool_Base_Index (Index : Natural); --pooling index in A
   procedure Set_Out_Index (Index : Natural); --index in R
   procedure Set_Word_Index (Index : Natural);
   procedure Set_Softmax_Mode (Mode : Word);  --Set softmax mode (0=EXP, 1=DIV)
   procedure Set_Sum_Param
     (Sum : Word);      --Set sum parameter for softmax DIV phase
   procedure Set_Weight_Base_Index
     (Index :
        Natural); --Set the base index in B (for from when the weights of this layer begin)
   procedure Set_Bias_Base_Index
     (Index :
        Natural); --Set the bias index in C (for from when the weights of this layer begin)
   procedure Set_N_Inputs
     (N : Natural); --Set number of inputs for the dense layer
   procedure Set_Scale_Register
     (Scale : Natural); --Set scale for requantization
   procedure Set_Zero_Point
     (Zero_Point : Integer); --Set zero point for requantization
   procedure Set_Quantized_Multiplier_Register
     (Multiplier : Integer); --Set quantized multiplier for requantization
   procedure Set_Quantized_Multiplier_Right_Shift_Register
     (Right_Shift :
        Natural); --Set right shift for quantized multiplier for requantization
   --Operation control
   procedure Perform_Op (Opcode : Word);
   procedure Perform_Max_Pool;
   procedure Perform_Avg_Pool;
   procedure Perform_Sigmoid;
   procedure Perform_ReLU;
   procedure Perform_Softmax;
   procedure Perform_Dense;

   function Is_Busy return Boolean;
   function Is_Done return Boolean;
   procedure Wait_While_Busy;
   --procedure Wait_Until_Done;

   procedure Write_Word_In_A (Index : Natural; Value : Word);
   procedure Write_Words_In_A (Src : in Word_Array);

   function Read_Word_From_A (Index : Natural) return Word;
   procedure Read_Words_From_A (Dest : out Word_Array);

   procedure Write_Word_In_B (Index : Natural; Value : Word);
   procedure Write_Words_In_B (Src : in Word_Array);

   function Read_Word_From_B (Index : Natural) return Word;
   procedure Read_Words_From_B (Dest : out Word_Array);

   procedure Write_Word_In_C (Index : Natural; Value : Word);
   procedure Write_Words_In_C (Src : in Word_Array);

   function Read_Word_From_C (Index : Natural) return Word;
   procedure Read_Words_From_C (Dest : out Word_Array);

   function Read_Word_From_R (Index : Natural) return Word;
   procedure Read_Words_From_R (Dest : out Word_Array);

   --Activation Functions
   procedure Apply_ReLU_All_Words
     (N : Natural; One_Dimensional : Boolean := False);
   procedure Apply_Sigmoid_All_Words
     (N : Natural; One_Dimensional : Boolean := False);

   --2x2 pooling across the entire N×N tensor (stride 2, no padding)
   --Produces an (N/2)×(N/2) result into R
   procedure Apply_MaxPool_2x2_All_Words (N : Natural);
   procedure Apply_AvgPool_2x2_All_words (N : Natural);

   --Dense function
   procedure Apply_Dense_All_Words
     (Inputs                           : Natural;
      Neurons                          : Natural;
      Weight_Base_Index                : Natural;
      Bias_Base_Index                  : Natural;
      Scale                            : Natural;
      Zero_Point                       : Integer;
      Quantized_Multiplier             : Integer;
      Quantized_Multiplier_Right_Shift : Natural);


   --Q0.7 Fixed Point Conversions
   --Q0.7: 1 sign bit + 7 fractional bits
   --Range: [-1.0, 1.0) represented as 0-255
   function Float_To_Q07 (Value : Float) return Unsigned_Byte;
   function Q07_To_Float (Value : Unsigned_Byte) return Float;
   function Int_To_Q07 (Value : Integer) return Unsigned_Byte;
   function Q07_To_Int (Value : Unsigned_Byte) return Integer;

end Ada_Ml;
