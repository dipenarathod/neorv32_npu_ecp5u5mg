package Ada_Ml.Processing is

   procedure Apply_Softmax_All_Words
     (N : Natural; One_Dimensional : Boolean := False);

   procedure Create_Word_Array_From_Integer_Array
     (Integer_Source : in Integer_Array; Result_Word_Array : out Word_Array);

end Ada_Ml.Processing;