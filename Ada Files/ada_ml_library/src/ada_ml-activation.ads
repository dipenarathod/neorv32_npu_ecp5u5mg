package Ada_Ml.Activation is
   --Activation Functions
   procedure Apply_ReLU_All_Words
     (N : Natural; One_Dimensional : Boolean := False);
   procedure Apply_Sigmoid_All_Words
     (N : Natural; One_Dimensional : Boolean := False);
   procedure Apply_Softmax_All_Words
     (N : Natural; One_Dimensional : Boolean := False);

end Ada_Ml.Activation;
