package Ada_Ml.pooling is

   --2x2 pooling across the entire N×N tensor (stride 2, no padding)
   --Produces an (N/2)×(N/2) result into R
   procedure Apply_MaxPool_2x2_All_Words (N : Natural);
   procedure Apply_AvgPool_2x2_All_words (N : Natural);

end Ada_Ml.pooling;
