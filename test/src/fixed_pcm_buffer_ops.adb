package body Fixed_PCM_Buffer_Ops is

   function "+" (PCM_Ref : MC_Samples;
                 PCM_DUT : MC_Samples)
                    return MC_Samples
   is
      Max_Last : constant Positive :=
                   Positive'Max (PCM_Ref'Last, PCM_DUT'Last);
      PCM_Sum  :          MC_Samples (1 .. Max_Last);
   begin
      for I in 1 .. Max_Last loop
         PCM_Sum (I) := PCM_Ref (I) + PCM_DUT (I);
      end loop;
      return PCM_Sum;
   end "+";

   function "-" (PCM_Ref : MC_Samples;
                 PCM_DUT : MC_Samples)
                    return MC_Samples
   is
      Max_Last : constant Positive :=
                   Positive'Max (PCM_Ref'Last, PCM_DUT'Last);
      PCM_Diff :          MC_Samples (1 .. Max_Last);
   begin
      for I in 1 .. Max_Last loop
         PCM_Diff (I) := PCM_Ref (I) - PCM_DUT (I);
      end loop;
      return PCM_Diff;
   end "-";

end Fixed_PCM_Buffer_Ops;
