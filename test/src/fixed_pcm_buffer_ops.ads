generic
   type PCM_Type is delta <>;
   type MC_Samples is array (Positive range <>) of PCM_Type;
package Fixed_PCM_Buffer_Ops is

   function "+" (PCM_Ref : MC_Samples;
                 PCM_DUT : MC_Samples)
                    return MC_Samples;

   function "-" (PCM_Ref : MC_Samples;
                 PCM_DUT : MC_Samples)
                    return MC_Samples;

end Fixed_PCM_Buffer_Ops;
