private generic
   type Audio_Res is range <>;
   type PCM_Type is digits <>;
   type MC_Samples is array (Positive range <>) of PCM_Type;
package Audio.Wavefiles.Gen_Float_IO is

   function Get (WF  : in out Wavefile) return MC_Samples;

   procedure Put (WF : in out Wavefile;
                  P  :        MC_Samples);

end Audio.Wavefiles.Gen_Float_IO;
