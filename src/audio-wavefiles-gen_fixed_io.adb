with Ada.Assertions;

with Audio.Wavefiles.Fixed_Types;
with Audio.Wavefiles.Gen_PCM_IO;

package body Audio.Wavefiles.Gen_Fixed_IO is

   package Audio_Types is new Audio.Wavefiles.Fixed_Types
     (Audio_Res, PCM_Type);

   type Audio_Samples is array (Positive range <>) of Audio_Res;

   package PCM_IO is new Audio.Wavefiles.Gen_PCM_IO
     (Audio_Res     => Audio_Res,
      Audio_Samples => Audio_Samples);
   use PCM_IO;

   function Convert_Samples (B   : Audio_Samples) return MC_Samples;
   function Convert_Samples (PCM : MC_Samples)    return Audio_Samples;

   function Convert_Samples (B : Audio_Samples) return MC_Samples is
   begin
      return PCM : MC_Samples (B'Range) do
         for I in PCM'Range loop
            PCM (I) := Audio_Types.Convert_Sample (B (I));
         end loop;
      end return;
   end Convert_Samples;

   function Convert_Samples (PCM : MC_Samples) return Audio_Samples is
   begin
      return B : Audio_Samples (PCM'Range) do
         for I in B'Range loop
            B (I) := Audio_Types.Convert_Sample (PCM (I));
         end loop;
      end return;
   end Convert_Samples;

   function Get (WF  : in out Wavefile) return MC_Samples is
      B : constant Audio_Samples := Get (WF);
      P : constant MC_Samples   := Convert_Samples (B);
   begin
      return P;
   end Get;

   procedure Put (WF : in out Wavefile;
                  P  :        MC_Samples) is
      Ch : constant Positive := Positive (WF.Wave_Format.Channels);
      B  : constant Audio_Samples := Convert_Samples (P);
   begin
      Ada.Assertions.Assert (Ch = P'Length,
                             "Wrong number of channels in buffer");
      Put (WF, B);
   end Put;

end Audio.Wavefiles.Gen_Fixed_IO;
