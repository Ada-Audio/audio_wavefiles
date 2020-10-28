with Ada.Numerics.Float_Random;            use Ada.Numerics.Float_Random;

with Audio.Wavefiles;                      use Audio.Wavefiles;
with Audio.Wavefiles.Data_Types;           use Audio.Wavefiles.Data_Types;
with Audio.Wavefiles.Generic_Float_PCM_IO;
with Audio.RIFF.Wav.Formats;               use Audio.RIFF.Wav.Formats;

procedure Write_Random_Noise_Wavefile is
   Wav_File_Name    : constant String          := "2ch_long_noise.wav";
   Sample_Rate_Enum : constant Wav_Sample_Rate := Sample_Rate_44100;
   Num_Channels     : constant Positive        := 2;

   WF               : Wavefile;
begin
   WF.Set_Format_Of_Wavefile (Init (Bit_Depth          => Bit_Depth_16,
                                    Sample_Rate        => Sample_Rate_Enum,
                                    Number_Of_Channels => Num_Channels,
                                    Use_Float          => False));

   WF.Create (Out_File, Wav_File_Name);

   if WF.Is_Open then

      Write_Stereo_Random_Noise : declare
         package PCM_IO is new Audio.Wavefiles.Generic_Float_PCM_IO
           (PCM_Sample    => Wav_Float_32,
            PCM_MC_Sample => Wav_Buffer_Float_32);
         use PCM_IO;

         Sample_Rate      : constant Float :=
                              Float (To_Positive (Sample_Rate_Enum));
         Duration_In_Secs : constant := 240.0;
         Last_Sample      : constant Positive
           := Positive (Sample_Rate * Duration_In_Secs);

         PCM_Buf          : Wav_Buffer_Float_32 (1 .. Num_Channels);

         Gen : Generator;
      begin
         Reset (Gen);

         for Sample in 1 .. Last_Sample loop
            for Ch_Num in PCM_Buf'Range loop
               PCM_Buf (Ch_Num) := Wav_Float_32 (Random (Gen) * 1.95 - 1.0);
            end loop;
            Put (WF, PCM_Buf);
         end loop;
      end Write_Stereo_Random_Noise;

      WF.Close;
   end if;

end Write_Random_Noise_Wavefile;
