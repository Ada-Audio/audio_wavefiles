with Ada.Text_IO;                   use Ada.Text_IO;

with Audio.Wavefiles;
with Audio.Wavefiles.Read;
with Audio.Wavefiles.Write;
with Audio.Fixed_PCM_Buffers;
--  with Audio.Float_PCM_Buffers;
with Audio.RIFF;

package body Wave_Test is

   Fixed_Depth : constant Positive := 32;

   pragma Warnings (Off, "declared high bound of type");

   type Fixed_Long is delta 1.0 / 2.0 ** (Fixed_Depth - 1) range -1.0 .. 1.0
     with Size => Fixed_Depth;

   pragma Warnings (On, "declared high bound of type");

   Channels    : constant Positive := 6;
   Samples     : constant Positive := 2048;

   package PCM is new Audio.Fixed_PCM_Buffers
     (Samples  => Samples,
      PCM_Type => Fixed_Long);

   package Wav_Read  renames  Audio.Wavefiles.Read;
   package Wav_Write renames  Audio.Wavefiles.Write;

   Verbose     : constant Boolean := False;


   procedure Display_Info_File (File_In  : String) is
      WF_In       : Audio.Wavefiles.Wavefile;
   begin
      Wav_Read.Open (WF_In, File_In);
      Wav_Read.Display_Info (WF_In);
      Wav_Read.Close (WF_In);
   end Display_Info_File;

   procedure Copy_File
     (File_In  : String;
      File_Out : String)
   is
      WF_In       : Audio.Wavefiles.Wavefile;
      WF_Out      : Audio.Wavefiles.Wavefile;
      PCM_Buf     : PCM.PCM_Buffer (Channels);
      Wave_Format : Audio.RIFF.Wave_Format_Extensible;
      EOF         : Boolean;
      Frame       : Integer := 0;
   begin
      Wav_Read.Open (WF_In, File_In);

      Wave_Format := Audio.Wavefiles.Get_Wave_Format (WF_In);

      Wav_Write.Open (WF_Out, File_Out, Wave_Format);

      if Verbose then
         Put_Line ("Input File:");
         Wav_Read.Display_Info (WF_In);
         Put_Line ("Output File:");
         Wav_Read.Display_Info (WF_Out);
      end if;

      loop
         Frame := Frame + 1;
         if Verbose then
            Put ("[" & Integer'Image (Frame) & "]");
         end if;
         PCM.Get (WF_In, PCM_Buf, EOF);
         PCM.Put (WF_Out, PCM_Buf);
         exit when EOF;
      end loop;
      Wav_Read.Close (WF_In);
      Wav_Write.Close (WF_Out);

   end Copy_File;

   procedure Compare_Files
     (File_Ref  : String;
      File_DUT  : String)
   is
      WF_Ref           : Audio.Wavefiles.Wavefile;
      WF_DUT           : Audio.Wavefiles.Wavefile;
      PCM_Ref          : PCM.PCM_Buffer (Channels);
      PCM_DUT          : PCM.PCM_Buffer (Channels);
      Wave_Format      : Audio.RIFF.Wave_Format_Extensible;
      EOF_Ref, EOF_DUT : Boolean;
      Diff_Frames      : Natural := 0;
      Frame            : Integer := 0;
   begin
      Wave_Format.Set_Default;

      Wav_Read.Open (WF_Ref, File_Ref);
      Wav_Read.Open (WF_DUT, File_DUT);
      loop
         Frame := Frame + 1;
         PCM.Get (WF_Ref, PCM_Ref, EOF_Ref);
         PCM.Get (WF_DUT, PCM_DUT, EOF_DUT);
         if not PCM."=" (PCM_Ref, PCM_DUT) then
            Diff_Frames := Diff_Frames + 1;
            Put_Line ("Difference found at frame " & Integer'Image (Frame));
         end if;
         exit when EOF_Ref or EOF_DUT;
      end loop;
      Wav_Read.Close (WF_Ref);
      Wav_Read.Close (WF_DUT);
      if Diff_Frames > 0 then
         Put_Line ("Differences have been found in "
                   & Natural'Image (Diff_Frames)
                   & " frames");
      else
         Put_Line ("No differences have been found");
      end if;
   end Compare_Files;

   procedure Diff_Files
     (File_Ref  : String;
      File_DUT  : String;
      File_Diff : String)
   is
      use type PCM.PCM_Buffer;

      WF_Ref           : Audio.Wavefiles.Wavefile;
      WF_DUT           : Audio.Wavefiles.Wavefile;
      WF_Diff          : Audio.Wavefiles.Wavefile;
      PCM_Ref          : PCM.PCM_Buffer (Channels);
      PCM_DUT          : PCM.PCM_Buffer (Channels);
      PCM_Diff         : PCM.PCM_Buffer (Channels);
      Wave_Format      : Audio.RIFF.Wave_Format_Extensible;
      EOF_Ref, EOF_DUT : Boolean;
   begin
      Audio.RIFF.Set_Default (Wave_Format);

      Wav_Read.Open (WF_Ref, File_Ref);
      Wav_Read.Open (WF_DUT, File_DUT);
      Wav_Write.Open (WF_Diff, File_Diff, Wave_Format);
      loop
         PCM.Get (WF_Ref, PCM_Ref, EOF_Ref);
         PCM.Get (WF_DUT, PCM_DUT, EOF_DUT);
         PCM_Diff := PCM_Ref - PCM_DUT;
         PCM.Put (WF_Diff, PCM_Diff);
         exit when EOF_Ref or EOF_DUT;
      end loop;
      Wav_Read.Close (WF_Ref);
      Wav_Read.Close (WF_DUT);
      Wav_Write.Close (WF_Diff);
   end Diff_Files;

   procedure Mix_Files
     (File_Ref  : String;
      File_DUT  : String;
      File_Mix  : String)
   is
      use type PCM.PCM_Buffer;

      WF_Ref           : Audio.Wavefiles.Wavefile;
      WF_DUT           : Audio.Wavefiles.Wavefile;
      WF_Mix           : Audio.Wavefiles.Wavefile;
      PCM_Ref          : PCM.PCM_Buffer (Channels);
      PCM_DUT          : PCM.PCM_Buffer (Channels);
      PCM_Mix          : PCM.PCM_Buffer (Channels);
      Wave_Format      : Audio.RIFF.Wave_Format_Extensible;
      EOF_Ref, EOF_DUT : Boolean;
   begin
      Audio.RIFF.Set_Default (Wave_Format);

      Wav_Read.Open (WF_Ref, File_Ref);
      Wav_Read.Open (WF_DUT, File_DUT);
      Wav_Write.Open (WF_Mix, File_Mix, Wave_Format);
      loop
         PCM.Get (WF_Ref, PCM_Ref, EOF_Ref);
         PCM.Get (WF_DUT, PCM_DUT, EOF_DUT);
         PCM_Mix := PCM_Ref + PCM_DUT;
         PCM.Put (WF_Mix, PCM_Mix);
         exit when EOF_Ref or EOF_DUT;
      end loop;
      Wav_Read.Close (WF_Ref);
      Wav_Read.Close (WF_DUT);
      Wav_Write.Close (WF_Mix);
   end Mix_Files;

end Wave_Test;
