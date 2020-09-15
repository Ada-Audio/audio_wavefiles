with Ada.Text_IO;                   use Ada.Text_IO;

with Audio.Wavefiles;
with Audio.Wavefiles.Read;
with Audio.Wavefiles.Write;
with Audio.RIFF;

with Fixed_PCM_Buffer_Ops;

package body Wave_Test is

   pragma Warnings (Off, "declared high bound of type");

   type Fixed_32_PCM is delta 1.0 / 2.0 ** (32 - 1) range -1.0 .. 1.0
     with Size => 32;

   pragma Warnings (On, "declared high bound of type");

   type Fixed_32_PCM_Buffer is array (Positive range <>) of Fixed_32_PCM;

   package Wav_Read  renames  Audio.Wavefiles.Read;
   package Wav_Write renames  Audio.Wavefiles.Write;

   function Get is new Wav_Read.Get_Fixed
     (PCM_Type   => Fixed_32_PCM,
      MC_Samples => Fixed_32_PCM_Buffer);

   procedure Put is new Wav_Write.Put_Fixed
     (PCM_Type   => Fixed_32_PCM,
      MC_Samples => Fixed_32_PCM_Buffer);

   package Fixed_32_PCM_Buffer_Ops is new Fixed_PCM_Buffer_Ops
     (PCM_Type   => Fixed_32_PCM,
      MC_Samples => Fixed_32_PCM_Buffer);
   use Fixed_32_PCM_Buffer_Ops;

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
      Wave_Format : Audio.RIFF.Wave_Format_Extensible;
      EOF         : Boolean;
      Samples     : Integer := 0;

      procedure Copy_MC_Sample;

      procedure Copy_MC_Sample is
         PCM_Buf : constant Fixed_32_PCM_Buffer := Get (WF_In);
      begin
         EOF := Wav_Read.Is_EOF (WF_In);
         Put (WF_Out, PCM_Buf);
      end Copy_MC_Sample;

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
         Samples := Samples + 1;
         if Verbose then
            Put ("[" & Integer'Image (Samples) & "]");
         end if;

         Copy_MC_Sample;
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
      Wave_Format      : Audio.RIFF.Wave_Format_Extensible;
      EOF_Ref, EOF_DUT : Boolean;
      Diff_Sample      : Natural := 0;
      Samples          : Integer := 0;

      procedure Compare_MC_Sample;
      procedure Report_Comparison;

      procedure Compare_MC_Sample is
         PCM_Ref : constant Fixed_32_PCM_Buffer := Get (WF_Ref);
         PCM_DUT : constant Fixed_32_PCM_Buffer := Get (WF_DUT);
      begin
         EOF_Ref := Wav_Read.Is_EOF (WF_Ref);
         EOF_DUT := Wav_Read.Is_EOF (WF_DUT);

         if PCM_Ref /= PCM_DUT then
            Diff_Sample := Diff_Sample + 1;
         end if;
      end Compare_MC_Sample;

      procedure Report_Comparison is
      begin
         Put_Line ("Compared " & Samples'Image & " samples");
         if Diff_Sample > 0 then
            Put_Line ("Differences have been found in "
                      & Natural'Image (Diff_Sample)
                      & " samples");
         else
            Put_Line ("No differences have been found");
         end if;
      end Report_Comparison;

   begin
      Wave_Format.Set_Default;

      Wav_Read.Open (WF_Ref, File_Ref);
      Wav_Read.Open (WF_DUT, File_DUT);
      loop
         Samples := Samples + 1;
         Compare_MC_Sample;
         exit when EOF_Ref or EOF_DUT;
      end loop;
      Wav_Read.Close (WF_Ref);
      Wav_Read.Close (WF_DUT);

      Report_Comparison;
   end Compare_Files;

   procedure Diff_Files
     (File_Ref  : String;
      File_DUT  : String;
      File_Diff : String)
   is
      WF_Ref           : Audio.Wavefiles.Wavefile;
      WF_DUT           : Audio.Wavefiles.Wavefile;
      WF_Diff          : Audio.Wavefiles.Wavefile;
      Wave_Format      : Audio.RIFF.Wave_Format_Extensible;
      EOF_Ref, EOF_DUT : Boolean;

      procedure Diff_MC_Sample;

      procedure Diff_MC_Sample is
         PCM_Ref  : constant Fixed_32_PCM_Buffer := Get (WF_Ref);
         PCM_DUT  : constant Fixed_32_PCM_Buffer := Get (WF_DUT);
         PCM_Diff : constant Fixed_32_PCM_Buffer :=
                      PCM_Ref - PCM_DUT;
      begin
         EOF_Ref := Wav_Read.Is_EOF (WF_Ref);
         EOF_DUT := Wav_Read.Is_EOF (WF_DUT);

         Put (WF_Diff, PCM_Diff);
      end Diff_MC_Sample;

   begin
      Audio.RIFF.Set_Default (Wave_Format);

      Wav_Read.Open (WF_Ref, File_Ref);
      Wav_Read.Open (WF_DUT, File_DUT);
      Wav_Write.Open (WF_Diff, File_Diff, Wave_Format);
      loop
         Diff_MC_Sample;
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
      WF_Ref           : Audio.Wavefiles.Wavefile;
      WF_DUT           : Audio.Wavefiles.Wavefile;
      WF_Mix           : Audio.Wavefiles.Wavefile;
      Wave_Format      : Audio.RIFF.Wave_Format_Extensible;
      EOF_Ref, EOF_DUT : Boolean;

      procedure Mix_MC_Sample;

      procedure Mix_MC_Sample is
         PCM_Ref : constant Fixed_32_PCM_Buffer := Get (WF_Ref);
         PCM_DUT : constant Fixed_32_PCM_Buffer := Get (WF_DUT);
         PCM_Mix : constant Fixed_32_PCM_Buffer :=
                     PCM_Ref + PCM_DUT;
      begin
         EOF_Ref := Wav_Read.Is_EOF (WF_Ref);
         EOF_DUT := Wav_Read.Is_EOF (WF_DUT);
         Put (WF_Mix, PCM_Mix);
      end Mix_MC_Sample;

   begin
      Audio.RIFF.Set_Default (Wave_Format);

      Wav_Read.Open (WF_Ref, File_Ref);
      Wav_Read.Open (WF_DUT, File_DUT);
      Wav_Write.Open (WF_Mix, File_Mix, Wave_Format);
      loop
         Mix_MC_Sample;
         exit when EOF_Ref or EOF_DUT;
      end loop;
      Wav_Read.Close (WF_Ref);
      Wav_Read.Close (WF_DUT);
      Wav_Write.Close (WF_Mix);
   end Mix_Files;

end Wave_Test;
