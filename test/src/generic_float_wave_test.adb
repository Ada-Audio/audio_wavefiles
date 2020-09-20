-------------------------------------------------------------------------------
--
--                                WAVEFILES
--
--                             Test application
--
--  The MIT License (MIT)
--
--  Copyright (c) 2015 -- 2020 Gustavo A. Hoffmann
--
--  Permission is hereby granted, free of charge, to any person obtaining a
--  copy of this software and associated documentation files (the "Software"),
--  to deal in the Software without restriction, including without limitation
--  the rights to use, copy, modify, merge, publish, distribute, sublicense,
--  and / or sell copies of the Software, and to permit persons to whom the
--  Software is furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
--  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
--  DEALINGS IN THE SOFTWARE.
-------------------------------------------------------------------------------

with Ada.Text_IO;                   use Ada.Text_IO;

with Audio.Wavefiles;
with Audio.Wavefiles.Generic_Float_PCM_IO;
with Audio.RIFF;

with Generic_Float_PCM_Buffer_Ops;

package body Generic_Float_Wave_Test is

   package Wav       renames  Audio.Wavefiles;

   package PCM_IO    is new   Audio.Wavefiles.Generic_Float_PCM_IO
     (PCM_Type      => PCM_Type,
      PCM_MC_Sample => PCM_MC_Sample);
   use PCM_IO;

   package Float_PCM_Buffer_Ops is new Generic_Float_PCM_Buffer_Ops
     (PCM_Type      => PCM_Type,
      PCM_MC_Sample => PCM_MC_Sample);
   use Float_PCM_Buffer_Ops;

   Verbose     : constant Boolean := False;

   procedure Display_Info_File (File_In  : String) is
      WF_In       : Audio.Wavefiles.Wavefile;
      Wave_Format : Audio.RIFF.Wave_Format_Extensible;
   begin
      Wav.Open (WF_In, Wav.In_File, File_In, Wave_Format);
      Wav.Display_Info (WF_In);
      Wav.Close (WF_In);
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

      procedure Copy_PCM_MC_Sample;

      procedure Copy_PCM_MC_Sample is
         PCM_Buf : constant PCM_MC_Sample := Get (WF_In);
      begin
         EOF := Wav.Is_EOF (WF_In);
         Put (WF_Out, PCM_Buf);
      end Copy_PCM_MC_Sample;

   begin
      Wav.Open (WF_In,  Wav.In_File,  File_In, Wave_Format);
      Wav.Open (WF_Out, Wav.Out_File, File_Out, Wave_Format);

      if Verbose then
         Put_Line ("Input File:");
         Wav.Display_Info (WF_In);
         Put_Line ("Output File:");
         Wav.Display_Info (WF_Out);
      end if;

      loop
         Samples := Samples + 1;
         if Verbose then
            Put ("[" & Integer'Image (Samples) & "]");
         end if;

         Copy_PCM_MC_Sample;
         exit when EOF;
      end loop;
      Wav.Close (WF_In);
      Wav.Close (WF_Out);

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

      procedure Compare_PCM_MC_Sample;
      procedure Report_Comparison;

      procedure Compare_PCM_MC_Sample is
         PCM_Ref : constant PCM_MC_Sample := Get (WF_Ref);
         PCM_DUT : constant PCM_MC_Sample := Get (WF_DUT);
      begin
         EOF_Ref := Wav.Is_EOF (WF_Ref);
         EOF_DUT := Wav.Is_EOF (WF_DUT);

         if PCM_Ref /= PCM_DUT then
            Diff_Sample := Diff_Sample + 1;
         end if;
      end Compare_PCM_MC_Sample;

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
      Wav.Open (WF_Ref, Wav.In_File, File_Ref, Wave_Format);
      Wav.Open (WF_DUT, Wav.In_File, File_DUT, Wave_Format);
      loop
         Samples := Samples + 1;
         Compare_PCM_MC_Sample;
         exit when EOF_Ref or EOF_DUT;
      end loop;
      Wav.Close (WF_Ref);
      Wav.Close (WF_DUT);

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

      procedure Diff_PCM_MC_Sample;

      procedure Diff_PCM_MC_Sample is
         PCM_Ref  : constant PCM_MC_Sample := Get (WF_Ref);
         PCM_DUT  : constant PCM_MC_Sample := Get (WF_DUT);
         PCM_Diff : constant PCM_MC_Sample :=
                      PCM_Ref - PCM_DUT;
      begin
         EOF_Ref := Wav.Is_EOF (WF_Ref);
         EOF_DUT := Wav.Is_EOF (WF_DUT);

         Put (WF_Diff, PCM_Diff);
      end Diff_PCM_MC_Sample;

   begin
      Wav.Open (WF_Ref,  Wav.In_File,  File_Ref,  Wave_Format);
      Wav.Open (WF_DUT,  Wav.In_File,  File_DUT,  Wave_Format);
      Wav.Open (WF_Diff, Wav.Out_File, File_Diff, Wave_Format);
      loop
         Diff_PCM_MC_Sample;
         exit when EOF_Ref or EOF_DUT;
      end loop;
      Wav.Close (WF_Ref);
      Wav.Close (WF_DUT);
      Wav.Close (WF_Diff);
   end Diff_Files;

   procedure Mix_Files
     (File_Ref  : String;
      File_DUT  : String;
      File_Mix  : String)
   is
      WF_Ref           : Audio.Wavefiles.Wavefile;
      WF_DUT           : Audio.Wavefiles.Wavefile;
      WF_Mix           : Audio.Wavefiles.Wavefile;
      Wave_Format_Ref  : Audio.RIFF.Wave_Format_Extensible;
      Wave_Format_DUT  : Audio.RIFF.Wave_Format_Extensible;
      EOF_Ref, EOF_DUT : Boolean;

      procedure Mix_PCM_MC_Sample;

      procedure Mix_PCM_MC_Sample is
         PCM_Ref : constant PCM_MC_Sample := Get (WF_Ref);
         PCM_DUT : constant PCM_MC_Sample := Get (WF_DUT);
         PCM_Mix : constant PCM_MC_Sample :=
                     PCM_Ref + PCM_DUT;
      begin
         EOF_Ref := Wav.Is_EOF (WF_Ref);
         EOF_DUT := Wav.Is_EOF (WF_DUT);
         Put (WF_Mix, PCM_Mix);
      end Mix_PCM_MC_Sample;

   begin
      Wav.Open (WF_Ref, Wav.In_File,  File_Ref, Wave_Format_Ref);
      Wav.Open (WF_DUT, Wav.In_File,  File_DUT, Wave_Format_DUT);
      Wav.Open (WF_Mix, Wav.Out_File, File_Mix, Wave_Format_Ref);
      loop
         Mix_PCM_MC_Sample;
         exit when EOF_Ref or EOF_DUT;
      end loop;
      Wav.Close (WF_Ref);
      Wav.Close (WF_DUT);
      Wav.Close (WF_Mix);
   end Mix_Files;

end Generic_Float_Wave_Test;
