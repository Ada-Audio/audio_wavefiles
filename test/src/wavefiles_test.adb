-------------------------------------------------------------------------------
--
--                                WAVEFILES
--
--                             Test application
--
-- The MIT License (MIT)
--
-- Copyright (c) 2015 Gustavo A. Hoffmann
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to
-- deal in the Software without restriction, including without limitation the
-- rights to use, copy, modify, merge, publish, distribute, sublicense, and /
-- or sell copies of the Software, and to permit persons to whom the Software
-- is furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
-- IN THE SOFTWARE.
-------------------------------------------------------------------------------

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with Wavefiles;
with Wavefiles.Read;
with Wavefiles.Write;
with Wavefiles.PCM_Buffers.IO;
with Wavefiles.PCM_Buffers.Operators;
with RIFF;

procedure Wavefiles_Test is
   Command_Line_OK : Boolean := False;

   package Wave_Test is
      procedure Display_Info_File
        (File_In : String);

      procedure Copy_File
        (File_In         : String;
         File_Out        : String);

      procedure Compare_Files
        (File_Ref    : String;
         File_DUT    : String);

      procedure Diff_Files
        (File_Ref       : String;
         File_DUT       : String;
         File_Diff      : String);

      procedure Mix_Files
        (File_Ref        : String;
         File_DUT        : String;
         File_Mix        : String);
   end Wave_Test;

   procedure Print_Usage;

   --  Nested package & procedures

   package body Wave_Test is

      Fixed_Depth : constant Positive := 32;

      pragma Warnings (Off, "declared high bound of type");

      type Fixed_Long is delta 1.0 / 2.0 ** (Fixed_Depth - 1) range -1.0 .. 1.0
        with Size => Fixed_Depth;

      pragma Warnings (On, "declared high bound of type");

      Channels    : constant Positive := 6;
      Samples     : constant Positive := 2048;

      package PCM_Buf is new Wavefiles.PCM_Buffers (Samples, Fixed_Long);
      package PCM_IO  is new PCM_Buf.IO;
      package PCM_Op  is new PCM_Buf.Operators;

      package Wav_Read  renames  Wavefiles.Read;
      package Wav_Write renames  Wavefiles.Write;

      Verbose     : constant Boolean := False;


      procedure Display_Info_File (File_In  : String) is
         WF_In       : Wavefiles.Wavefile;
      begin
         Wav_Read.Open (WF_In, File_In);
         Wav_Read.Display_Info (WF_In);
         Wav_Read.Close (WF_In);
      end Display_Info_File;

      procedure Copy_File (File_In  : String;
                           File_Out : String) is
         WF_In       : Wavefiles.Wavefile;
         WF_Out      : Wavefiles.Wavefile;
         PCM         : PCM_Buf.PCM_Buffer (Channels);
         Wave_Format : RIFF.Wave_Format_Extensible;
         EOF         : Boolean;
         Frame       : Integer := 0;
      begin
         Wav_Read.Open (WF_In, File_In);

         Wave_Format := Wavefiles.Get_Wave_Format (WF_In);

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
            PCM_IO.Read (WF_In, PCM, EOF);
            PCM_IO.Write (WF_Out, PCM);
            exit when EOF;
         end loop;
         Wav_Read.Close (WF_In);
         Wav_Write.Close (WF_Out);

      end Copy_File;

      procedure Compare_Files (File_Ref  : String;
                               File_DUT  : String) is
         WF_Ref           : Wavefiles.Wavefile;
         WF_DUT           : Wavefiles.Wavefile;
         PCM_Ref          : PCM_Buf.PCM_Buffer (Channels);
         PCM_DUT          : PCM_Buf.PCM_Buffer (Channels);
         Wave_Format      : RIFF.Wave_Format_Extensible;
         EOF_Ref, EOF_DUT : Boolean;
         Diff_Frames      : Natural := 0;
         Frame            : Integer := 0;
      begin
         Wave_Format.Set_Default;

         Wav_Read.Open (WF_Ref, File_Ref);
         Wav_Read.Open (WF_DUT, File_DUT);
         loop
            Frame := Frame + 1;
            PCM_IO.Read (WF_Ref, PCM_Ref, EOF_Ref);
            PCM_IO.Read (WF_DUT, PCM_DUT, EOF_DUT);
            if not PCM_Buf."=" (PCM_Ref, PCM_DUT) then
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

      procedure Diff_Files (File_Ref  : String;
                            File_DUT  : String;
                            File_Diff : String) is
         WF_Ref           : Wavefiles.Wavefile;
         WF_DUT           : Wavefiles.Wavefile;
         WF_Diff          : Wavefiles.Wavefile;
         PCM_Ref          : PCM_Buf.PCM_Buffer (Channels);
         PCM_DUT          : PCM_Buf.PCM_Buffer (Channels);
         PCM_Diff         : PCM_Buf.PCM_Buffer (Channels);
         Wave_Format      : RIFF.Wave_Format_Extensible;
         EOF_Ref, EOF_DUT : Boolean;
      begin
         RIFF.Set_Default (Wave_Format);

         Wav_Read.Open (WF_Ref, File_Ref);
         Wav_Read.Open (WF_DUT, File_DUT);
         Wav_Write.Open (WF_Diff, File_Diff, Wave_Format);
         loop
            PCM_IO.Read (WF_Ref, PCM_Ref, EOF_Ref);
            PCM_IO.Read (WF_DUT, PCM_DUT, EOF_DUT);
            PCM_Diff := PCM_Op."-" (PCM_Ref, PCM_DUT);
            PCM_IO.Write (WF_Diff, PCM_Diff);
            exit when EOF_Ref or EOF_DUT;
         end loop;
         Wav_Read.Close (WF_Ref);
         Wav_Read.Close (WF_DUT);
         Wav_Write.Close (WF_Diff);
      end Diff_Files;

      procedure Mix_Files (File_Ref  : String;
                           File_DUT  : String;
                           File_Mix  : String) is
         WF_Ref           : Wavefiles.Wavefile;
         WF_DUT           : Wavefiles.Wavefile;
         WF_Mix           : Wavefiles.Wavefile;
         PCM_Ref          : PCM_Buf.PCM_Buffer (Channels);
         PCM_DUT          : PCM_Buf.PCM_Buffer (Channels);
         PCM_Mix          : PCM_Buf.PCM_Buffer (Channels);
         Wave_Format      : RIFF.Wave_Format_Extensible;
         EOF_Ref, EOF_DUT : Boolean;
      begin
         RIFF.Set_Default (Wave_Format);

         Wav_Read.Open (WF_Ref, File_Ref);
         Wav_Read.Open (WF_DUT, File_DUT);
         Wav_Write.Open (WF_Mix, File_Mix, Wave_Format);
         loop
            PCM_IO.Read (WF_Ref, PCM_Ref, EOF_Ref);
            PCM_IO.Read (WF_DUT, PCM_DUT, EOF_DUT);
            PCM_Mix := PCM_Op."+" (PCM_Ref, PCM_DUT);
            PCM_IO.Write (WF_Mix, PCM_Mix);
            exit when EOF_Ref or EOF_DUT;
         end loop;
         Wav_Read.Close (WF_Ref);
         Wav_Read.Close (WF_DUT);
         Wav_Write.Close (WF_Mix);
      end Mix_Files;

   end Wave_Test;

   procedure Print_Usage is
   begin
      Put_Line ("Usage: " & Command_Name & " <command>");
      New_Line;
      Put_Line ("   info    <input_wavefile>");
      Put_Line ("   copy    <input_wavefile> <output_wavefile>");
      Put_Line ("   compare <ref_wavefile>   <dut_wavefile>");
      Put_Line ("   diff    <ref_wavefile>   <dut_wavefile>     "
                & "<diff_wavefile>");
      Put_Line ("   mix     <wavefile_1>     <wavefile_1>       "
                & "<mix_wavefile>");
   end Print_Usage;

begin

   if Argument_Count >= 1 then
      if Argument (1) = "info" and then Argument_Count = 2 then
         Command_Line_OK := True;
         Put_Line ("Information from: " & Argument (2));
         Wave_Test.Display_Info_File (Argument (2));
      elsif Argument (1) = "copy" and then Argument_Count = 3 then
         Command_Line_OK := True;
         Put_Line ("Copying from: " & Argument (2));
         Put_Line ("Copying to:   " & Argument (3));
         Wave_Test.Copy_File (Argument (2), Argument (3));
      elsif Argument (1) = "compare" and then Argument_Count = 3 then
         Command_Line_OK := True;
         Put_Line ("Reference: " & Argument (2));
         Put_Line ("DUT:       " & Argument (3));
         Wave_Test.Compare_Files (Argument (2), Argument (3));
      elsif Argument (1) = "diff" and then Argument_Count = 4 then
         Command_Line_OK := True;
         Put_Line ("Reference: " & Argument (2));
         Put_Line ("DUT:       " & Argument (3));
         Put_Line ("Diff:      " & Argument (4));
         Wave_Test.Diff_Files (Argument (2), Argument (3), Argument (4));
      elsif Argument (1) = "mix" and then Argument_Count = 4 then
         Command_Line_OK := True;
         Put_Line ("Wavefile #1: " & Argument (2));
         Put_Line ("Wavefile #2: " & Argument (3));
         Put_Line ("Mix file:    " & Argument (4));
         Wave_Test.Mix_Files (Argument (2), Argument (3), Argument (4));
      end if;
   end if;

   if not Command_Line_OK then
      Print_Usage;
   end if;

end Wavefiles_Test;
