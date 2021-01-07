------------------------------------------------------------------------------
--                                                                          --
--          THIS IS AN AUTOMATICALLY GENERATED FILE! DO NOT EDIT!           --
--                                                                          --
--                               WAVEFILES                                  --
--                                                                          --
--                      Wavefile data I/O operations                        --
--                                                                          --
--  The MIT License (MIT)                                                   --
--                                                                          --
--  Copyright (c) 2015 -- 2021 Gustavo A. Hoffmann                          --
--                                                                          --
--  Permission is hereby granted, free of charge, to any person obtaining   --
--  a copy of this software and associated documentation files (the         --
--  "Software"), to deal in the Software without restriction, including     --
--  without limitation the rights to use, copy, modify, merge, publish,     --
--  distribute, sublicense, and / or sell copies of the Software, and to    --
--  permit persons to whom the Software is furnished to do so, subject to   --
--  the following conditions:                                               --
--                                                                          --
--  The above copyright notice and this permission notice shall be          --
--  included in all copies or substantial portions of the Software.         --
--                                                                          --
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         --
--  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      --
--  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  --
--  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY    --
--  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,    --
--  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE       --
--  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                  --
------------------------------------------------------------------------------

package body Audio.Wavefiles.Generic_Float_Wav_IO is

   procedure Read_Wav_MC_Sample (WF  : in out Wavefile;
                                 Wav :    out Wav_MC_Sample)
     with Inline;
   procedure Write_Wav_MC_Sample (WF  : in out Wavefile;
                                  Wav :        Wav_MC_Sample)
     with Inline;

   ------------------------
   -- Read_Wav_MC_Sample --
   ------------------------

   procedure Read_Wav_MC_Sample
     (WF  : in out Wavefile;
      Wav :    out Wav_MC_Sample)
   is
      N_Ch   : constant Positive := Number_Of_Channels (WF);
      Sample : Wav_Sample;

      use Ada.Streams.Stream_IO;

      Prev_File_Index  : constant Positive_Count := Index (WF.File)
        with Ghost;
      Expected_Byte_IO : constant Positive_Count
        := Positive_Count
          (To_Positive (WF.Wave_Format.Bits_Per_Sample) * N_Ch / 8)
        with Ghost;
   begin
      for J in Wav'Range loop
         Wav_Sample'Read (WF.File_Access, Sample);

         Wav (J) := Sample;
         if Ada.Streams.Stream_IO.End_Of_File (WF.File) and then
           J < Wav'Last
         then
            --  Cannot read data for all channels
            WF.Set_Error (Wavefile_Error_File_Too_Short);
         end if;
      end loop;

      WF.Sample_Pos.Current := WF.Sample_Pos.Current + 1;

      pragma Assert (Ada.Streams.Stream_IO.Index (WF.File) =
                       Prev_File_Index + Expected_Byte_IO);
   end Read_Wav_MC_Sample;

   -------------------------
   -- Write_Wav_MC_Sample --
   -------------------------

   procedure Write_Wav_MC_Sample
     (WF  : in out Wavefile;
      Wav :        Wav_MC_Sample)
   is
      N_Ch : constant Positive := Number_Of_Channels (WF);

      use Ada.Streams.Stream_IO;

      Prev_File_Index  : constant Positive_Count := Index (WF.File)
        with Ghost;
      Expected_Byte_IO : constant Positive_Count
        := Positive_Count
          (To_Positive (WF.Wave_Format.Bits_Per_Sample) * N_Ch / 8)
        with Ghost;
   begin
      Wav_MC_Sample'Write (WF.File_Access, Wav);

      WF.Sample_Pos := (Total   => WF.Sample_Pos.Total   + 1,
                        Current => WF.Sample_Pos.Current + 1);

      pragma Assert (Ada.Streams.Stream_IO.Index (WF.File) =
                       Prev_File_Index + Expected_Byte_IO);
   end Write_Wav_MC_Sample;

   ---------
   -- Get --
   ---------

   function Get (WF  : in out Wavefile) return Wav_MC_Sample
   is
      N_Ch   : constant Positive := Number_Of_Channels (WF);

      Channel_Range_Valid_Last : constant Channel_Range :=
        Channel_Range'Val (N_Ch - 1
                           + Channel_Range'Pos (Channel_Range'First));

      subtype Valid_Channel_Range is Channel_Range range
        Channel_Range'First .. Channel_Range_Valid_Last;
   begin
      return Wav : Wav_MC_Sample (Valid_Channel_Range) do
         Read_Wav_MC_Sample (WF, Wav);
      end return;
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get (WF  : in out Wavefile;
                  Wav :    out Wav_MC_Sample) is
   begin
      Read_Wav_MC_Sample (WF, Wav);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put (WF  : in out Wavefile;
                  Wav :        Wav_MC_Sample) is
   begin
      Write_Wav_MC_Sample (WF, Wav);
   end Put;

end Audio.Wavefiles.Generic_Float_Wav_IO;
