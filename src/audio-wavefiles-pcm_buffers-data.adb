-------------------------------------------------------------------------------
--
--                                WAVEFILES
--
--                   Internal I/O operations for PCM buffers
--
--  The MIT License (MIT)
--
--  Copyright (c) 2015 Gustavo A. Hoffmann
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

with Ada.Assertions;
with Audio.Wavefiles.PCM_Buffers.Types;

package body Audio.Wavefiles.PCM_Buffers.Data is

   package Audio_Types is new Wavefiles.PCM_Buffers.Types
     (Audio_Res, PCM_Float_Type_Support, To_Long_Float);

   type Audio_Samples is array (Positive range <>) of Audio_Res;
   type PCM_Samples is array (Positive range <>) of PCM_Type;

   function Read_Data (WF  : in out Wavefile) return Audio_Samples;
   procedure Write_Data (WF : in out Wavefile;
                         B  :        Audio_Samples);
   function Convert_Samples (B : Audio_Samples) return PCM_Samples;
   function Convert_Samples (PCM : PCM_Samples) return Audio_Samples;
   function Get_Sample (Buf : PCM_Buffer;
                        Ch  : Positive;
                        N   : Positive) return PCM_Samples;

   function Read_Data (WF  : in out Wavefile) return Audio_Samples
   is
      Ch : constant Positive := Positive (WF.Wave_Format.Channels);
      BB : Audio_Res;
   begin
      return B  : Audio_Samples (1 .. Ch) do
         for J in 1 .. Ch loop

            Audio_Res'Read (WF.File_Access, BB);
            B (J) := BB;
            if Ada.Streams.Stream_IO.End_Of_File (WF.File) and then
              J < Ch
            then
               --  Cannot read data for all channels
               raise Wavefile_Error;
            end if;
         end loop;
      end return;
   end Read_Data;

   procedure Write_Data (WF : in out Wavefile;
                         B  :        Audio_Samples) is
      Ch : constant Positive := Positive (WF.Wave_Format.Channels);
   begin
      Audio_Samples'Write (WF.File_Access, B);
      WF.Samples := WF.Samples + Long_Integer (Ch);
   end Write_Data;

   function Convert_Samples (B : Audio_Samples) return PCM_Samples is
   begin
      return PCM : PCM_Samples (B'Range) do
         for I in PCM'Range loop
            PCM (I) := Audio_Types.Convert_Sample (B (I));
         end loop;
      end return;
   end Convert_Samples;

   function Convert_Samples (PCM : PCM_Samples) return Audio_Samples is
   begin
      return B : Audio_Samples (PCM'Range) do
         for I in B'Range loop
            B (I) := Audio_Types.Convert_Sample (PCM (I));
         end loop;
      end return;
   end Convert_Samples;

   procedure Read_Data
     (WF  : in out Wavefile;
      Buf : in out PCM_Buffer)
   is
      Ch : constant Positive := Positive (WF.Wave_Format.Channels);
   begin
      Ada.Assertions.Assert (Ch <= Buf.Channels,
                             "Unsufficient number of channel in buffer");

      for I in 1 .. Wavefiles.PCM_Buffers.Samples loop
         declare
            B : constant Audio_Samples := Read_Data (WF);
            P : constant PCM_Samples   := Convert_Samples (B);
         begin
            WF.Samples_Read := WF.Samples_Read + Long_Integer (Ch);
            Buf.Info.Samples_Valid := Buf.Info.Samples_Valid + 1;
            for J in P'Range loop
               Buf.Audio_Data (J) (I) := P (J);
            end loop;
         end;

         exit when WF.Samples_Read >= WF.Samples or
           Ada.Streams.Stream_IO.End_Of_File (WF.File);
         --  exit when ;
      end loop;
      for J in 1 .. Ch loop
         Buf.Info.Active (J) := True;
      end loop;
      for J in Ch + 1 .. Buf.Channels loop
         Buf.Info.Active (J) := False;
      end loop;
   end Read_Data;

   function Get_Sample (Buf : PCM_Buffer;
                        Ch  : Positive;
                        N   : Positive) return PCM_Samples is
   begin
      return P : PCM_Samples (1 .. Ch) do
         for J in 1 .. Ch loop
            P (J) := Buf.Audio_Data (J) (N);
         end loop;
      end return;
   end Get_Sample;

   procedure Write_Data
     (WF  : in out Wavefile;
      Buf : in PCM_Buffer)
   is
      Ch : constant Positive := Positive (WF.Wave_Format.Channels);
   begin
      Ada.Assertions.Assert (Ch <= Buf.Channels,
                             "Unsufficient number of channel in buffer");

      for I in 1 .. Buf.Info.Samples_Valid loop
         declare
            P : constant PCM_Samples   := Get_Sample (Buf, Ch, I);
            B : constant Audio_Samples := Convert_Samples (P);
         begin
            Write_Data (WF, B);
         end;
      end loop;

   end Write_Data;

end Audio.Wavefiles.PCM_Buffers.Data;
