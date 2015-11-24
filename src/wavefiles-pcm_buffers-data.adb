-------------------------------------------------------------------------------
--
--                                WAVEFILES
--
--                   Internal I/O operations for PCM buffers
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

with Ada.Assertions;
with Wavefiles.PCM_Buffers.Types;

package body Wavefiles.PCM_Buffers.Data is

   package Audio_Types is new Wavefiles.PCM_Buffers.Types
     (Audio_Res, PCM_Float_Type_Support, To_Long_Float);

   procedure Read_Data (WF  : in out Wavefile;
                        Buf : in out PCM_Buffer) is
      Ch : constant Positive := Positive (WF.Wave_Format.Channels);
      type Audio_Sample is array (1 .. Ch) of Audio_Res;
      B  : Audio_Sample;
      BB : Audio_Res;

   begin
      Ada.Assertions.Assert (Ch <= Buf.Channels,
                             "Unsufficient number of channel in buffer");

      for I in 1 .. Wavefiles.PCM_Buffers.Samples loop
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

         WF.Samples_Read := WF.Samples_Read + Long_Integer (Ch);
         Buf.Info.Samples_Valid := Buf.Info.Samples_Valid + 1;
         for J in 1 .. Ch loop
            Buf.Audio_Data (J) (I) := Audio_Types.Convert_Sample (B (J));
         end loop;

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


   procedure Write_Data (WF  : in out Wavefile;
                         Buf : in PCM_Buffer) is
      Ch : constant Positive := Positive (WF.Wave_Format.Channels);
      type Audio_Sample is array (1 .. Ch) of Audio_Res;
      B  : Audio_Sample;
   begin
      Ada.Assertions.Assert (Ch <= Buf.Channels,
                             "Unsufficient number of channel in buffer");

      for I in 1 .. Buf.Info.Samples_Valid loop
         for J in 1 .. Ch loop
            B (J) := Audio_Types.Convert_Sample (Buf.Audio_Data (J) (I));
         end loop;
         Audio_Sample'Write (WF.File_Access, B);
         WF.Samples := WF.Samples + Long_Integer (Ch);
      end loop;

   end Write_Data;

end Wavefiles.PCM_Buffers.Data;
