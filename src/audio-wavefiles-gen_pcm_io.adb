-------------------------------------------------------------------------------
--
--                                WAVEFILES
--
--                           Generic Wavefile I/O
--
--  The MIT License (MIT)
--
--  Copyright (c) 2020 Gustavo A. Hoffmann
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

package body Audio.Wavefiles.Gen_PCM_IO is

   function Get (WF  : in out Wavefile) return Audio_Samples
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
   end Get;

   procedure Put (WF : in out Wavefile;
                    B  :        Audio_Samples) is
      Ch : constant Positive := Positive (WF.Wave_Format.Channels);
   begin
      Audio_Samples'Write (WF.File_Access, B);
      WF.Samples := WF.Samples + Long_Integer (Ch);
   end Put;

end Audio.Wavefiles.Gen_PCM_IO;
