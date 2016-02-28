-------------------------------------------------------------------------------
--
--                                WAVEFILES
--
--                              Main package
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

with Ada.Streams.Stream_IO;
with RIFF;

--  <description>
--     Main package for WAVE file reading / writing
--  </description>

package Wavefiles is
   type Wavefile is limited private;

   Wavefile_Error       : exception;
   Wavefile_Unsupported : exception;

   function Get_Wave_Format
     (W : Wavefile) return  RIFF.Wave_Format_Extensible;

private

   type Wavefile is limited
      record
         Is_Opened        : Boolean      := False;
         File             : Ada.Streams.Stream_IO.File_Type;
         File_Access      : Ada.Streams.Stream_IO.Stream_Access;
         File_Index       : Ada.Streams.Stream_IO.Positive_Count;
         Wave_Format      : RIFF.Wave_Format_Extensible;
         Samples          : Long_Integer;
         Samples_Read     : Long_Integer := 0;
      end record;

end Wavefiles;
