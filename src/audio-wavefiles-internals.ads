------------------------------------------------------------------------------
--                                                                          --
--                               WAVEFILES                                  --
--                                                                          --
--                       Internal auxiliary routines                        --
--                                                                          --
--  The MIT License (MIT)                                                   --
--                                                                          --
--  Copyright (c) 2015 -- 2020 Gustavo A. Hoffmann                          --
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

with Ada.Streams.Stream_IO;
with Interfaces;            use Interfaces;

private
package Audio.Wavefiles.Internals is

   procedure Skip_Bytes
     (F     : in out Ada.Streams.Stream_IO.File_Type;
      Bytes :        Unsigned_32);

   procedure Set_File_Index_To_Chunk_Data_Start
     (File              : Ada.Streams.Stream_IO.File_Type;
      Chunk_Start_Index : Ada.Streams.Stream_IO.Positive_Count;
      Position_In_Chunk : Ada.Streams.Stream_IO.Count := 0);

   function Number_Of_Bytes
     (Position          : Sample_Count;
      Channels_In_Total : Interfaces.Unsigned_16;
      Bits_Per_Sample   : Wav_Bit_Depth)
      return Ada.Streams.Stream_IO.Count;

   function Number_Of_Samples
     (Chunk_Size        : Long_Integer;
      Channels_In_Total : Interfaces.Unsigned_16;
      Bits_Per_Sample   : Wav_Bit_Depth) return Sample_Count;

end Audio.Wavefiles.Internals;
