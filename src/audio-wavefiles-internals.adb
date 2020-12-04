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

with Audio.RIFF;

package body Audio.Wavefiles.Internals is

   use Ada.Streams.Stream_IO;

   ----------------
   -- Skip_Bytes --
   ----------------

   procedure Skip_Bytes
     (F     : in out Ada.Streams.Stream_IO.File_Type;
      Bytes :        Unsigned_32) is
   begin
      Set_Index (F,
                 Ada.Streams.Stream_IO.Index (F)
                 + Ada.Streams.Stream_IO.Count (Bytes));
   end Skip_Bytes;

   ----------------------------------------
   -- Set_File_Index_To_Chunk_Data_Start --
   ----------------------------------------

   procedure Set_File_Index_To_Chunk_Data_Start
     (File              : Ada.Streams.Stream_IO.File_Type;
      Chunk_Start_Index : Ada.Streams.Stream_IO.Positive_Count;
      Position_In_Chunk : Ada.Streams.Stream_IO.Count := 0)
   is
      Chunk_Data_Index : Ada.Streams.Stream_IO.Positive_Count;

      Chunk_Header_Size : constant Ada.Streams.Stream_IO.Positive_Count :=
                            Audio.RIFF.RIFF_Chunk_Header'Size / 8;
   begin
      --  Calculate file index corresponding to a position after the
      --  RIFF chunk header
      Chunk_Data_Index := Chunk_Start_Index + Chunk_Header_Size
        + Position_In_Chunk;

      Ada.Streams.Stream_IO.Set_Index (File, Chunk_Data_Index);
   end Set_File_Index_To_Chunk_Data_Start;

   ---------------------
   -- Number_Of_Bytes --
   ---------------------

   function Number_Of_Bytes
     (Position          : Sample_Count;
      Channels_In_Total : Interfaces.Unsigned_16;
      Bits_Per_Sample   : Wav_Bit_Depth)
      return Ada.Streams.Stream_IO.Count
   is (Ada.Streams.Stream_IO.Count
       (Long_Integer (Position)
        * (Long_Integer (To_Positive (Bits_Per_Sample)) / 8)
        * Long_Integer (Channels_In_Total)));

   -----------------------
   -- Number_Of_Samples --
   -----------------------

   function Number_Of_Samples
     (Chunk_Size        : Long_Integer;
      Channels_In_Total : Interfaces.Unsigned_16;
      Bits_Per_Sample   : Wav_Bit_Depth) return Sample_Count
   is (Sample_Count (Chunk_Size)
       / (Sample_Count (To_Positive (Bits_Per_Sample)) / 8)
       / Sample_Count (Channels_In_Total));

end Audio.Wavefiles.Internals;
