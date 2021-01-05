------------------------------------------------------------------------------
--                                                                          --
--                               WAVEFILES                                  --
--                                                                          --
--                       Wavefile reporting routines                        --
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

with Ada.Text_IO;                    use Ada.Text_IO;

with Audio.RIFF.Wav.Formats.Report;

package body Audio.Wavefiles.Report is

   function To_String (Err : Wavefile_Error_Code) return String;

   function To_String (Err : Wavefile_Warning_Code) return String;

   ------------------
   -- Display_Info --
   ------------------

   procedure Display_Info (WF : Wavefile) is
   begin
      Audio.RIFF.Wav.Formats.Report.Print (WF.Wave_Format);
   end Display_Info;

   ------------------
   -- Display_Info --
   ------------------

   procedure Display_Info (RIFF_Info : RIFF_Information) is
   begin
      Put_Line ("---- RIFF Chunks Information ----");
      Put_Line ("Id:          " & RIFF_Identifier'Image (RIFF_Info.Id));
      Put_Line ("Format:      " & RIFF_Format'Image (RIFF_Info.Format));
      Put_Line ("Chunk count: "
                & Ada.Containers.Count_Type'Image (RIFF_Info.Chunks.Length));
      New_Line;

      for Chunk_Element of RIFF_Info.Chunks loop
         Put_Line ("- ID:           "
                   & Chunk_Element.ID);
         Put_Line ("  Internal tag: "
                   & Wav_Chunk_Tag'Image (Chunk_Element.Chunk_Tag));
         Put_Line ("  Size:         "
                   & Long_Integer'Image (Chunk_Element.Size));
         Put_Line ("  Start index:  "
                   & Ada.Streams.Stream_IO.Positive_Count'Image
                     (Chunk_Element.Start_Index));
         Put_Line ("  Consolidated: "
                   & Boolean'Image (Chunk_Element.Consolidated));
         New_Line;
      end loop;
      Put_Line ("---------------------------------");

   end Display_Info;

   ---------------
   -- To_String --
   ---------------

   function To_String (Err : Wavefile_Error_Code) return String is
   begin
      case Err is
      when Wavefile_Error_File_Not_Open =>
         return "File not open";
      when Wavefile_Error_File_Already_Open =>
         return "File already open";
      when Wavefile_Error_File_Too_Short =>
         return "File too short";
      when Wavefile_Error_Format_Chuck_Not_Found =>
         return "Format chunk not found";
      when Wavefile_Error_Data_Chuck_Not_Found =>
         return "Data chunk not found";
      when Wavefile_Error_Unsupported_Wavefile_Format =>
         return "Unsupported wavefile format";
      when Wavefile_Error_Unsupported_Bit_Depth =>
         return "Unsupported bit depth";
      when Wavefile_Error_Unsupported_Format_Size =>
         return "Unsupported format size";
      end case;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Err : Wavefile_Warning_Code) return String is
   begin
      case Err is
      when Wavefile_Warning_Inconsistent_Channel_Mask =>
         return "Inconsistent channel mask";
      end case;
   end To_String;

   --------------------
   -- Display_Errors --
   --------------------

   procedure Display_Errors (WF : Wavefile) is
   begin
      Put_Line ("---------------- Wavefile errors ----------------");
      New_Line;

      for Error_Code in WF.Errors'Range loop
         if WF.Errors (Error_Code) then
            Put_Line ("- " & To_String (Error_Code));
         end if;
      end loop;

      New_Line;
      Put_Line ("-------------------------------------------------");
   end Display_Errors;

   ----------------------
   -- Display_Warnings --
   ----------------------

   procedure Display_Warnings (WF : Wavefile) is
   begin
      Put_Line ("-------------- Wavefile warnings ----------------");
      New_Line;

      for Warning_Code in WF.Warnings'Range loop
         if WF.Warnings (Warning_Code) then
            Put_Line ("- " & To_String (Warning_Code));
         end if;
      end loop;

      New_Line;
      Put_Line ("-------------------------------------------------");
   end Display_Warnings;

end Audio.Wavefiles.Report;
