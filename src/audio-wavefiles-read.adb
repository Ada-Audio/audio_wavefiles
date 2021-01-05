------------------------------------------------------------------------------
--                                                                          --
--                               WAVEFILES                                  --
--                                                                          --
--                       Wavefile reading routines                          --
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

with Ada.Text_IO;                  use Ada.Text_IO;
with Interfaces;

with Audio.RIFF;                   use Audio.RIFF;
with Audio.Wavefiles.Internals;    use Audio.Wavefiles.Internals;
with Audio.Wavefiles.Report;

package body Audio.Wavefiles.Read is

   procedure Parse_Fmt_Chunk
     (WF : in out Wavefile);
   procedure Parse_Data_Chunk
     (WF : in out Wavefile);

   ---------------------
   -- Parse_Fmt_Chunk --
   ---------------------

   procedure Parse_Fmt_Chunk
     (WF : in out Wavefile)
   is
      Verbose : constant Boolean := False;

      use type Ada.Streams.Stream_IO.Count;

      Found : Wav_Chunk_Element_Found;
   begin
      Find_First_Chunk (Chunks    => WF.RIFF_Info.Chunks,
                        Chunk_Tag => Wav_Chunk_Fmt,
                        Found     => Found);

      if not Found.Success then
         WF.Set_Error (Wavefile_Error_Format_Chuck_Not_Found);
         return;
      else
         Set_File_Index_To_Chunk_Data_Start (WF.File,
                                             Found.Chunk_Element.Start_Index);

         case Found.Chunk_Element.Size is
            when Wave_Format_Chunk_Size'Enum_Rep (Wave_Format_16_Size) =>
               Wave_Format_16'Read (WF.File_Access,
                                    Wave_Format_16 (WF.Wave_Format));
               Reset_For_Wave_Format_16 (WF.Wave_Format);
            when Wave_Format_Chunk_Size'Enum_Rep (Wave_Format_18_Size) =>
               Wave_Format_18'Read (WF.File_Access,
                                    Wave_Format_18 (WF.Wave_Format));
               Reset_For_Wave_Format_18 (WF.Wave_Format);

               if Verbose then
                  Put_Line ("Size of waveformat record "
                            & Integer'Image (
                              Wave_Format_18'Value_Size / 8));
                  Put_Line ("BitsPerSample: "
                            & Positive'Image
                              (To_Positive (WF.Wave_Format.Bits_Per_Sample)));
                  Put_Line ("Size: " & Interfaces.Unsigned_16'Image
                            (WF.Wave_Format.Size));
               end if;

            when Wave_Format_Chunk_Size'Enum_Rep
                 (Wave_Format_Extensible_Size) =>
               Wave_Format_Extensible'Read (WF.File_Access,
                                            WF.Wave_Format);

               if Verbose then
                  Put_Line ("Size of waveformat record "
                            & Integer'Image
                              (Wave_Format_Extensible'Value_Size / 8));
                  Put_Line ("File index: " & Integer'Image (
                            Integer (Ada.Streams.Stream_IO.Index (WF.File))));
                  Put_Line ("BitsPerSample: "
                            & Positive'Image
                              (To_Positive (WF.Wave_Format.Bits_Per_Sample)));
                  Put_Line ("Size: " & Interfaces.Unsigned_16'Image
                            (WF.Wave_Format.Size));
               end if;

            when others =>
               WF.Set_Error (Wavefile_Error_Unsupported_Format_Size);
               return;
         end case;

         if Verbose then
            Wavefiles.Report.Display_Info (WF);
            Put_Line ("fmt chunk size: " & Long_Integer'Image
                      (Found.Chunk_Element.Size));
         end if;
      end if;
   end Parse_Fmt_Chunk;

   ----------------------
   -- Parse_Data_Chunk --
   ----------------------

   procedure Parse_Data_Chunk
     (WF : in out Wavefile)
   is
      Verbose : constant Boolean := False;

      Found : Wav_Chunk_Element_Found;
   begin
      Find_First_Chunk (Chunks        => WF.RIFF_Info.Chunks,
                        Chunk_Tag     => Wav_Chunk_Data,
                        Found         => Found);

      if not Found.Success then
         WF.Set_Error (Wavefile_Error_Data_Chuck_Not_Found);
         return;
      else
         Set_File_Index_To_Chunk_Data_Start (WF.File,
                                             Found.Chunk_Element.Start_Index);

         if Verbose then
            Put_Line ("RIFF Tag: " & Found.Chunk_Element.ID);
         end if;

         WF.Sample_Pos :=
           (Current => First_Sample_Count,
            Total   =>
              Number_Of_Samples
                (Chunk_Size        => Found.Chunk_Element.Size,
                 Channels_In_Total => WF.Wave_Format.Channels,
                 Bits_Per_Sample   => WF.Wave_Format.Bits_Per_Sample));

         if Verbose then
            Put_Line ("Data chunk size: "
                      & Long_Integer'Image (Found.Chunk_Element.Size));
            Put_Line ("Num samples: "
                      & Sample_Count'Image (WF.Sample_Pos.Total));
         end if;
      end if;
   end Parse_Data_Chunk;

   ---------------------------
   -- Read_Until_Data_Start --
   ---------------------------

   procedure Read_Until_Data_Start
     (WF          : in out Wavefile)
   is
      Verbose      : constant Boolean := False;
   begin
      if WF.RIFF_Info.Chunks.Is_Empty then
         Audio.Wavefiles.Read.Parse_Wav_Chunks (WF);
      end if;

      if Verbose then
         Audio.Wavefiles.Report.Display_Info (WF.RIFF_Info);
      end if;

      Parse_Fmt_Chunk (WF);

      Parse_Data_Chunk (WF);

   end Read_Until_Data_Start;

   ----------------------
   -- Parse_Wav_Chunks --
   ----------------------

   procedure Parse_Wav_Chunks
     (WF     : in out Wavefile)
   is
      use Ada.Streams;

      Prev_File_Index : constant Ada.Streams.Stream_IO.Positive_Count :=
                          Stream_IO.Index (WF.File);
      Curr_File_Index :          Ada.Streams.Stream_IO.Positive_Count;

      Chunk_Header    : RIFF_Chunk_Header;

      Info            : RIFF_Information renames WF.RIFF_Info;
   begin
      --  Set index to initial RIFF chunk
      Ada.Streams.Stream_IO.Set_Index (WF.File, 1);

      Info.Chunks.Clear;

      Parse_RIFF_Header : declare
         FOURCC : FOURCC_String;
      begin
         RIFF_Chunk_Header'Read (WF.File_Access, Chunk_Header);
         FOURCC_String'Read (WF.File_Access, FOURCC);

         Info.Id     := To_RIFF_Identifier (Chunk_Header.ID);
         Info.Format := To_RIFF_Format (FOURCC);
      end Parse_RIFF_Header;

      if Info.Id    /= RIFF_Identifier_Unknown and then
        Info.Format /= RIFF_Format_Unknown
      then
         loop
            Curr_File_Index := Ada.Streams.Stream_IO.Index (WF.File);

            RIFF_Chunk_Header'Read (WF.File_Access, Chunk_Header);

            declare
               Chunk_Element : constant Wav_Chunk_Element
                 := (Chunk_Tag    => To_Wav_Chunk_Tag (Chunk_Header.ID),
                     ID           => Chunk_Header.ID,
                     Size         => Long_Integer (Chunk_Header.Size),
                     Start_Index  => Curr_File_Index,
                     Consolidated => True);
            begin
               Info.Chunks.Append (Chunk_Element);
            end;

            --  This is most probably an error in the wavefile:
            exit when Natural (Chunk_Header.Size) = 0;

            Skip_Bytes (WF.File, Chunk_Header.Size);

            exit when Ada.Streams.Stream_IO.End_Of_File (WF.File);
         end loop;
      end if;

      --  Setting file index back to previous location
      Ada.Streams.Stream_IO.Set_Index (WF.File, Prev_File_Index);
   end Parse_Wav_Chunks;

   ------------------------
   -- Set_Current_Sample --
   ------------------------

   procedure Set_Current_Sample
     (WF       : in out Wavefile;
      Position :        Sample_Count)
   is
      Found : Wav_Chunk_Element_Found;
   begin
      Find_First_Chunk (Chunks    => WF.RIFF_Info.Chunks,
                        Chunk_Tag => Wav_Chunk_Data,
                        Found     => Found);

      if not Found.Success then
         WF.Set_Error (Wavefile_Error_Data_Chuck_Not_Found);
         return;
      else
         Set_File_Index_To_Chunk_Data_Start
           (File              => WF.File,
            Chunk_Start_Index => Found.Chunk_Element.Start_Index,
            Position_In_Chunk => Number_Of_Bytes
              (Position          => Position - WF.First_Sample,
               Channels_In_Total => WF.Wave_Format.Channels,
               Bits_Per_Sample   => WF.Wave_Format.Bits_Per_Sample));

         WF.Sample_Pos.Current := Position;
      end if;
   end Set_Current_Sample;

end Audio.Wavefiles.Read;
