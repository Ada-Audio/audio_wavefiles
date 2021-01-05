------------------------------------------------------------------------------
--                                                                          --
--                               WAVEFILES                                  --
--                                                                          --
--                        Wavefile writing routines                         --
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

with Interfaces;                   use Interfaces;

with Audio.Wavefiles.Internals;    use Audio.Wavefiles.Internals;
with Audio.RIFF;                   use Audio.RIFF;
with Audio.RIFF.Wav.Formats.Report;

package body Audio.Wavefiles.Write is

   procedure Write_RIFF_Header
     (WF            : in out Wavefile;
      Id_FOURCC     :        FOURCC_String;
      Format_FOURCC :        FOURCC_String);
   procedure Write_Fmt_Chunk
     (WF : in out Wavefile);
   procedure Write_Data_Chunk
     (WF : in out Wavefile);

   use Ada.Streams.Stream_IO;

   -----------------------
   -- Write_RIFF_Header --
   -----------------------

   procedure Write_RIFF_Header
     (WF            : in out Wavefile;
      Id_FOURCC     :        FOURCC_String;
      Format_FOURCC :        FOURCC_String)
   is
      Chunk_Header  : constant RIFF_Chunk_Header := (Id_FOURCC, 0);
   begin
      --  Write RIFF chunk
      RIFF_Chunk_Header'Write (WF.File_Access, Chunk_Header);

      --  Write WAVE tag
      FOURCC_String'Write (WF.File_Access, Format_FOURCC);

      --  Update RIFF_Info
      WF.RIFF_Info.Id     := To_RIFF_Identifier (Chunk_Header.ID);
      WF.RIFF_Info.Format := To_RIFF_Format (Format_FOURCC);
   end Write_RIFF_Header;

   ---------------------
   -- Write_Fmt_Chunk --
   ---------------------

   procedure Write_Fmt_Chunk
     (WF : in out Wavefile)
   is
      function Chunk_Size (Format_Size : Unsigned_16) return Unsigned_32;
      procedure Write_Chunk (Chunk_Size : Unsigned_32);

      ----------------
      -- Chunk_Size --
      ----------------

      function Chunk_Size (Format_Size : Unsigned_16) return Unsigned_32 is
      begin
         if Format_Size = 0 then
            return Wave_Format_Chunk_Size'Enum_Rep (Wave_Format_18_Size);
         else
            return Wave_Format_Chunk_Size'Enum_Rep
              (Wave_Format_Extensible_Size);
         end if;
      end Chunk_Size;

      -----------------
      -- Write_Chunk --
      -----------------

      procedure Write_Chunk (Chunk_Size : Unsigned_32) is
      begin
         case Chunk_Size is
         when Wave_Format_Chunk_Size'Enum_Rep (Wave_Format_16_Size) =>
            Wave_Format_16'Write (WF.File_Access,
                                  Wave_Format_16 (WF.Wave_Format));
         when Wave_Format_Chunk_Size'Enum_Rep (Wave_Format_18_Size) =>
            Wave_Format_18'Write (WF.File_Access,
                                  Wave_Format_18 (WF.Wave_Format));
         when Wave_Format_Chunk_Size'Enum_Rep (Wave_Format_Extensible_Size) =>
            Wave_Format_Extensible'Write (WF.File_Access,
                                          WF.Wave_Format);
         when others =>
            WF.Set_Error (Wavefile_Error_Unsupported_Format_Size);
            return;
         end case;
      end Write_Chunk;

      Chunk_Header : constant RIFF_Chunk_Header :=
                       ("fmt ",
                        Chunk_Size (WF.Wave_Format.Size));

      File_Index   : Ada.Streams.Stream_IO.Positive_Count;

      Verbose      : constant Boolean := False;

      use Audio.RIFF.Wav.Formats.Report;
   begin
      File_Index := Ada.Streams.Stream_IO.Index (WF.File);

      RIFF_Chunk_Header'Write (WF.File_Access, Chunk_Header);

      Append_Chunk : declare
         Chunk_Element : constant Wav_Chunk_Element
           := (Chunk_Tag    => To_Wav_Chunk_Tag (Chunk_Header.ID),
               ID           => Chunk_Header.ID,
               Size         => Long_Integer (Chunk_Header.Size),
               Start_Index  => File_Index,
               Consolidated => True);
      begin
         WF.RIFF_Info.Chunks.Append (Chunk_Element);
      end Append_Chunk;

      Write_Chunk (Chunk_Header.Size);

      if Verbose then
         Print (WF.Wave_Format);
      end if;
   end Write_Fmt_Chunk;

   ----------------------
   -- Write_Data_Chunk --
   ----------------------

   procedure Write_Data_Chunk
     (WF : in out Wavefile)
   is
      Chunk_Header : constant RIFF_Chunk_Header := ("data", 0);

      File_Index   : Ada.Streams.Stream_IO.Positive_Count;
   begin
      File_Index := Ada.Streams.Stream_IO.Index (WF.File);

      RIFF_Chunk_Header'Write (WF.File_Access, Chunk_Header);

      Append_Chunk : declare
         Chunk_Element : constant Wav_Chunk_Element
           := (Chunk_Tag    => To_Wav_Chunk_Tag (Chunk_Header.ID),
               ID           => Chunk_Header.ID,
               Size         => Long_Integer (Chunk_Header.Size),
               Start_Index  => File_Index,
               Consolidated => True);
      begin
         WF.RIFF_Info.Chunks.Append (Chunk_Element);
      end Append_Chunk;
   end Write_Data_Chunk;

   ----------------------------
   -- Write_Until_Data_Start --
   ----------------------------

   procedure Write_Until_Data_Start
     (WF          : in out Wavefile) is
   begin
      Write_RIFF_Header (WF, "RIFF", "WAVE");
      Write_Fmt_Chunk (WF);
      Write_Data_Chunk (WF);
   end Write_Until_Data_Start;

   ----------------------
   -- Update_Data_Size --
   ----------------------

   procedure Update_Data_Size
     (WF  : in out Wavefile)
   is
      Chunk_Header : RIFF_Chunk_Header;
      Size         : constant Unsigned_32
        := Unsigned_32 (Number_Of_Bytes
                        (Position          => WF.Sample_Pos.Total,
                         Channels_In_Total => WF.Wave_Format.Channels,
                         Bits_Per_Sample   => WF.Wave_Format.Bits_Per_Sample));
   begin
      --  Update/finalize RIFF chunk
      Update_RIFF_Header_Chunk : declare
      begin
         Ada.Streams.Stream_IO.Set_Index (WF.File, 1);
         Chunk_Header.ID   := "RIFF";
         Chunk_Header.Size := Size + 36;
         RIFF_Chunk_Header'Write (WF.File_Access, Chunk_Header);
      end Update_RIFF_Header_Chunk;

      --  Update/finalize RIFF tag of data chunk
      Update_Data_Chunk : declare
         Found : Wav_Chunk_Element_Found;
      begin
         Find_First_Chunk (Chunks    => WF.RIFF_Info.Chunks,
                           Chunk_Tag => Wav_Chunk_Data,
                           Found     => Found);

         if not Found.Success then
            WF.Set_Error (Wavefile_Error_Data_Chuck_Not_Found);
            return;
         else
            Ada.Streams.Stream_IO.Set_Index (WF.File,
                                             Found.Chunk_Element.Start_Index);
            Chunk_Header.ID   := "data";
            Chunk_Header.Size := Size;

            --  WF.Samples is already multiplied by WF.Wave_Format.Channels
            RIFF_Chunk_Header'Write (WF.File_Access, Chunk_Header);
         end if;
      end Update_Data_Chunk;

   end Update_Data_Size;

end Audio.Wavefiles.Write;
