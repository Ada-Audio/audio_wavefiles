-------------------------------------------------------------------------------
--
--                                WAVEFILES
--
--                            Wavefile reading
--
--  The MIT License (MIT)
--
--  Copyright (c) 2015 -- 2020 Gustavo A. Hoffmann
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

with Ada.Text_IO;                  use Ada.Text_IO;

with Audio.RIFF;                   use Audio.RIFF;
with Audio.Wavefiles.RIFF;         use Audio.Wavefiles.RIFF;
with Audio.Wavefiles.Internals;    use Audio.Wavefiles.Internals;

package body Audio.Wavefiles.Read is

   use Ada.Streams.Stream_IO;

   procedure Open
     (WF          : in out Wavefile;
      File_Name   : String;
      Wave_Format : in out Wave_Format_Extensible)
   is
      RIFF_Tag    : RIFF_Tag_Type;
      RIFF_Chunk  : RIFF_Chunk_Type;
      Verbose     : constant Boolean := False;
   begin
      if WF.Is_Opened then
         raise Wavefile_Error;
      end if;

      --  Open input wavefile
      Open (WF.File, In_File, File_Name);
      WF.File_Access := Stream (WF.File);

      WF.Is_Opened := True;
      WF.Samples_Read := 0;

      --  Read/check RIFF Chunk
      RIFF_Tag_Type'Read (WF.File_Access, RIFF_Tag);
      if RIFF_Tag.FOURCC /= "RIFF" then
         raise Wavefile_Error;
      end if;
      if Verbose then
         Put_Line ("RIFF Tag: " & RIFF_Tag.FOURCC);
         Put_Line ("RIFF/WAVE chunk size: "
                   & Unsigned_32'Image (RIFF_Tag.Size));
      end if;

      --  Read/check WAVE tag
      RIFF_Chunk_Type'Read (WF.File_Access, RIFF_Chunk);
      if RIFF_Chunk.FOURCC /= "WAVE" then
         raise Wavefile_Error;
      end if;
      if Verbose then
         Put_Line ("RIFF Tag: " & RIFF_Chunk.FOURCC);
      end if;

      --  Read/skip chunks until fmt chunk
      loop
         RIFF_Tag_Type'Read (WF.File_Access, RIFF_Tag);
         if Verbose then
            Put_Line ("RIFF Tag: " & RIFF_Tag.FOURCC);
         end if;
         exit when RIFF_Tag.FOURCC = "fmt ";
         Skip_Bytes (WF.File, RIFF_Tag.Size);
      end loop;

      case RIFF_Tag.Size is
         when Wave_Format_Chunk_Size'Enum_Rep (Wave_Format_16_Size) =>
            Wave_Format_16'Read (WF.File_Access,
                                 Wave_Format_16 (WF.Wave_Format));

         when Wave_Format_Chunk_Size'Enum_Rep (Wave_Format_18_Size) =>
            Wave_Format_18'Read (WF.File_Access,
                                 Wave_Format_18 (WF.Wave_Format));

            if Verbose then
               Put_Line ("Size of waveformat record "
                         & Integer'Image (
                           Wave_Format_18'Value_Size / 8));
               Put_Line ("BitsPerSample: "
                         & Unsigned_16'Image (WF.Wave_Format.Bits_Per_Sample));
               Put_Line ("Size: " & Unsigned_16'Image (WF.Wave_Format.Size));
            end if;

         when Wave_Format_Chunk_Size'Enum_Rep (Wave_Format_Extensible_Size) =>
            Wave_Format_Extensible'Read (WF.File_Access,
                                         WF.Wave_Format);

            if Verbose then
               Put_Line ("Size of waveformat record "
                         & Integer'Image
                           (Wave_Format_Extensible'Value_Size / 8));
               Put_Line ("File index: " & Integer'Image (
                         Integer (Ada.Streams.Stream_IO.Index (WF.File))));
               Put_Line ("BitsPerSample: "
                         & Unsigned_16'Image (WF.Wave_Format.Bits_Per_Sample));
               Put_Line ("Size: " & Unsigned_16'Image (WF.Wave_Format.Size));
            end if;

         when others =>
            raise Wavefile_Error;
      end case;

      if Verbose then
         Print (WF.Wave_Format);
         Put_Line ("fmt chunk size: " & Unsigned_32'Image (RIFF_Tag.Size));
      end if;

      --  Read/skip chunks until data chunk
      loop
         RIFF_Tag_Type'Read (WF.File_Access, RIFF_Tag);
         if Verbose then
            Put_Line ("RIFF Tag: " & RIFF_Tag.FOURCC);
         end if;
         exit when RIFF_Tag.FOURCC = "data";
         Skip_Bytes (WF.File, RIFF_Tag.Size);
      end loop;

      WF.Samples := Long_Integer (RIFF_Tag.Size)
        / Long_Integer (WF.Wave_Format.Bits_Per_Sample / 8);

      if Verbose then
         Put_Line ("Data chunk size: " & Unsigned_32'Image (RIFF_Tag.Size));
         Put_Line ("Num samples: " & Long_Integer'Image (WF.Samples));
         Put_Line ("Num samples: " & Long_Integer'Image (WF.Samples
                   / Long_Integer (WF.Wave_Format.Channels)));
      end if;

      Wave_Format := Format_Of_Wavefile (WF);

   end Open;

   function Is_EOF
     (WF   : in out Wavefile) return Boolean is
   begin
      if WF.Samples_Read >= WF.Samples or
        Ada.Streams.Stream_IO.End_Of_File (WF.File)
      then
         return True;
      else
         return False;
      end if;
   end Is_EOF;

   procedure Display_Info (WF : in Wavefile) is
   begin
      Print (WF.Wave_Format);

   end Display_Info;

   procedure Close (WF  : in out Wavefile) is
   begin
      if not WF.Is_Opened then
         raise Wavefile_Error;
      end if;

      Close (WF.File);

      WF.Is_Opened := False;
   end Close;

end Audio.Wavefiles.Read;
