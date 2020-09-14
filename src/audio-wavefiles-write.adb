-------------------------------------------------------------------------------
--
--                                WAVEFILES
--
--                            Wavefile writing
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

with Interfaces;                   use Interfaces;

with Audio.RIFF;                   use Audio.RIFF;
with Audio.Wavefiles.Internals;    use Audio.Wavefiles.Internals;
with Audio.Wavefiles.Gen_Float_IO;
with Audio.Wavefiles.Gen_Fixed_IO;

package body Audio.Wavefiles.Write is

   use Ada.Streams.Stream_IO;

   procedure Open
     (WF          : in out Wavefile;
      File_Name   : String;
      Wave_Format : RIFF.Wave_Format_Extensible)
   is
      RIFF_Tag    : RIFF_Tag_Type;
      RIFF_Chunk  : RIFF_Chunk_Type;
   begin
      if WF.Is_Opened then
         raise Wavefile_Error;
      end if;

      WF.Wave_Format := Wave_Format;

      --  Open output wavefile
      Create (WF.File, Out_File, File_Name);
      WF.File_Access := Stream (WF.File);
      WF.Is_Opened := True;

      --  Write RIFF chunk
      RIFF_Tag.FOURCC := "RIFF";
      RIFF_Tag.Size   := 0;
      RIFF_Tag_Type'Write (WF.File_Access, RIFF_Tag);

      --  Write WAVE tag
      RIFF_Chunk.FOURCC := "WAVE";
      RIFF_Chunk_Type'Write (WF.File_Access, RIFF_Chunk);

      --  Write fmt chunk
      RIFF_Tag.FOURCC := "fmt ";
      if WF.Wave_Format.Size = 0 then
         RIFF_Tag.Size   := Wave_Format_Chunk_Size'Enum_Rep
           (Wave_Format_18_Size);
      else
         RIFF_Tag.Size   := Wave_Format_Chunk_Size'Enum_Rep
           (Wave_Format_Extensible_Size);
      end if;
      RIFF_Tag_Type'Write (WF.File_Access, RIFF_Tag);

      case RIFF_Tag.Size is
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
            raise Wavefile_Error;
      end case;
      Print (WF.Wave_Format);

      --  Write data chunk
      WF.File_Index := Ada.Streams.Stream_IO.Index (WF.File);
      RIFF_Tag.FOURCC := "data";
      RIFF_Tag.Size   := 0;
      RIFF_Tag_Type'Write (WF.File_Access, RIFF_Tag);

      WF.Samples := 0;
   end Open;

   procedure Put_Float
     (WF   : in out Wavefile;
      PCM  :        MC_Samples)
   is
      package Float_Data_16 is new Audio.Wavefiles.Gen_Float_IO
        (Audio_Res     => Wav_Int_16,
         PCM_Type      => PCM_Type,
         MC_Samples    => MC_Samples);

      package Float_Data_24 is new Audio.Wavefiles.Gen_Float_IO
        (Audio_Res     => Wav_Int_24,
         PCM_Type      => PCM_Type,
         MC_Samples    => MC_Samples);

      package Float_Data_32 is new Audio.Wavefiles.Gen_Float_IO
        (Audio_Res     => Wav_Int_32,
         PCM_Type      => PCM_Type,
         MC_Samples    => MC_Samples);
   begin
      if not WF.Is_Opened then
         raise Wavefile_Error;
      end if;

      if not Is_Supported_Format (WF.Wave_Format) then
         raise Wavefile_Unsupported;
      end if;

      case WF.Wave_Format.Bits_Per_Sample is
         when 16 =>
            Float_Data_16.Put (WF, PCM);
         when 24 =>
            Float_Data_24.Put (WF, PCM);
         when 32 =>
            Float_Data_32.Put (WF, PCM);
         when others =>
            raise Wavefile_Unsupported;
      end case;
   end Put_Float;


   procedure Put_Fixed
     (WF   : in out Wavefile;
      PCM  :        MC_Samples)
   is
      package Fixed_Data_16 is new Audio.Wavefiles.Gen_Fixed_IO
        (Audio_Res     => Wav_Int_16,
         PCM_Type      => PCM_Type,
         MC_Samples    => MC_Samples);

      package Fixed_Data_24 is new Audio.Wavefiles.Gen_Fixed_IO
        (Audio_Res     => Wav_Int_24,
         PCM_Type      => PCM_Type,
         MC_Samples    => MC_Samples);

      package Fixed_Data_32 is new Audio.Wavefiles.Gen_Fixed_IO
        (Audio_Res     => Wav_Int_32,
         PCM_Type      => PCM_Type,
         MC_Samples    => MC_Samples);
   begin
      if not WF.Is_Opened then
         raise Wavefile_Error;
      end if;

      if not Is_Supported_Format (WF.Wave_Format) then
         raise Wavefile_Unsupported;
      end if;

      case WF.Wave_Format.Bits_Per_Sample is
         when 16 =>
            Fixed_Data_16.Put (WF, PCM);
         when 24 =>
            Fixed_Data_24.Put (WF, PCM);
         when 32 =>
            Fixed_Data_32.Put (WF, PCM);
         when others =>
            raise Wavefile_Unsupported;
      end case;
   end Put_Fixed;

   procedure Close (WF  : in out Wavefile) is
      RIFF_Tag    : RIFF_Tag_Type;
      Size        : Unsigned_32;
   begin
      if not WF.Is_Opened then
         raise Wavefile_Error;
      end if;

      Size := Unsigned_32 (WF.Samples)
        * Unsigned_32 (WF.Wave_Format.Bits_Per_Sample / 8);

      --  Update/finalize RIFF chunk
      Ada.Streams.Stream_IO.Set_Index (WF.File, 1);
      RIFF_Tag.FOURCC := "RIFF";
      RIFF_Tag.Size   := Size + 36;
      RIFF_Tag_Type'Write (WF.File_Access, RIFF_Tag);

      --  Update/finalize RIFF tag of data chunk
      Ada.Streams.Stream_IO.Set_Index (WF.File, WF.File_Index);
      RIFF_Tag.FOURCC := "data";
      RIFF_Tag.Size   := Size;

      --  WF.Num_Samples is already multiplied by WF.Wave_Format.Channels
      RIFF_Tag_Type'Write (WF.File_Access, RIFF_Tag);

      Close (WF.File);

      WF.Is_Opened := False;
   end Close;

end Audio.Wavefiles.Write;
