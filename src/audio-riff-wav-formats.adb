------------------------------------------------------------------------------
--                                                                          --
--                         AUDIO / RIFF / WAV                               --
--                                                                          --
--                  RIFF format information for wavefiles                   --
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

with Audio.RIFF.Wav.GUIDs;
with Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;

package body Audio.RIFF.Wav.Formats is

   type Channel_Configuration_Integer is mod 2 ** Channel_Configuration_Size;

   ------------------------
   -- To_RIFF_Identifier --
   ------------------------

   function To_RIFF_Identifier (FOURCC : FOURCC_String) return RIFF_Identifier
   is
   begin
      if    FOURCC = "RIFF" then return RIFF_Identifier_Standard;
      elsif FOURCC = "RIFX" then return RIFF_Identifier_Big_Endian;
      elsif FOURCC = "LIST" then return RIFF_Identifier_List;
      elsif FOURCC = "RF64" then return RIFF_Identifier_RIFF_64;
      elsif FOURCC = "BW64" then return RIFF_Identifier_Broadcast_Wave_64;
      else                       return RIFF_Identifier_Unknown;
      end if;
   end To_RIFF_Identifier;

   --------------------
   -- To_RIFF_Format --
   --------------------

   function To_RIFF_Format (FOURCC : FOURCC_String) return RIFF_Format is
   begin
      if    FOURCC = "WAVE" then return RIFF_Format_Wave;
      elsif FOURCC = "AVI " then return RIFF_Format_AVI;
      elsif FOURCC = "MIDI" then return RIFF_Format_MIDI;
      elsif FOURCC = "PAL " then return RIFF_Format_Pallete;
      elsif FOURCC = "RDIB" then return RIFF_Format_DIB;
      elsif FOURCC = "RMMP" then return RIFF_Format_MMP;
      else                       return RIFF_Format_Unknown;
      end if;
   end To_RIFF_Format;

   ----------------------
   -- To_Wav_Chunk_Tag --
   ----------------------

   function To_Wav_Chunk_Tag (FOURCC : FOURCC_String) return Wav_Chunk_Tag is
   begin
      if    FOURCC = "fmt " then return Wav_Chunk_Fmt;
      elsif FOURCC = "data" then return Wav_Chunk_Data;
      elsif FOURCC = "INFO" then return Wav_Chunk_Info;
      elsif FOURCC = "JUNK" then return Wav_Chunk_Junk;
      elsif FOURCC = "PAD " then return Wav_Chunk_Pad;
      elsif FOURCC = "fact" then return Wav_Chunk_Fact;
      elsif FOURCC = "cue " then return Wav_Chunk_Cue;
      elsif FOURCC = "wavl" then return Wav_Chunk_Wave_List;
      elsif FOURCC = "slnt" then return Wav_Chunk_Silent;
      elsif FOURCC = "plst" then return Wav_Chunk_Playlist;
      elsif FOURCC = "list" then return Wav_Chunk_Associated_Data_List;
      elsif FOURCC = "labl" then return Wav_Chunk_Label;
      elsif FOURCC = "ltxt" then return Wav_Chunk_Labeled_Text;
      elsif FOURCC = "note" then return Wav_Chunk_Note;
      elsif FOURCC = "smpl" then return Wav_Chunk_Sample;
      elsif FOURCC = "inst" then return Wav_Chunk_Instrument;
      elsif FOURCC = "bext" then return Wav_Chunk_Broadcast_Extension;
      elsif FOURCC = "iXML" then return Wav_Chunk_IXML;
      elsif FOURCC = "axml" then return Wav_Chunk_AXML;
      elsif FOURCC = "qlty" then return Wav_Chunk_Quality;
      elsif FOURCC = "mext" then return Wav_Chunk_MPEG_Audio_Extension;
      elsif FOURCC = "levl" then return Wav_Chunk_Peak_Envelope;
      elsif FOURCC = "link" then return Wav_Chunk_Link;
      elsif FOURCC = "ds64" then return Wav_Chunk_Data_Size_64;
      elsif FOURCC = "bxml" then return
           Wav_Chunk_Compressed_Broadcast_Extension;
      elsif FOURCC = "sxml" then return Wav_Chunk_Sound_Related_XML;
      elsif FOURCC = "chna" then return Wav_Chunk_Channel_Info;
      else                       return Wav_Chunk_Unknown;
      end if;
   end To_Wav_Chunk_Tag;

   -------------
   -- Default --
   -------------

   function Default return Wave_Format_16 is
   begin
      return W : Wave_Format_16 do
         W.Format_Tag        := Wav_Format_PCM;
         W.Channels          := 2;
         W.Samples_Per_Sec   := Sample_Rate_44100;
         W.Bits_Per_Sample   := Bit_Depth_16;
         W.Block_Align       := ((To_Unsigned_16 (W.Bits_Per_Sample) + 7)
                                 / 8) * W.Channels;
         W.Avg_Bytes_Per_Sec := 0;
      end return;
   end Default;

   -------------
   -- Default --
   -------------

   overriding function Default return Wave_Format_18 is
   begin
      return W : Wave_Format_18 do
         Wave_Format_16 (W) := Default;
         W.Size := 0;
      end return;
   end Default;

   -------------
   -- Default --
   -------------

   overriding function Default return Wave_Format_Extensible is
      use Audio.RIFF.Wav.GUIDs;
      use Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;
   begin
      return W : Wave_Format_Extensible do
         Wave_Format_18 (W)      := Default;
         W.Size                  := 22;
         W.Valid_Bits_Per_Sample := To_Unsigned_16 (W.Bits_Per_Sample);
         W.Sub_Format            := GUID_Undefined;
         W.Channel_Config        := Channel_Config_2_0;
      end return;
   end Default;

   ------------------------------
   -- Reset_For_Wave_Format_16 --
   ------------------------------

   procedure Reset_For_Wave_Format_16 (W : in out Wave_Format_Extensible) is
   begin
      W.Size := 0;
      Reset_For_Wave_Format_18 (W);
   end Reset_For_Wave_Format_16;

   ------------------------------
   -- Reset_For_Wave_Format_18 --
   ------------------------------

   procedure Reset_For_Wave_Format_18 (W : in out Wave_Format_Extensible) is
      use Audio.RIFF.Wav.GUIDs;
      use Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;
   begin
      W.Valid_Bits_Per_Sample := To_Unsigned_16 (W.Bits_Per_Sample);
      W.Sub_Format            := GUID_Undefined;
      W.Channel_Config        := Channel_Config_Empty;
   end Reset_For_Wave_Format_18;

   -------------
   -- To_GUID --
   -------------

   function To_GUID (Format : Wav_Format_Tag) return GUID is
      use Audio.RIFF.Wav.GUIDs;
   begin
      case Format is
         when Wav_Format_PCM             => return GUID_PCM;
         when Wav_Format_IEEE_Float      => return GUID_IEEE_Float;
         when Wav_Format_A_Law           => return GUID_ALAW;
         when Wav_Format_Mu_Law          => return GUID_MULAW;
         when Wav_Format_ADPCM           => return GUID_ADPCM;
         when Wav_Format_MPEG            => return GUID_MPEG;
         when Wav_Format_Dolby_AC3_SPDIF => return GUID_DOLBY_AC3_SPDIF;
         when Wav_Format_MPEG_Layer_3    => return GUID_MPEG_LAYER_3;
         when others                     => return GUID_Undefined;
      end case;
   end To_GUID;

   -----------------------
   -- To_Wav_Format_Tag --
   -----------------------

   function To_Wav_Format_Tag (ID : GUID) return Wav_Format_Tag is
      use Audio.RIFF.Wav.GUIDs;
   begin
      if ID = GUID_Undefined then
         return Wav_Format_Unknown;
      elsif ID = GUID_PCM then
         return Wav_Format_PCM;
      elsif ID = GUID_IEEE_Float then
         return Wav_Format_IEEE_Float;
      --  elsif ID = GUID_DRM then
      --     return Wav_Format_Unknown;
      elsif ID = GUID_ALAW then
         return Wav_Format_A_Law;
      elsif ID = GUID_MULAW then
         return Wav_Format_Mu_Law;
      elsif ID = GUID_ADPCM then
         return Wav_Format_ADPCM;
      elsif ID = GUID_MPEG then
         return Wav_Format_MPEG;
      elsif ID = GUID_DOLBY_AC3_SPDIF then
         return Wav_Format_Dolby_AC3_SPDIF;
      --  elsif ID = GUID_WMA_SPDIF then
      --     return Wav_Format_Unknown;
      --  elsif ID = GUID_DTS then
      --     return Wav_Format_Unknown;
      elsif ID = GUID_MPEG_LAYER_3 then
         return Wav_Format_MPEG_Layer_3;
      --  elsif ID = GUID_MPEG_HE_AAC then
      --     return Wav_Format_Unknown
      --  elsif ID = GUID_WMA_2 then
      --     return Wav_Format_Unknown;
      --  elsif ID = GUID_WMA_3 then
      --     return Wav_Format_Unknown;
      --  elsif ID = GUID_WMA_LOSSLESS then
      --     return Wav_Format_Unknown;
      else
         return Wav_Format_Unknown;
      end if;
   end To_Wav_Format_Tag;

   -------------------
   -- Is_Consistent --
   -------------------

   function Is_Consistent
     (Channel_Config      : Channel_Configuration;
      Number_Of_Channels  : Positive) return Boolean
   is
      Counted_Channels : Natural := 0;
   begin
      for Channel_Is_Active of Channel_Config loop
         if Channel_Is_Active then
            Counted_Channels := Counted_Channels + 1;
         end if;
      end loop;

      return Number_Of_Channels = Counted_Channels;
   end Is_Consistent;

   ----------------------------------
   -- Should_Use_Extensible_Format --
   ----------------------------------

   function Should_Use_Extensible_Format
     (Bit_Depth          : Wav_Bit_Depth;
      Number_Of_Channels : Positive) return Boolean is
   begin
      if Bit_Depth not in Bit_Depth_8 | Bit_Depth_16 then
         return True;
      elsif Number_Of_Channels > 2 then
         return True;
      else
         return False;
      end if;
   end Should_Use_Extensible_Format;

   -----------------
   -- Block_Align --
   -----------------

   function Block_Align
     (Bit_Depth          : Wav_Bit_Depth;
      Number_Of_Channels : Positive) return Unsigned_16 is
     (((To_Unsigned_16 (Bit_Depth) + 7) / 8)
      * Unsigned_16 (Number_Of_Channels));

   ------------------------------
   -- Average_Bytes_Per_Second --
   ------------------------------

   function Average_Bytes_Per_Second
     (Block_Align        : Unsigned_16;
      Sample_Rate        : Wav_Sample_Rate) return Unsigned_32 is
     (Unsigned_32 (Block_Align) * To_Unsigned_32 (Sample_Rate));

   ----------
   -- Init --
   ----------

   function Init
     (Bit_Depth          : Wav_Bit_Depth;
      Sample_Rate        : Wav_Sample_Rate;
      Number_Of_Channels : Positive;
      Use_Float          : Boolean := False) return Wave_Format_Extensible
   is
      Format             : constant Wav_Format_Tag
        := (if Use_Float then Wav_Format_IEEE_Float else Wav_Format_PCM);
      Use_Wav_Extensible : constant Boolean
        := Should_Use_Extensible_Format (Bit_Depth, Number_Of_Channels);

      use Audio.RIFF.Wav.GUIDs;
      use Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;
   begin
      return W : Wave_Format_Extensible do
         W.Channels          := Unsigned_16 (Number_Of_Channels);
         W.Samples_Per_Sec   := Sample_Rate;
         W.Bits_Per_Sample   := Bit_Depth;
         W.Block_Align       := Block_Align (Bit_Depth, Number_Of_Channels);
         W.Avg_Bytes_Per_Sec := Average_Bytes_Per_Second (W.Block_Align,
                                                          Sample_Rate);

         if not Use_Wav_Extensible then
            W.Format_Tag            := Format;
            W.Size                  := 0;
            W.Valid_Bits_Per_Sample := 0;
            W.Sub_Format            := GUID_Undefined;
            W.Channel_Config        := Channel_Config_Empty;
         else
            W.Size                  := 22;
            W.Valid_Bits_Per_Sample := To_Unsigned_16 (W.Bits_Per_Sample);
            W.Channel_Config        := Channel_Config_Empty;

            Init_Formats : declare
               Sub_Format : constant GUID := To_GUID (Format);
            begin
               if Sub_Format /= GUID_Undefined then
                  W.Format_Tag      := Wav_Format_Extensible;
                  W.Sub_Format      := Sub_Format;
               else
                  W.Format_Tag      := Format;
                  W.Sub_Format      := GUID_Undefined;
               end if;
            end Init_Formats;
         end if;
      end return;
   end Init;

   ---------------------
   -- Is_Float_Format --
   ---------------------

   function Is_Float_Format
     (W : Wave_Format_Extensible) return Boolean
   is
      use Audio.RIFF.Wav.GUIDs;
   begin
      return W.Format_Tag = Wav_Format_IEEE_Float or
        W.Sub_Format = GUID_IEEE_Float;
   end Is_Float_Format;

   --------------------------------
   -- Read_Channel_Configuration --
   --------------------------------

   procedure Read_Channel_Configuration
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Channel_Configuration)
   is
      V : Channel_Configuration_Integer;
      X : Channel_Configuration;
      for X'Address use V'Address;
   begin
      Channel_Configuration_Integer'Read (Stream, V);
      Item := X;
   end Read_Channel_Configuration;

   ---------------------------------
   -- Write_Channel_Configuration --
   ---------------------------------

   procedure Write_Channel_Configuration
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Channel_Configuration)
   is
      V : Channel_Configuration_Integer;
      X : Channel_Configuration;
      for X'Address use V'Address;
   begin
      X := Item;
      Channel_Configuration_Integer'Write (Stream, V);
   end Write_Channel_Configuration;

end Audio.RIFF.Wav.Formats;
