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

with Ada.Streams;

package Audio.RIFF.Wav.Formats is

   type RIFF_Identifier is
     (RIFF_Identifier_Unknown,
      RIFF_Identifier_Standard,
      RIFF_Identifier_Big_Endian,
      RIFF_Identifier_List,
      RIFF_Identifier_RIFF_64,
      RIFF_Identifier_Broadcast_Wave_64);

   function To_RIFF_Identifier (FOURCC : FOURCC_String) return RIFF_Identifier;

   type RIFF_Format is
     (RIFF_Format_Unknown,
      RIFF_Format_Wave,
      RIFF_Format_AVI,      --  Audio/Video Interleaved
      RIFF_Format_MIDI,
      RIFF_Format_Pallete,
      RIFF_Format_DIB,      --  Device-Independent Bitmap
      RIFF_Format_MMP);     --  Multimedia Movie File

   function To_RIFF_Format (FOURCC : FOURCC_String) return RIFF_Format;

   type Wav_Chunk_Tag is
     (Wav_Chunk_Unknown,
      Wav_Chunk_Fmt,
      Wav_Chunk_Data,
      Wav_Chunk_Info,
      Wav_Chunk_Junk,
      Wav_Chunk_Pad,
      Wav_Chunk_Fact,
      Wav_Chunk_Cue,
      Wav_Chunk_Wave_List,
      Wav_Chunk_Silent,
      Wav_Chunk_Playlist,
      Wav_Chunk_Associated_Data_List,
      Wav_Chunk_Label,
      Wav_Chunk_Labeled_Text,
      Wav_Chunk_Note,
      Wav_Chunk_Sample,
      Wav_Chunk_Instrument,
      Wav_Chunk_Broadcast_Extension,
      Wav_Chunk_IXML,
      Wav_Chunk_AXML,
      Wav_Chunk_Quality,
      Wav_Chunk_MPEG_Audio_Extension,
      Wav_Chunk_Peak_Envelope,
      Wav_Chunk_Link,
      Wav_Chunk_Data_Size_64,
      Wav_Chunk_Compressed_Broadcast_Extension,
      Wav_Chunk_Sound_Related_XML,
      Wav_Chunk_Channel_Info);

   function To_Wav_Chunk_Tag (FOURCC : FOURCC_String) return Wav_Chunk_Tag;

   type Sub_GUID is array (1 .. 8) of Unsigned_8;

   type GUID is
      record
         Data_1 : Unsigned_32;
         Data_2 : Unsigned_16;
         Data_3 : Unsigned_16;
         Data_4 : Sub_GUID;
      end record;

   Channel_Configuration_Size : constant Integer := 32;

   type Speaker_Position is
     (Speaker_Front_Left,
      Speaker_Front_Right,
      Speaker_Front_Center,
      Speaker_Low_Frequency,
      Speaker_Back_Left,
      Speaker_Back_Right,
      Speaker_Front_Left_Of_Center,
      Speaker_Front_Right_Of_Center,
      Speaker_Back_Center,
      Speaker_Side_Left,
      Speaker_Side_Right,
      Speaker_Top_Center,
      Speaker_Top_Front_Left,
      Speaker_Top_Front_Center,
      Speaker_Top_Front_Right,
      Speaker_Top_Back_Left,
      Speaker_Top_Back_Center,
      Speaker_Top_Back_Right);

   type Channel_Configuration is array (Speaker_Position) of Boolean
     with
       Pack, Size => Channel_Configuration_Size,
       Read       => Read_Channel_Configuration,
       Write      => Write_Channel_Configuration;

   procedure Read_Channel_Configuration
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Channel_Configuration);

   procedure Write_Channel_Configuration
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Channel_Configuration);

   function Is_Consistent
     (Channel_Config      : Channel_Configuration;
      Number_Of_Channels  : Positive) return Boolean;

   type Wav_Sample_Rate is
     (Sample_Rate_8000,
      Sample_Rate_11025,  Sample_Rate_12000,  Sample_Rate_16000,
      Sample_Rate_22050,  Sample_Rate_24000,  Sample_Rate_32000,
      Sample_Rate_44100,  Sample_Rate_48000,  Sample_Rate_64000,
      Sample_Rate_88200,  Sample_Rate_96000,
      Sample_Rate_176400, Sample_Rate_192000,
      Sample_Rate_352800)
     with Size => Unsigned_32'Size;

   for Wav_Sample_Rate use
     (Sample_Rate_8000   =>   8_000,
      Sample_Rate_11025  =>  11_025,
      Sample_Rate_12000  =>  12_000,
      Sample_Rate_16000  =>  16_000,
      Sample_Rate_22050  =>  22_050,
      Sample_Rate_24000  =>  24_000,
      Sample_Rate_32000  =>  32_000,
      Sample_Rate_44100  =>  44_100,
      Sample_Rate_48000  =>  48_000,
      Sample_Rate_64000  =>  64_000,
      Sample_Rate_88200  =>  88_200,
      Sample_Rate_96000  =>  96_000,
      Sample_Rate_176400 => 176_400,
      Sample_Rate_192000 => 192_000,
      Sample_Rate_352800 => 352_800);

   function To_Positive (SR : Wav_Sample_Rate) return Positive is
     (Positive (Wav_Sample_Rate'Enum_Rep (SR)));

   function To_Unsigned_32 (SR : Wav_Sample_Rate) return Unsigned_32 is
     (Wav_Sample_Rate'Enum_Rep (SR));

   function To_Float (SR : Wav_Sample_Rate) return Float is
     (Float (Wav_Sample_Rate'Enum_Rep (SR)));

   type Wav_Bit_Depth is
     (Bit_Depth_8, Bit_Depth_16, Bit_Depth_24, Bit_Depth_32, Bit_Depth_64)
     with Size => Unsigned_16'Size;

   for Wav_Bit_Depth use
     (Bit_Depth_8  =>  8,
      Bit_Depth_16 => 16,
      Bit_Depth_24 => 24,
      Bit_Depth_32 => 32,
      Bit_Depth_64 => 64);

   function To_Positive (B : Wav_Bit_Depth) return Positive is
     (Positive (Wav_Bit_Depth'Enum_Rep (B)));

   function To_Unsigned_16 (B : Wav_Bit_Depth) return Unsigned_16 is
     (Wav_Bit_Depth'Enum_Rep (B));

   type Wav_Format_Tag is
     (Wav_Format_Unknown,
      Wav_Format_PCM,
      Wav_Format_ADPCM,
      Wav_Format_IEEE_Float,
      Wav_Format_VSELP,
      Wav_Format_IBM_CVSD,
      Wav_Format_A_Law,
      Wav_Format_Mu_Law,
      Wav_Format_OKI_ADPCM,
      Wav_Format_DVI_ADPCM,
      Wav_Format_Mediaspace_ADPCM,
      Wav_Format_Sierra_ADPCM,
      Wav_Format_G723_ADPCM,
      Wav_Format_DIGI_STD,
      Wav_Format_DIGI_FIX,
      Wav_Format_Dialogic_OKI_ADPCM,
      Wav_Format_Mediavision_ADPCM,
      Wav_Format_CU_Codec,
      Wav_Format_Yamaha_ADPCM,
      Wav_Format_SONARC,
      Wav_Format_DSP_Truespeech,
      Wav_Format_Echo_SC1,
      Wav_Format_Audiofile_AF36,
      Wav_Format_APTX,
      Wav_Format_Audiofile_AF10,
      Wav_Format_Prosody_1612,
      Wav_Format_LRC,
      Wav_Format_Dolby_AC2,
      Wav_Format_GSM610,
      Wav_Format_MSN_Audio,
      Wav_Format_Antex_ADPCME,
      Wav_Format_Control_Res_VQLPC,
      Wav_Format_DIGI_Real,
      Wav_Format_DIGI_ADPCM,
      Wav_Format_Control_Res_CR10,
      Wav_Format_NMS_VBXADPCM,
      Wav_Format_Roland_RDAC,
      Wav_Format_Echo_SC3,
      Wav_Format_Rockwell_ADPCM,
      Wav_Format_Rockwell_Digitalk,
      Wav_Format_XEBEC,
      Wav_Format_G721_ADPCM,
      Wav_Format_G728_CELP,
      Wav_Format_MSG723,
      Wav_Format_MPEG,
      Wav_Format_RT24,
      Wav_Format_PAC,
      Wav_Format_MPEG_Layer_3,
      Wav_Format_Lucent_G723,
      Wav_Format_Cirrus,
      Wav_Format_ESPCM,
      Wav_Format_Voxware,
      Wav_Format_Canopus_ATRAC,
      Wav_Format_G726_ADPCM,
      Wav_Format_G722_ADPCM,
      Wav_Format_DSAT,
      Wav_Format_DSAT_Display,
      Wav_Format_Voxware_Byte_Aligned,
      Wav_Format_Voxware_AC8,
      Wav_Format_Voxware_AC10,
      Wav_Format_Voxware_AC16,
      Wav_Format_Voxware_AC20,
      Wav_Format_Voxware_RT24,
      Wav_Format_Voxware_RT29,
      Wav_Format_Voxware_RT29HW,
      Wav_Format_Voxware_VR12,
      Wav_Format_Voxware_VR18,
      Wav_Format_Voxware_TQ40,
      Wav_Format_Softsound,
      Wav_Format_Voxware_TQ60,
      Wav_Format_MSRT24,
      Wav_Format_G729A,
      Wav_Format_MVI_MV12,
      Wav_Format_DF_G726,
      Wav_Format_DF_GSM610,
      Wav_Format_Onlive,
      Wav_Format_SBC24,
      Wav_Format_Dolby_AC3_SPDIF,
      Wav_Format_Zyxel_ADPCM,
      Wav_Format_Philips_LPCBB,
      Wav_Format_Packed,
      Wav_Format_Rhetorex_ADPCM,
      Wav_Format_IBM_A_Law,
      Wav_Format_IBM_Mu_Law,
      Wav_Format_IBM_ADPCM,
      Wav_Format_Vivo_G723,
      Wav_Format_Vivo_Siren,
      Wav_Format_Digital_G723,
      Wav_Format_Creative_ADPCM,
      Wav_Format_Creative_Fastspeech8,
      Wav_Format_Creative_Fastspeech10,
      Wav_Format_Quarterdeck,
      Wav_Format_FM_Towns_SND,
      Wav_Format_BZV_Digital,
      Wav_Format_VME_VMPCM,
      Wav_Format_OLIGSM,
      Wav_Format_OLIADPCM,
      Wav_Format_OLICELP,
      Wav_Format_OLISBC,
      Wav_Format_OLIOPR,
      Wav_Format_LH_CODEC,
      Wav_Format_Norris,
      Wav_Format_Soundspace_Musicompress,
      Wav_Format_DVM,
      Wav_Format_Interwav_VSC112,
      Wav_Format_Extensible)
     with Size => Unsigned_16'Size;

   for Wav_Format_Tag use
     (Wav_Format_Unknown                    => 16#0000#,
      Wav_Format_PCM                        => 16#0001#,
      Wav_Format_ADPCM                      => 16#0002#,
      Wav_Format_IEEE_Float                 => 16#0003#,
      Wav_Format_VSELP                      => 16#0004#,
      Wav_Format_IBM_CVSD                   => 16#0005#,
      Wav_Format_A_Law                      => 16#0006#,
      Wav_Format_Mu_Law                     => 16#0007#,
      Wav_Format_OKI_ADPCM                  => 16#0010#,
      Wav_Format_DVI_ADPCM                  => 16#0011#,
      Wav_Format_Mediaspace_ADPCM           => 16#0012#,
      Wav_Format_Sierra_ADPCM               => 16#0013#,
      Wav_Format_G723_ADPCM                 => 16#0014#,
      Wav_Format_DIGI_STD                   => 16#0015#,
      Wav_Format_DIGI_FIX                   => 16#0016#,
      Wav_Format_Dialogic_OKI_ADPCM         => 16#0017#,
      Wav_Format_Mediavision_ADPCM          => 16#0018#,
      Wav_Format_CU_Codec                   => 16#0019#,
      Wav_Format_Yamaha_ADPCM               => 16#0020#,
      Wav_Format_SONARC                     => 16#0021#,
      Wav_Format_DSP_Truespeech             => 16#0022#,
      Wav_Format_Echo_SC1                   => 16#0023#,
      Wav_Format_Audiofile_AF36             => 16#0024#,
      Wav_Format_APTX                       => 16#0025#,
      Wav_Format_Audiofile_AF10             => 16#0026#,
      Wav_Format_Prosody_1612               => 16#0027#,
      Wav_Format_LRC                        => 16#0028#,
      Wav_Format_Dolby_AC2                  => 16#0030#,
      Wav_Format_GSM610                     => 16#0031#,
      Wav_Format_MSN_Audio                  => 16#0032#,
      Wav_Format_Antex_ADPCME               => 16#0033#,
      Wav_Format_Control_Res_VQLPC          => 16#0034#,
      Wav_Format_DIGI_Real                  => 16#0035#,
      Wav_Format_DIGI_ADPCM                 => 16#0036#,
      Wav_Format_Control_Res_CR10           => 16#0037#,
      Wav_Format_NMS_VBXADPCM               => 16#0038#,
      Wav_Format_Roland_RDAC                => 16#0039#,
      Wav_Format_Echo_SC3                   => 16#003A#,
      Wav_Format_Rockwell_ADPCM             => 16#003B#,
      Wav_Format_Rockwell_Digitalk          => 16#003C#,
      Wav_Format_XEBEC                      => 16#003D#,
      Wav_Format_G721_ADPCM                 => 16#0040#,
      Wav_Format_G728_CELP                  => 16#0041#,
      Wav_Format_MSG723                     => 16#0042#,
      Wav_Format_MPEG                       => 16#0050#,
      Wav_Format_RT24                       => 16#0052#,
      Wav_Format_PAC                        => 16#0053#,
      Wav_Format_MPEG_Layer_3               => 16#0055#,
      Wav_Format_Lucent_G723                => 16#0059#,
      Wav_Format_Cirrus                     => 16#0060#,
      Wav_Format_ESPCM                      => 16#0061#,
      Wav_Format_Voxware                    => 16#0062#,
      Wav_Format_Canopus_ATRAC              => 16#0063#,
      Wav_Format_G726_ADPCM                 => 16#0064#,
      Wav_Format_G722_ADPCM                 => 16#0065#,
      Wav_Format_DSAT                       => 16#0066#,
      Wav_Format_DSAT_Display               => 16#0067#,
      Wav_Format_Voxware_Byte_Aligned       => 16#0069#,
      Wav_Format_Voxware_AC8                => 16#0070#,
      Wav_Format_Voxware_AC10               => 16#0071#,
      Wav_Format_Voxware_AC16               => 16#0072#,
      Wav_Format_Voxware_AC20               => 16#0073#,
      Wav_Format_Voxware_RT24               => 16#0074#,
      Wav_Format_Voxware_RT29               => 16#0075#,
      Wav_Format_Voxware_RT29HW             => 16#0076#,
      Wav_Format_Voxware_VR12               => 16#0077#,
      Wav_Format_Voxware_VR18               => 16#0078#,
      Wav_Format_Voxware_TQ40               => 16#0079#,
      Wav_Format_Softsound                  => 16#0080#,
      Wav_Format_Voxware_TQ60               => 16#0081#,
      Wav_Format_MSRT24                     => 16#0082#,
      Wav_Format_G729A                      => 16#0083#,
      Wav_Format_MVI_MV12                   => 16#0084#,
      Wav_Format_DF_G726                    => 16#0085#,
      Wav_Format_DF_GSM610                  => 16#0086#,
      Wav_Format_Onlive                     => 16#0089#,
      Wav_Format_SBC24                      => 16#0091#,
      Wav_Format_Dolby_AC3_SPDIF            => 16#0092#,
      Wav_Format_Zyxel_ADPCM                => 16#0097#,
      Wav_Format_Philips_LPCBB              => 16#0098#,
      Wav_Format_Packed                     => 16#0099#,
      Wav_Format_Rhetorex_ADPCM             => 16#0100#,
      Wav_Format_IBM_A_Law                  => 16#0101#,
      Wav_Format_IBM_Mu_Law                 => 16#0102#,
      Wav_Format_IBM_ADPCM                  => 16#0103#,
      Wav_Format_Vivo_G723                  => 16#0111#,
      Wav_Format_Vivo_Siren                 => 16#0112#,
      Wav_Format_Digital_G723               => 16#0123#,
      Wav_Format_Creative_ADPCM             => 16#0200#,
      Wav_Format_Creative_Fastspeech8       => 16#0202#,
      Wav_Format_Creative_Fastspeech10      => 16#0203#,
      Wav_Format_Quarterdeck                => 16#0220#,
      Wav_Format_FM_Towns_SND               => 16#0300#,
      Wav_Format_BZV_Digital                => 16#0400#,
      Wav_Format_VME_VMPCM                  => 16#0680#,
      Wav_Format_OLIGSM                     => 16#1000#,
      Wav_Format_OLIADPCM                   => 16#1001#,
      Wav_Format_OLICELP                    => 16#1002#,
      Wav_Format_OLISBC                     => 16#1003#,
      Wav_Format_OLIOPR                     => 16#1004#,
      Wav_Format_LH_CODEC                   => 16#1100#,
      Wav_Format_Norris                     => 16#1400#,
      Wav_Format_Soundspace_Musicompress    => 16#1500#,
      Wav_Format_DVM                        => 16#2000#,
      Wav_Format_Interwav_VSC112            => 16#7150#,
      Wav_Format_Extensible                 => 16#FFFE#);

   function To_GUID (Format : Wav_Format_Tag) return GUID;

   function To_Wav_Format_Tag (ID : GUID) return Wav_Format_Tag;

   type Wave_Format_16 is tagged
      record
         Format_Tag        : Wav_Format_Tag;
         Channels          : Unsigned_16;
         Samples_Per_Sec   : Wav_Sample_Rate;
         Avg_Bytes_Per_Sec : Unsigned_32;
         Block_Align       : Unsigned_16;
         Bits_Per_Sample   : Wav_Bit_Depth;
      end record;

   function Default return Wave_Format_16;

   type Wave_Format_18 is new Wave_Format_16 with
      record
         Size              : Unsigned_16 := 0;
      end record;

   overriding function Default return Wave_Format_18;

   type Wave_Format_Extensible is new Wave_Format_18 with
      record
         Valid_Bits_Per_Sample : Unsigned_16;
         Channel_Config        : Channel_Configuration;
         Sub_Format            : GUID;
      end record;

   overriding function Default return Wave_Format_Extensible;

   procedure Reset_For_Wave_Format_16 (W : in out Wave_Format_Extensible);

   procedure Reset_For_Wave_Format_18 (W : in out Wave_Format_Extensible);

   function Is_Float_Format
     (W : Wave_Format_Extensible) return Boolean;

   function Should_Use_Extensible_Format
     (Bit_Depth          : Wav_Bit_Depth;
      Number_Of_Channels : Positive) return Boolean;

   function Block_Align
     (Bit_Depth          : Wav_Bit_Depth;
      Number_Of_Channels : Positive) return Unsigned_16;

   function Average_Bytes_Per_Second
     (Block_Align        : Unsigned_16;
      Sample_Rate        : Wav_Sample_Rate) return Unsigned_32;

   function Init
     (Bit_Depth          : Wav_Bit_Depth;
      Sample_Rate        : Wav_Sample_Rate;
      Number_Of_Channels : Positive;
      Use_Float          : Boolean := False) return Wave_Format_Extensible;

   type Wave_Format_Chunk_Size is
     (Wave_Format_16_Size,
      Wave_Format_18_Size,
      Wave_Format_Extensible_Size);

   for Wave_Format_Chunk_Size use
     (Wave_Format_16_Size         => 16,
      Wave_Format_18_Size         => 18,
      Wave_Format_Extensible_Size => 40);

end Audio.RIFF.Wav.Formats;
