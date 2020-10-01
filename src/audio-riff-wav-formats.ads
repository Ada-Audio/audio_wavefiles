-------------------------------------------------------------------------------
--
--                           WAVEFILE DEFINITIONS
--
--                              Wave Formats
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

with Ada.Streams;
with System;

package Audio.RIFF.Wav.Formats is

   type Sub_GUID is array (1 .. 8) of Unsigned_8;

   type GUID is
      record
         Data_1 : Unsigned_32;
         Data_2 : Unsigned_16;
         Data_3 : Unsigned_16;
         Data_4 : Sub_GUID;
      end record;

   type Channel_Mask_Bit is new Boolean with Size => 1;

   Channel_Mask_Type_Size : constant Integer := 32;

   type Channel_Mask_Type is
      record
         Speaker_Front_Left            : Channel_Mask_Bit;
         Speaker_Front_Right           : Channel_Mask_Bit;
         Speaker_Front_Center          : Channel_Mask_Bit;
         Speaker_Low_Frequency         : Channel_Mask_Bit;
         Speaker_Back_Left             : Channel_Mask_Bit;
         Speaker_Back_Right            : Channel_Mask_Bit;
         Speaker_Front_Left_Of_Center  : Channel_Mask_Bit;
         Speaker_Front_Right_Of_Center : Channel_Mask_Bit;
         Speaker_Back_Center           : Channel_Mask_Bit;
         Speaker_Side_Left             : Channel_Mask_Bit;
         Speaker_Side_Right            : Channel_Mask_Bit;
         Speaker_Top_Center            : Channel_Mask_Bit;
         Speaker_Top_Front_Left        : Channel_Mask_Bit;
         Speaker_Top_Front_Center      : Channel_Mask_Bit;
         Speaker_Top_Front_Right       : Channel_Mask_Bit;
         Speaker_Top_Back_Left         : Channel_Mask_Bit;
         Speaker_Top_Back_Center       : Channel_Mask_Bit;
         Speaker_Top_Back_Right        : Channel_Mask_Bit;
      end record
     with
       Pack, Size => Channel_Mask_Type_Size,
       Read       => Read_Channel_Mask,
       Write      => Write_Channel_Mask,
       Bit_Order  => System.Low_Order_First;

   procedure Read_Channel_Mask
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Channel_Mask_Type);

   procedure Write_Channel_Mask
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Channel_Mask_Type);

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

   type Wave_Format_16 is tagged
      record
         Format_Tag        : Unsigned_16;
         Channels          : Unsigned_16;
         Samples_Per_Sec   : Wav_Sample_Rate;
         Avg_Bytes_Per_Sec : Unsigned_32;
         Block_Align       : Unsigned_16;
         Bits_Per_Sample   : Unsigned_16;
      end record;

   function Default return Wave_Format_16;

   type Wave_Format_18 is new Wave_Format_16 with
      record
         Size              : Unsigned_16 := 0;
      end record;

   function Default return Wave_Format_18;

   type Wave_Format_Extensible is new Wave_Format_18 with
      record
         Valid_Bits_Per_Sample : Unsigned_16;
         Channel_Mask          : Channel_Mask_Type;
         Sub_Format            : GUID;
      end record;

   function Default return Wave_Format_Extensible;

   function Is_Float_Format
     (W : Wave_Format_Extensible) return Boolean;

   type Wave_Format_Chunk_Size is
     (Wave_Format_16_Size,
      Wave_Format_18_Size,
      Wave_Format_Extensible_Size);

   for Wave_Format_Chunk_Size use
     (Wave_Format_16_Size         => 16,
      Wave_Format_18_Size         => 18,
      Wave_Format_Extensible_Size => 40);

end Audio.RIFF.Wav.Formats;
