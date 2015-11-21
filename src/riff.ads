-------------------------------------------------------------------------------
--
--                                WAVEFILES
--
--                              WAV RIFF data
--
-- The MIT License (MIT)
--
-- Copyright (c) 2015 Gustavo A. Hoffmann
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to
-- deal in the Software without restriction, including without limitation the
-- rights to use, copy, modify, merge, publish, distribute, sublicense, and /
-- or sell copies of the Software, and to permit persons to whom the Software
-- is furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
-- IN THE SOFTWARE.
-------------------------------------------------------------------------------

with Interfaces;
with Ada.Streams;
with System;

--  <description>
--     RIFF information for wavefile parsing.
--     This implementation partially supports WAVE_FORMAT_EXTENSIBLE.
--  </description>
package RIFF is

   use Interfaces;

   type RIFF_Tag_Type is
      record
         FOURCC  : String (1 .. 4);
         Size    : Unsigned_32;
      end record;

   type RIFF_Chunk_Type is
      record
         FOURCC  : String (1 .. 4);
      end record;

   type Wave_Format_Chunk_Size is (Wave_Format_16_Size,
                                   Wave_Format_18_Size,
                                   Wave_Format_Extensible_Size);

   for Wave_Format_Chunk_Size use (Wave_Format_16_Size         => 16,
                                   Wave_Format_18_Size         => 18,
                                   Wave_Format_Extensible_Size => 40);

   type Sub_GUID is array (1 .. 8) of Unsigned_8;

   type GUID is
      record
         Data_1 : Unsigned_32;
         Data_2 : Unsigned_16;
         Data_3 : Unsigned_16;
         Data_4 : Sub_GUID;
      end record;

   GUID_Undefined : constant GUID := (16#00000000#,
                                      16#0000#,
                                      16#0000#,
                                      (16#00#, 16#00#,
                                       16#00#, 16#00#,
                                       16#00#, 16#00#,
                                       16#00#, 16#00#));
   --  Undefined GUID

   GUID_PCM : constant GUID := (16#00000001#,
                                16#0000#,
                                16#0010#,
                                (16#80#, 16#00#,
                                 16#00#, 16#aa#,
                                 16#00#, 16#38#,
                                 16#9b#, 16#71#));
   --  CEA 861 0x01: KSDATAFORMAT_SUBTYPE_PCM (IEC 60958 PCM)

   GUID_IEEE_Float : constant GUID := (16#00000003#,
                                16#0000#,
                                16#0010#,
                                (16#80#, 16#00#,
                                 16#00#, 16#aa#,
                                 16#00#, 16#38#,
                                 16#9b#, 16#71#));
   --  CEA 861 0x01: KSDATAFORMAT_SUBTYPE_IEEE_FLOAT (IEEE Floating-Point PCM)

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
       Read       => Read,
       Write      => Write,
       Bit_Order  => System.Low_Order_First;

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Channel_Mask_Type);

   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Channel_Mask_Type);

   type Wave_Format_16 is tagged
      record
         Format_Tag        : Unsigned_16;
         Channels          : Unsigned_16;
         Samples_Per_Sec   : Unsigned_32;
         Avg_Bytes_Per_Sec : Unsigned_32;
         Block_Align       : Unsigned_16;
         Bits_Per_Sample   : Unsigned_16;
      end record;

   procedure Set_Default (W : out Wave_Format_16);

   type Wave_Format_18 is new Wave_Format_16 with
      record
         Size              : Unsigned_16 := 0;
      end record;

   procedure Set_Default (W : out Wave_Format_18);

   type Wave_Format_Extensible is new Wave_Format_18 with
      record
         Valid_Bits_Per_Sample : Unsigned_16 := 0;
         Channel_Mask          : Channel_Mask_Type;
         Sub_Format            : GUID := GUID_Undefined;
      end record;

   procedure Set_Default (W : out Wave_Format_Extensible);

   procedure Print (W : Wave_Format_Extensible);

end RIFF;
