-------------------------------------------------------------------------------
--
--                                WAVEFILES
--
--                              Main package
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

with System;                use System;
with Interfaces;            use Interfaces;
with Ada.Streams.Stream_IO;

package Audio.Wavefiles is

   type Wavefile is limited private;

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
         Sub_Format            : GUID;
      end record;

   procedure Set_Default (W : out Wave_Format_Extensible);

   type Wav_File_Mode is (In_File, Out_File);

   Wavefile_Error       : exception;
   Wavefile_Unsupported : exception;

   procedure Open
     (WF          : in out Wavefile;
      Mode        : Wav_File_Mode;
      File_Name   : String;
      Wave_Format : in out Wave_Format_Extensible);

   function Is_EOF
     (WF   : in out Wavefile) return Boolean
     with Inline, Pre => File_Mode (WF) = In_File;

   procedure Display_Info (WF : in Wavefile)
     with Pre => File_Mode (WF) = In_File;

   procedure Close (WF : in out Wavefile);

   function Format_Of_Wavefile
     (W : Wavefile) return Wave_Format_Extensible;

   function Number_Of_Channels
     (W : Wavefile) return Positive;

   function File_Mode
     (W : Wavefile) return Wav_File_Mode;

   function Is_Supported_Format
     (W : Wave_Format_Extensible) return Boolean;

private

   type Wav_Numeric_Data_Type is (Wav_Fixed_Data, Wav_Float_Data);

   type Wavefile is limited
      record
         Is_Opened        : Boolean      := False;
         File             : Ada.Streams.Stream_IO.File_Type;
         File_Access      : Ada.Streams.Stream_IO.Stream_Access;
         File_Index       : Ada.Streams.Stream_IO.Positive_Count;
         Wave_Format      : Wave_Format_Extensible;
         Samples          : Long_Integer;
         Samples_Read     : Long_Integer;
      end record;


end Audio.Wavefiles;
