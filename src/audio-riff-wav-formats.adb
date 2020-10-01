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

with Audio.RIFF.Wav.GUIDs;

package body Audio.RIFF.Wav.Formats is

   type Channel_Mask_Integer is mod 2**Channel_Mask_Type_Size;

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

   function Default return Wave_Format_18 is
   begin
      return W : Wave_Format_18 do
         Wave_Format_16 (W) := Default;
         W.Size := 0;
      end return;
   end Default;

   function Default return Wave_Format_Extensible is
      use Audio.RIFF.Wav.GUIDs;
   begin
      return W : Wave_Format_Extensible do
         Wave_Format_18 (W)      := Default;
         W.Size                  := 22;
         W.Valid_Bits_Per_Sample := To_Unsigned_16 (W.Bits_Per_Sample);
         W.Sub_Format            := GUID_Undefined;
         W.Channel_Mask          := (others => False);
      end return;
   end Default;

   procedure Reset_For_Wave_Format_16 (W : in out Wave_Format_Extensible) is
   begin
      W.Size := 0;
      Reset_For_Wave_Format_18 (W);
   end Reset_For_Wave_Format_16;

   procedure Reset_For_Wave_Format_18 (W : in out Wave_Format_Extensible) is
      use Audio.RIFF.Wav.GUIDs;
   begin
      W.Valid_Bits_Per_Sample := To_Unsigned_16 (W.Bits_Per_Sample);
      W.Sub_Format            := GUID_Undefined;
      W.Channel_Mask          := (others => False);
   end Reset_For_Wave_Format_18;

   function To_GUID (Format : Wav_Format_Tag) return GUID is
      use Audio.RIFF.Wav.GUIDs;
   begin
      case Format is
         when Wav_Format_PCM       => return GUID_PCM;
         when Wav_Format_PCM_Float => return GUID_IEEE_Float;
         when others               => return GUID_Undefined;
      end case;
   end To_GUID;

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

   function Block_Align
     (Bit_Depth          : Wav_Bit_Depth;
      Number_Of_Channels : Positive) return Unsigned_16 is
     (((To_Unsigned_16 (Bit_Depth) + 7) / 8)
      * Unsigned_16 (Number_Of_Channels));

   function Average_Bytes_Per_Second
     (Block_Align        : Unsigned_16;
      Sample_Rate        : Wav_Sample_Rate) return Unsigned_32 is
     (Unsigned_32 (Block_Align) * To_Unsigned_32 (Sample_Rate));

   function Init (Format             : Wav_Format_Tag;
                  Bit_Depth          : Wav_Bit_Depth;
                  Sample_Rate        : Wav_Sample_Rate;
                  Number_Of_Channels : Positive) return Wave_Format_Extensible
   is
      Use_Wav_Extensible : constant Boolean
        := Should_Use_Extensible_Format (Bit_Depth, Number_Of_Channels);
      use Audio.RIFF.Wav.GUIDs;
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
            W.Channel_Mask          := (others => False);
         else
            W.Size                  := 22;
            W.Valid_Bits_Per_Sample := To_Unsigned_16 (W.Bits_Per_Sample);
            W.Channel_Mask          := (others => False);

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

   function Is_Float_Format
     (W : Wave_Format_Extensible) return Boolean
   is
      use Audio.RIFF.Wav.GUIDs;
   begin
      return W.Format_Tag = Wav_Format_PCM_Float or
        W.Sub_Format = GUID_IEEE_Float;
   end Is_Float_Format;

   procedure Read_Channel_Mask
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Channel_Mask_Type)
   is
      V : Channel_Mask_Integer;
      X : Channel_Mask_Type;
      for X'Address use V'Address;
   begin
      Channel_Mask_Integer'Read (Stream, V);
      Item := X;
   end Read_Channel_Mask;

   procedure Write_Channel_Mask
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Channel_Mask_Type)
   is
      V : Channel_Mask_Integer;
      X : Channel_Mask_Type;
      for X'Address use V'Address;
   begin
      X := Item;
      Channel_Mask_Integer'Write (Stream, V);
   end Write_Channel_Mask;

end Audio.RIFF.Wav.Formats;
