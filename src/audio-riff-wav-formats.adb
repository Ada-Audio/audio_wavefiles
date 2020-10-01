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
