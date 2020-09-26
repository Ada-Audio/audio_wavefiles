
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
------------------------------------------------------------------------------

with Audio.Wavefiles.Read;
with Audio.Wavefiles.Write;
with Audio.Wavefiles.RIFF;  use Audio.Wavefiles.RIFF;

package body Audio.Wavefiles is

   use Ada.Streams.Stream_IO;

   type Channel_Mask_Integer is mod 2**Channel_Mask_Type_Size;

   procedure Open
     (WF          : in out Wavefile;
      Mode        : Wav_File_Mode;
      File_Name   : String;
      Wave_Format : in out Wave_Format_Extensible) is
   begin
      if Mode = In_File then
         Audio.Wavefiles.Read.Open (WF, File_Name, Wave_Format);
      else
         Audio.Wavefiles.Write.Open (WF, File_Name, Wave_Format);
      end if;
   end Open;

   function Is_EOF
     (WF   : in out Wavefile) return Boolean
   is (Audio.Wavefiles.Read.Is_EOF (WF));

   procedure Display_Info (WF : in Wavefile) is
   begin
      Audio.Wavefiles.Read.Display_Info (WF);
   end Display_Info;

   procedure Close (WF : in out Wavefile) is
   begin
      if File_Mode (WF) = In_File then
         Audio.Wavefiles.Read.Close (WF);
      else
         Audio.Wavefiles.Write.Close (WF);
      end if;
   end Close;

   function Format_Of_Wavefile
     (W : Wavefile) return  Wave_Format_Extensible is
   begin
      return W.Wave_Format;
   end Format_Of_Wavefile;

   function Number_Of_Channels
     (W : Wavefile) return Positive is (Positive (W.Wave_Format.Channels));

   function File_Mode (W : Wavefile) return Wav_File_Mode is
     (if Mode (W.File) = In_File then In_File else Out_File);

   function Is_Supported_Format (W : Wave_Format_Extensible)
                                 return Boolean is
   begin
      if not (W.Sub_Format = GUID_Undefined
              or W.Sub_Format = GUID_PCM
              or W.Sub_Format = GUID_IEEE_Float)
      then
         return False;
      end if;

      return True;
   end Is_Supported_Format;

   procedure Set_Default (W : out Wave_Format_16) is
   begin
      W.Format_Tag        := 16#0001#;
      W.Channels          := 2;
      W.Samples_Per_Sec   := 44100;
      W.Bits_Per_Sample   := 16;
      W.Block_Align       := ((W.Bits_Per_Sample + 7) / 8) * W.Channels;
      W.Avg_Bytes_Per_Sec := 0;
   end Set_Default;

   procedure Set_Default (W : out Wave_Format_18) is
   begin
      Set_Default (Wave_Format_16 (W));
      null;
   end Set_Default;

   procedure Set_Default (W : out Wave_Format_Extensible) is
   begin
      Set_Default (Wave_Format_18 (W));
      W.Size := 22;
      null;
   end Set_Default;

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

end Audio.Wavefiles;
