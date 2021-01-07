------------------------------------------------------------------------------
--                                                                          --
--                         AUDIO / RIFF / WAV                               --
--                                                                          --
--           Reporting of RIFF format information for wavefiles             --
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

with Ada.Text_IO;          use Ada.Text_IO;

with Audio.RIFF.Wav.GUIDs; use Audio.RIFF.Wav.GUIDs;

package body Audio.RIFF.Wav.Formats.Report is

   -----------
   -- Print --
   -----------

   procedure Print (W : Wave_Format_Extensible) is
   begin
      Put_Line ("------------ WAVEFORMAT header  ------------");
      Put_Line ("Format:              "
                & Wav_Format_Tag'Image (W.Format_Tag));
      Put_Line ("BitsPerSample:      "
                & Image (W.Bits_Per_Sample));
      Put_Line ("Channels:           "
                & Unsigned_16'Image (W.Channels));
      Put_Line ("SamplesPerSec:      "
                & Positive'Image
                  (To_Positive (W.Samples_Per_Sec)));
      Put_Line ("BlockAlign:         "
                & Unsigned_16'Image (W.Block_Align));
      Put_Line ("AvgBytesPerSec:     "
                & Unsigned_32'Image (W.Avg_Bytes_Per_Sec));
      Put_Line ("Ext. Size:          "
                & Unsigned_16'Image (W.Size));
      if W.Size > 0 then

         Put_Line ("ValidBitsPerSample: "
                   & Unsigned_16'Image (W.Valid_Bits_Per_Sample));
         Put ("Channel Mask:      ");
         if W.Channel_Config (Speaker_Front_Left) then
            Put (" Front_Left");
         end if;
         if W.Channel_Config (Speaker_Front_Right) then
            Put (" Front_Right");
         end if;
         if W.Channel_Config (Speaker_Front_Center) then
            Put (" Front_Center");
         end if;
         if W.Channel_Config (Speaker_Low_Frequency) then
            Put (" Low_Frequency");
         end if;
         if W.Channel_Config (Speaker_Back_Left) then
            Put (" Back_Left");
         end if;
         if W.Channel_Config (Speaker_Back_Right) then
            Put (" Back_Right");
         end if;
         if W.Channel_Config (Speaker_Front_Left_Of_Center) then
            Put (" Front_Left_Of_Center");
         end if;
         if W.Channel_Config (Speaker_Front_Right_Of_Center) then
            Put (" Front_Right_Of_Center");
         end if;
         if W.Channel_Config (Speaker_Back_Center) then
            Put (" Back_Center");
         end if;
         if W.Channel_Config (Speaker_Side_Left) then
            Put (" Side_Left");
         end if;
         if W.Channel_Config (Speaker_Side_Right) then
            Put (" Side_Right");
         end if;
         if W.Channel_Config (Speaker_Top_Center) then
            Put (" Top_Center");
         end if;
         if W.Channel_Config (Speaker_Top_Front_Left) then
            Put (" Top_Front_Left");
         end if;
         if W.Channel_Config (Speaker_Top_Front_Center) then
            Put (" Top_Front_Center");
         end if;
         if W.Channel_Config (Speaker_Top_Front_Right) then
            Put (" Top_Front_Right");
         end if;
         if W.Channel_Config (Speaker_Top_Back_Left) then
            Put (" Top_Back_Left");
         end if;
         if W.Channel_Config (Speaker_Top_Back_Center) then
            Put (" Top_Back_Center");
         end if;
         if W.Channel_Config (Speaker_Top_Back_Right) then
            Put (" Top_Back_Right");
         end if;

         New_Line;

         Put ("SubFormat:          ");
         if W.Sub_Format = GUID_Undefined then
            Put_Line ("undefined");
         elsif W.Sub_Format = GUID_PCM then
            Put_Line ("KSDATAFORMAT_SUBTYPE_PCM (IEC 60958 PCM)");
         elsif W.Sub_Format = GUID_IEEE_Float then
            Put_Line ("KSDATAFORMAT_SUBTYPE_IEEE_FLOAT " &
                        "(IEEE Floating-Point PCM)");
         else
            Put_Line ("unknown");
         end if;
      end if;
      Put_Line ("-------------------------------------------");
   end Print;

end Audio.RIFF.Wav.Formats.Report;
