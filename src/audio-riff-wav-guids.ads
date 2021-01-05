------------------------------------------------------------------------------
--                                                                          --
--                         AUDIO / RIFF / WAV                               --
--                                                                          --
--                         GUIDs for wavefiles                              --
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

with Audio.RIFF.Wav.Formats;

package Audio.RIFF.Wav.GUIDs is

   use  Audio.RIFF.Wav.Formats;

   GUID_Undefined : constant GUID :=
                      (16#00000000#,
                       16#0000#,
                       16#0000#,
                       (16#00#, 16#00#,
                        16#00#, 16#00#,
                        16#00#, 16#00#,
                        16#00#, 16#00#));
   --  Undefined GUID

   GUID_PCM       : constant GUID :=
                      (16#00000001#,
                       16#0000#,
                       16#0010#,
                       (16#80#, 16#00#,
                        16#00#, 16#aa#,
                        16#00#, 16#38#,
                        16#9b#, 16#71#));
   --  CEA 861 0x01: KSDATAFORMAT_SUBTYPE_PCM (IEC 60958 PCM)

   GUID_IEEE_Float : constant GUID :=
                       (16#00000003#,
                        16#0000#,
                        16#0010#,
                        (16#80#, 16#00#,
                         16#00#, 16#aa#,
                         16#00#, 16#38#,
                         16#9b#, 16#71#));
   --  CEA 861 0x01: KSDATAFORMAT_SUBTYPE_IEEE_FLOAT (IEEE Floating-Point PCM)

   GUID_DRM : constant GUID :=
                      (16#00000009#,
                       16#0000#,
                       16#0010#,
                       (16#80#, 16#00#,
                        16#00#, 16#aa#,
                        16#00#, 16#38#,
                        16#9b#, 16#71#));
   --  KSDATAFORMAT_SUBTYPE_DRM

   GUID_ALAW : constant GUID :=
                      (16#00000006#,
                       16#0000#,
                       16#0010#,
                       (16#80#, 16#00#,
                        16#00#, 16#aa#,
                        16#00#, 16#38#,
                        16#9b#, 16#71#));
   --  KSDATAFORMAT_SUBTYPE_ALAW

   GUID_MULAW : constant GUID :=
                      (16#00000007#,
                       16#0000#,
                       16#0010#,
                       (16#80#, 16#00#,
                        16#00#, 16#aa#,
                        16#00#, 16#38#,
                        16#9b#, 16#71#));
   --  KSDATAFORMAT_SUBTYPE_MULAW

   GUID_ADPCM : constant GUID :=
                      (16#00000002#,
                       16#0000#,
                       16#0010#,
                       (16#80#, 16#00#,
                        16#00#, 16#aa#,
                        16#00#, 16#38#,
                        16#9b#, 16#71#));
   --  KSDATAFORMAT_SUBTYPE_ADPCM

   GUID_MPEG : constant GUID :=
                      (16#00000050#,
                       16#0000#,
                       16#0010#,
                       (16#80#, 16#00#,
                        16#00#, 16#aa#,
                        16#00#, 16#38#,
                        16#9b#, 16#71#));
   --  KSDATAFORMAT_SUBTYPE_MPEG

   GUID_DOLBY_AC3_SPDIF : constant GUID :=
                      (16#00000092#,
                       16#0000#,
                       16#0010#,
                       (16#80#, 16#00#,
                        16#00#, 16#aa#,
                        16#00#, 16#38#,
                        16#9b#, 16#71#));
   --  KSDATAFORMAT_SUBTYPE_IEC61937_DOLBY_DIGITAL
   --  WAVE_FORMAT_DOLBY_AC3_SPDIF

   GUID_WMA_SPDIF : constant GUID :=
                      (16#00000164#,
                       16#0000#,
                       16#0010#,
                       (16#80#, 16#00#,
                        16#00#, 16#aa#,
                        16#00#, 16#38#,
                        16#9b#, 16#71#));
   --  KSDATAFORMAT_SUBTYPE_IEC61937_WMA_PRO
   --  WAVE_FORMAT_WMASPDIF

   GUID_DTS : constant GUID :=
                      (16#00000008#,
                       16#0000#,
                       16#0010#,
                       (16#80#, 16#00#,
                        16#00#, 16#aa#,
                        16#00#, 16#38#,
                        16#9b#, 16#71#));
   --  KSDATAFORMAT_SUBTYPE_IEC61937_DTS
   --  WAVE_FORMAT_DTS

   GUID_MPEG_LAYER_3 : constant GUID :=
                      (16#00000055#,
                       16#0000#,
                       16#0010#,
                       (16#80#, 16#00#,
                        16#00#, 16#aa#,
                        16#00#, 16#38#,
                        16#9b#, 16#71#));
   --  KSDATAFORMAT_SUBTYPE_MPEGLAYER3
   --  WAVE_FORMAT_MPEGLAYER3

   GUID_MPEG_HE_AAC : constant GUID :=
                      (16#00001610#,
                       16#0000#,
                       16#0010#,
                       (16#80#, 16#00#,
                        16#00#, 16#aa#,
                        16#00#, 16#38#,
                        16#9b#, 16#71#));
   --  KSDATAFORMAT_SUBTYPE_MPEG_HEAAC
   --  WAVE_FORMAT_MPEG_HEAAC

   GUID_WMA_2 : constant GUID :=
                      (16#00000161#,
                       16#0000#,
                       16#0010#,
                       (16#80#, 16#00#,
                        16#00#, 16#aa#,
                        16#00#, 16#38#,
                        16#9b#, 16#71#));
   --  KSDATAFORMAT_SUBTYPE_WMAUDIO2
   --  WAVE_FORMAT_WMAUDIO2

   GUID_WMA_3 : constant GUID :=
                      (16#00000162#,
                       16#0000#,
                       16#0010#,
                       (16#80#, 16#00#,
                        16#00#, 16#aa#,
                        16#00#, 16#38#,
                        16#9b#, 16#71#));
   --  KSDATAFORMAT_SUBTYPE_WMAUDIO3
   --  WAVE_FORMAT_WMAUDIO3

   GUID_WMA_LOSSLESS : constant GUID :=
                      (16#00000163#,
                       16#0000#,
                       16#0010#,
                       (16#80#, 16#00#,
                        16#00#, 16#aa#,
                        16#00#, 16#38#,
                        16#9b#, 16#71#));
   --  KSDATAFORMAT_SUBTYPE_WMAUDIO_LOSSLESS
   --  WAVE_FORMAT_WMAUDIO_LOSSLESS

end Audio.RIFF.Wav.GUIDs;
