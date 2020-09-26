-------------------------------------------------------------------------------
--
--                           WAVEFILE DEFINITIONS
--
--                                  GUIDs
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

with Audio.Wavefile_Definitions.Wave_Formats;

package Audio.Wavefile_Definitions.GUIDs is

   use  Audio.Wavefile_Definitions.Wave_Formats;

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

end Audio.Wavefile_Definitions.GUIDs;
