------------------------------------------------------------------------------
--                                                                          --
--                               WAVEFILES                                  --
--                                                                          --
--     Text I/O packages for common data types for wavefile buffer I/O      --
--                                                                          --
--  The MIT License (MIT)                                                   --
--                                                                          --
--  Copyright (c) 2020 Gustavo A. Hoffmann                                  --
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

with Ada.Text_IO;

package Audio.Wavefiles.Data_Types.Text_IO is

   package Wav_Fixed_8_Text_IO is new
     Ada.Text_IO.Fixed_IO (Wav_Fixed_8);

   package Wav_Fixed_16_Text_IO is new
     Ada.Text_IO.Fixed_IO (Wav_Fixed_16);

   package Wav_Fixed_24_Text_IO is new
     Ada.Text_IO.Fixed_IO (Wav_Fixed_24);

   package Wav_Fixed_32_Text_IO is new
     Ada.Text_IO.Fixed_IO (Wav_Fixed_32);

   package Wav_Fixed_64_Text_IO is new
     Ada.Text_IO.Fixed_IO (Wav_Fixed_64);

   package Wav_Float_32_Text_IO is new
     Ada.Text_IO.Float_IO (Wav_Float_32);

   package Wav_Float_64_Text_IO is new
     Ada.Text_IO.Float_IO (Wav_Float_64);

   package Wav_Float_128_Text_IO is new
     Ada.Text_IO.Float_IO (Wav_Float_128);

end Audio.Wavefiles.Data_Types.Text_IO;
