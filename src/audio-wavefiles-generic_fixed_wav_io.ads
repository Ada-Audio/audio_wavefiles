------------------------------------------------------------------------------
--                                                                          --
--          THIS IS AN AUTOMATICALLY GENERATED FILE! DO NOT EDIT!           --
--                                                                          --
--                               WAVEFILES                                  --
--                                                                          --
--                      Wavefile data I/O operations                        --
--                                                                          --
--  The MIT License (MIT)                                                   --
--                                                                          --
--  Copyright (c) 2015 -- 2021 Gustavo A. Hoffmann                          --
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

private generic
   type Wav_Sample is delta <>;
   type Channel_Range is (<>);
   type Wav_MC_Sample is array (Channel_Range range <>) of Wav_Sample;
package Audio.Wavefiles.Generic_Fixed_Wav_IO is

   function Get (WF  : in out Wavefile) return Wav_MC_Sample
     with Inline, Pre => Mode (WF) = In_File;

   procedure Get (WF  : in out Wavefile;
                  Wav :    out Wav_MC_Sample)
     with Inline, Pre => Mode (WF) = In_File;

   procedure Put (WF  : in out Wavefile;
                  Wav :        Wav_MC_Sample)
     with Inline,
          Pre => Mode (WF) = Out_File
                 and Wav'Length >= Number_Of_Channels (WF);

end Audio.Wavefiles.Generic_Fixed_Wav_IO;
