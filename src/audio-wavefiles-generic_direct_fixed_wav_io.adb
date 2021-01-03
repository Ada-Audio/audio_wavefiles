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

with Audio.Wavefiles.Generic_Fixed_Wav_IO;

package body Audio.Wavefiles.Generic_Direct_Fixed_Wav_IO is

   package Wav_IO is new Audio.Wavefiles.Generic_Fixed_Wav_IO
     (Wav_Sample    => Wav_Sample,
      Channel_Range => Channel_Range,
      Wav_MC_Sample => Wav_MC_Sample);

   ---------
   -- Get --
   ---------

   function Get (WF  : in out Wavefile) return Wav_MC_Sample is
     (Wav_IO.Get (WF));

   ---------
   -- Get --
   ---------

   procedure Get (WF  : in out Wavefile;
                  Wav :    out Wav_MC_Sample) is
   begin
      Wav_IO.Get (WF, Wav);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put (WF  : in out Wavefile;
                  Wav :        Wav_MC_Sample) is
   begin
      Wav_IO.Put (WF, Wav);
   end Put;

end Audio.Wavefiles.Generic_Direct_Fixed_Wav_IO;
