-------------------------------------------------------------------------------
--
--                                WAVEFILES
--
--                      Wavefile data I/O operations
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

generic
#if (NUM_TYPE = "FLOAT") then
   type Wav_Data_Type is digits <>;
#else
   type Wav_Data_Type is delta <>;
#end if;
   type Wav_MC_Sample is array (Positive range <>) of Wav_Data_Type;
#if NUM_TYPE'Defined and then (NUM_TYPE = "FLOAT") then
package Audio.Wavefiles.Generic_Float_Wav_IO is
#else
package Audio.Wavefiles.Generic_Fixed_Wav_IO is
#end if;

   function Get (WF  : in out Wavefile) return Wav_MC_Sample
     with Inline, Pre => File_Mode (WF) = In_File;


   procedure Put (WF  : in out Wavefile;
                  Wav :        Wav_MC_Sample)
     with Inline, Pre => File_Mode (WF) = Out_File;

#if NUM_TYPE'Defined and then (NUM_TYPE = "FLOAT") then
end Audio.Wavefiles.Generic_Float_Wav_IO;
#else
end Audio.Wavefiles.Generic_Fixed_Wav_IO;
#end if;
