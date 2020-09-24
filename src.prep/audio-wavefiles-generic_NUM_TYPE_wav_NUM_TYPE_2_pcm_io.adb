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

with Ada.Assertions;

#if (NUM_TYPE = "FLOAT") then
with Audio.Wavefiles.Generic_Float_Wav_IO;
#else
with Audio.Wavefiles.Generic_Fixed_Wav_IO;
#end if;

#if (NUM_TYPE = "FLOAT") and then (NUM_TYPE_2 = "FLOAT") then
package body Audio.Wavefiles.Generic_Float_Wav_Float_PCM_IO is
#elsif (NUM_TYPE = "FLOAT") and then (NUM_TYPE_2 = "FIXED") then
package body Audio.Wavefiles.Generic_Float_Wav_Fixed_PCM_IO is
#elsif (NUM_TYPE = "FIXED") and then (NUM_TYPE_2 = "FLOAT") then
package body Audio.Wavefiles.Generic_Fixed_Wav_Float_PCM_IO is
#elsif (NUM_TYPE = "FIXED") and then (NUM_TYPE_2 = "FIXED") then
package body Audio.Wavefiles.Generic_Fixed_Wav_Fixed_PCM_IO is
#end if;

   type Wav_Data is array (Positive range <>) of Wav_Data_Type;

#if (NUM_TYPE = "FLOAT") then
   package Wav_IO is new Audio.Wavefiles.Generic_Float_Wav_IO
#else
   package Wav_IO is new Audio.Wavefiles.Generic_Fixed_Wav_IO
#end if;
     (Wav_Data_Type => Wav_Data_Type,
      Wav_Data      => Wav_Data);
   use Wav_IO;

   function Convert_Samples (Wav : Wav_Data)      return PCM_MC_Sample
     with Inline;
   function Convert_Samples (PCM : PCM_MC_Sample) return Wav_Data
     with Inline;

   function Convert_Samples (Wav : Wav_Data) return PCM_MC_Sample is
   begin
      return PCM : PCM_MC_Sample (Wav'Range) do
         for I in PCM'Range loop
            PCM (I) := PCM_Type (Wav (I));
         end loop;
      end return;
   end Convert_Samples;

   function Convert_Samples (PCM : PCM_MC_Sample) return Wav_Data is
   begin
      return Wav : Wav_Data (PCM'Range) do
         for I in Wav'Range loop
            Wav (I) := Wav_Data_Type (PCM (I));
         end loop;
      end return;
   end Convert_Samples;

   function Get (WF  : in out Wavefile) return PCM_MC_Sample is
      Wav : constant Wav_Data      := Get (WF);
      PCM : constant PCM_MC_Sample := Convert_Samples (Wav);
   begin
      return PCM;
   end Get;

   procedure Put (WF  : in out Wavefile;
                  PCM :        PCM_MC_Sample) is
      N_Ch : constant Positive := Number_Of_Channels (WF);
      Wav  : constant Wav_Data := Convert_Samples (PCM);
   begin
      Ada.Assertions.Assert (N_Ch = PCM'Length,
                             "Wrong number of channels in buffer");
      Put (WF, Wav);
   end Put;

#if (NUM_TYPE = "FLOAT") and then (NUM_TYPE_2 = "FLOAT") then
end Audio.Wavefiles.Generic_Float_Wav_Float_PCM_IO;
#elsif (NUM_TYPE = "FLOAT") and then (NUM_TYPE_2 = "FIXED") then
end Audio.Wavefiles.Generic_Float_Wav_Fixed_PCM_IO;
#elsif (NUM_TYPE = "FIXED") and then (NUM_TYPE_2 = "FLOAT") then
end Audio.Wavefiles.Generic_Fixed_Wav_Float_PCM_IO;
#elsif (NUM_TYPE = "FIXED") and then (NUM_TYPE_2 = "FIXED") then
end Audio.Wavefiles.Generic_Fixed_Wav_Fixed_PCM_IO;
#end if;
