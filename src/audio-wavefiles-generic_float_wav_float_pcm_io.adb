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

with Audio.Wavefiles.Generic_Float_Wav_IO;

package body Audio.Wavefiles.Generic_Float_Wav_Float_PCM_IO is

   type Wav_MC_Sample is array (Positive range <>) of Wav_Sample;

   package Wav_IO is new Audio.Wavefiles.Generic_Float_Wav_IO
     (Wav_Sample    => Wav_Sample,
      Wav_MC_Sample => Wav_MC_Sample);
   use Wav_IO;

   function Convert_Samples (Wav : Wav_MC_Sample) return PCM_MC_Sample
     with Inline;
   function Convert_Samples (PCM : PCM_MC_Sample) return Wav_MC_Sample
     with Inline;

   function Convert_Samples (Wav : Wav_MC_Sample) return PCM_MC_Sample is
   begin
      return PCM : PCM_MC_Sample (Wav'Range) do
         for I in PCM'Range loop
            PCM (I) := PCM_Sample (Wav (I));
         end loop;
      end return;
   end Convert_Samples;

   function Convert_Samples (PCM : PCM_MC_Sample) return Wav_MC_Sample is
   begin
      return Wav : Wav_MC_Sample (PCM'Range) do
         for I in Wav'Range loop
            Wav (I) := Wav_Sample (PCM (I));
         end loop;
      end return;
   end Convert_Samples;

   function Get (WF  : in out Wavefile) return PCM_MC_Sample is
      Wav : constant Wav_MC_Sample := Get (WF);
      PCM : constant PCM_MC_Sample := Convert_Samples (Wav);
   begin
      return PCM;
   end Get;

   procedure Put (WF  : in out Wavefile;
                  PCM :        PCM_MC_Sample) is
      N_Ch : constant Positive := Number_Of_Channels (WF);
      Wav  : constant Wav_MC_Sample := Convert_Samples (PCM);
   begin
      Ada.Assertions.Assert (N_Ch = PCM'Length,
                             "Wrong number of channels in buffer");
      Put (WF, Wav);
   end Put;

end Audio.Wavefiles.Generic_Float_Wav_Float_PCM_IO;
