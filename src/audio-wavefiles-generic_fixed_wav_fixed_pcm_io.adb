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

with Audio.Wavefiles.Generic_Fixed_Wav_IO;

package body Audio.Wavefiles.Generic_Fixed_Wav_Fixed_PCM_IO is

   type Wav_MC_Sample is array (Channel_Range range <>) of Wav_Sample;

   package Wav_IO is new Audio.Wavefiles.Generic_Fixed_Wav_IO
     (Wav_Sample    => Wav_Sample,
      Channel_Range => Channel_Range,
      Wav_MC_Sample => Wav_MC_Sample);
   use Wav_IO;

   procedure Convert (Wav :     Wav_MC_Sample;
                      PCM : out PCM_MC_Sample)
     with Inline;
   procedure Convert (PCM :     PCM_MC_Sample;
                      Wav : out Wav_MC_Sample)
     with Inline;
   function Convert (Wav : Wav_MC_Sample) return PCM_MC_Sample
     with Inline;
   function Convert (PCM : PCM_MC_Sample) return Wav_MC_Sample
     with Inline;
   pragma Unreferenced (Convert);

   -------------
   -- Convert --
   -------------

   procedure Convert (Wav :     Wav_MC_Sample;
                      PCM : out PCM_MC_Sample) is
   begin
      for I in PCM'Range loop
         PCM (I) := PCM_Sample (Wav (I));
      end loop;
   end Convert;

   -------------
   -- Convert --
   -------------

   procedure Convert (PCM :     PCM_MC_Sample;
                      Wav : out Wav_MC_Sample) is
   begin
      for I in Wav'Range loop
         Wav (I) := Wav_Sample (PCM (I));
      end loop;
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Wav : Wav_MC_Sample) return PCM_MC_Sample is
   begin
      return PCM : PCM_MC_Sample (Wav'Range) do
         Convert (Wav, PCM);
      end return;
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (PCM : PCM_MC_Sample) return Wav_MC_Sample is
   begin
      return Wav : Wav_MC_Sample (PCM'Range) do
         Convert (PCM, Wav);
      end return;
   end Convert;

   ---------
   -- Get --
   ---------

   function Get (WF  : in out Wavefile) return PCM_MC_Sample is
      Wav : constant Wav_MC_Sample := Get (WF);
      PCM : constant PCM_MC_Sample := Convert (Wav);
   begin
      return PCM;
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get (WF   : in out Wavefile;
                  PCM  :    out PCM_MC_Sample) is
      Wav : Wav_MC_Sample (PCM'Range);
   begin
      Get (WF, Wav);
      Convert (Wav, PCM);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put (WF  : in out Wavefile;
                  PCM :        PCM_MC_Sample) is
      Wav : Wav_MC_Sample (PCM'Range);
   begin
      Convert (PCM, Wav);
      Put (WF, Wav);
   end Put;

end Audio.Wavefiles.Generic_Fixed_Wav_Fixed_PCM_IO;
