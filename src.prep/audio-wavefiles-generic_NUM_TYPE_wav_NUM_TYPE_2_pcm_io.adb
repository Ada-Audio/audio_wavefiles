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

   type Wav_MC_Sample is array (Channel_Range range <>) of Wav_Sample;

#if (NUM_TYPE = "FLOAT") then
   package Wav_IO is new Audio.Wavefiles.Generic_Float_Wav_IO
#else
   package Wav_IO is new Audio.Wavefiles.Generic_Fixed_Wav_IO
#end if;
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
#if (NUM_TYPE = "FLOAT") and then (NUM_TYPE_2 = "FIXED") then
   function Saturate (Wav : Wav_Sample) return PCM_Sample
     with Inline;
#end if;
#if (NUM_TYPE = "FIXED") and then (NUM_TYPE_2 = "FLOAT") then
   function Saturate (PCM : PCM_Sample) return Wav_Sample
     with Inline;
#end if;

#if (NUM_TYPE = "FLOAT") and then (NUM_TYPE_2 = "FIXED") then
   --------------
   -- Saturate --
   --------------

   function Saturate (Wav : Wav_Sample) return PCM_Sample is
   begin
      if Wav > Wav_Sample (PCM_Sample'Last) then
         return PCM_Sample'Last;
      elsif Wav < Wav_Sample (PCM_Sample'First) then
         return PCM_Sample'First;
      else
         return PCM_Sample (Wav);
      end if;
   end Saturate;

#end if;
#if (NUM_TYPE = "FIXED") and then (NUM_TYPE_2 = "FLOAT") then
   --------------
   -- Saturate --
   --------------

   function Saturate (PCM : PCM_Sample) return Wav_Sample is
   begin
      if PCM > PCM_Sample (Wav_Sample'Last) then
         return Wav_Sample'Last;
      elsif PCM < PCM_Sample (Wav_Sample'First) then
         return Wav_Sample'First;
      else
         return Wav_Sample (PCM);
      end if;
   end Saturate;

#end if;
   -------------
   -- Convert --
   -------------

   procedure Convert (Wav :     Wav_MC_Sample;
                      PCM : out PCM_MC_Sample) is
   begin
      for I in PCM'Range loop
#if (NUM_TYPE = "FLOAT") and then (NUM_TYPE_2 = "FIXED") then
         PCM (I) := Saturate (Wav (I));
#else
         PCM (I) := PCM_Sample (Wav (I));
#end if;
      end loop;
   end Convert;

   -------------
   -- Convert --
   -------------

   procedure Convert (PCM :     PCM_MC_Sample;
                      Wav : out Wav_MC_Sample) is
   begin
      for I in Wav'Range loop
#if (NUM_TYPE = "FIXED") and then (NUM_TYPE_2 = "FLOAT") then
         Wav (I) := Saturate (PCM (I));
#else
         Wav (I) := Wav_Sample (PCM (I));
#end if;
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

#if (NUM_TYPE = "FLOAT") and then (NUM_TYPE_2 = "FLOAT") then
end Audio.Wavefiles.Generic_Float_Wav_Float_PCM_IO;
#elsif (NUM_TYPE = "FLOAT") and then (NUM_TYPE_2 = "FIXED") then
end Audio.Wavefiles.Generic_Float_Wav_Fixed_PCM_IO;
#elsif (NUM_TYPE = "FIXED") and then (NUM_TYPE_2 = "FLOAT") then
end Audio.Wavefiles.Generic_Fixed_Wav_Float_PCM_IO;
#elsif (NUM_TYPE = "FIXED") and then (NUM_TYPE_2 = "FIXED") then
end Audio.Wavefiles.Generic_Fixed_Wav_Fixed_PCM_IO;
#end if;
