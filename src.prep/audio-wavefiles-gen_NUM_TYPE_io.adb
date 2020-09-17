-------------------------------------------------------------------------------
--
--                                WAVEFILES
--
--               Type conversion for wavefile I/O operations
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

#if NUM_TYPE'Defined and then (NUM_TYPE = "FLOAT") then
with Audio.Wavefiles.Float_Types;
#else
with Audio.Wavefiles.Fixed_Types;
#end if;
with Audio.Wavefiles.Gen_Wav_IO;

#if NUM_TYPE'Defined and then (NUM_TYPE = "FLOAT") then
package body Audio.Wavefiles.Gen_Float_IO is
#else
package body Audio.Wavefiles.Gen_Fixed_IO is
#end if;

#if NUM_TYPE'Defined and then (NUM_TYPE = "FLOAT") then
   package Wav_Data_Types is new Audio.Wavefiles.Float_Types
#else
   package Wav_Data_Types is new Audio.Wavefiles.Fixed_Types
#end if;
     (Wav_Num_Type, Wav_Data_Type, PCM_Type);

   type Wav_Data is array (Positive range <>) of Wav_Data_Type;

   package Wav_IO is new Audio.Wavefiles.Gen_Wav_IO
     (Wav_Data_Type => Wav_Data_Type,
      Wav_Data      => Wav_Data);
   use Wav_IO;

   function Convert_Samples (Wav : Wav_Data)      return PCM_MC_Sample;
   function Convert_Samples (PCM : PCM_MC_Sample) return Wav_Data;

   function Convert_Samples (Wav : Wav_Data) return PCM_MC_Sample is
   begin
      return PCM : PCM_MC_Sample (Wav'Range) do
         for I in PCM'Range loop
            PCM (I) := Wav_Data_Types.Convert_Sample (Wav (I));
         end loop;
      end return;
   end Convert_Samples;

   function Convert_Samples (PCM : PCM_MC_Sample) return Wav_Data is
   begin
      return Wav : Wav_Data (PCM'Range) do
         for I in Wav'Range loop
            Wav (I) := Wav_Data_Types.Convert_Sample (PCM (I));
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
      Ch  : constant Positive := Positive (WF.Wave_Format.Channels);
      Wav : constant Wav_Data := Convert_Samples (PCM);
   begin
      Ada.Assertions.Assert (Ch = PCM'Length,
                             "Wrong number of channels in buffer");
      Put (WF, Wav);
   end Put;

#if NUM_TYPE'Defined and then (NUM_TYPE = "FLOAT") then
end Audio.Wavefiles.Gen_Float_IO;
#else
end Audio.Wavefiles.Gen_Fixed_IO;
#end if;
