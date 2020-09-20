-------------------------------------------------------------------------------
--
--                                WAVEFILES
--
--                   Wavefile I/O operations for PCM buffers
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

with Audio.RIFF;                   use Audio.RIFF;
with Audio.Wavefiles.Internals;    use Audio.Wavefiles.Internals;
with Audio.Wavefiles.Generic_Float_Wav_IO;

package body Audio.Wavefiles.Generic_Float_PCM_IO is

   package PCM_Fixed_Wav_16 is new Audio.Wavefiles.Generic_Float_Wav_IO
     (Wav_Num_Type  => Wav_Fixed_Data,
      Wav_Data_Type => Wav_Data_16_Type,
      PCM_Type      => PCM_Type,
      PCM_MC_Sample => PCM_MC_Sample);

   package PCM_Fixed_Wav_24 is new Audio.Wavefiles.Generic_Float_Wav_IO
     (Wav_Num_Type  => Wav_Fixed_Data,
      Wav_Data_Type => Wav_Data_24_Type,
      PCM_Type      => PCM_Type,
      PCM_MC_Sample => PCM_MC_Sample);

   package PCM_Fixed_Wav_32 is new Audio.Wavefiles.Generic_Float_Wav_IO
     (Wav_Num_Type  => Wav_Fixed_Data,
      Wav_Data_Type => Wav_Data_32_Type,
      PCM_Type      => PCM_Type,
      PCM_MC_Sample => PCM_MC_Sample);

   package PCM_Float_Wav_32 is new Audio.Wavefiles.Generic_Float_Wav_IO
     (Wav_Num_Type  => Wav_Float_Data,
      Wav_Data_Type => Wav_Data_32_Type,
      PCM_Type      => PCM_Type,
      PCM_MC_Sample => PCM_MC_Sample);

   package PCM_Float_Wav_64 is new Audio.Wavefiles.Generic_Float_Wav_IO
     (Wav_Num_Type  => Wav_Float_Data,
      Wav_Data_Type => Wav_Data_64_Type,
      PCM_Type      => PCM_Type,
      PCM_MC_Sample => PCM_MC_Sample);

   function Get
     (WF   : in out Wavefile) return PCM_MC_Sample
   is
      Ch : constant Positive := Positive (WF.Wave_Format.Channels);
   begin
      if not WF.Is_Opened then
         raise Wavefile_Error;
      end if;
      if not Is_Supported_Format (WF.Wave_Format) then
         raise Wavefile_Unsupported;
      end if;

      WF.Samples_Read := WF.Samples_Read + Long_Integer (Ch);

      if WF.Wave_Format.Sub_Format = GUID_IEEE_Float then
         case WF.Wave_Format.Bits_Per_Sample is
            when 32 =>
               return PCM_Float_Wav_32.Get (WF);
            when 64 =>
               return PCM_Float_Wav_64.Get (WF);
            when others =>
               raise Wavefile_Unsupported;
         end case;
      else
         --  Always assume WF.Wave_Format.Sub_Format = GUID_PCM
         case WF.Wave_Format.Bits_Per_Sample is
            when 8 =>
               raise Wavefile_Unsupported;
            when 16 =>
               return PCM_Fixed_Wav_16.Get (WF);
            when 24 =>
               return PCM_Fixed_Wav_24.Get (WF);
            when 32 =>
               return PCM_Fixed_Wav_32.Get (WF);
            when others =>
               raise Wavefile_Unsupported;
         end case;
      end if;

   end Get;

   procedure Put
     (WF   : in out Wavefile;
      PCM  :        PCM_MC_Sample) is
   begin
      if not WF.Is_Opened then
         raise Wavefile_Error;
      end if;

      if not Is_Supported_Format (WF.Wave_Format) then
         raise Wavefile_Unsupported;
      end if;

      if WF.Wave_Format.Sub_Format = GUID_IEEE_Float then
         case WF.Wave_Format.Bits_Per_Sample is
            when 32 =>
               PCM_Float_Wav_32.Put (WF, PCM);
            when 64 =>
               PCM_Float_Wav_64.Put (WF, PCM);
            when others =>
               raise Wavefile_Unsupported;
         end case;
      else
         --  Always assume WF.Wave_Format.Sub_Format = GUID_PCM
         case WF.Wave_Format.Bits_Per_Sample is
            when 8 =>
               raise Wavefile_Unsupported;
            when 16 =>
               PCM_Fixed_Wav_16.Put (WF, PCM);
            when 24 =>
               PCM_Fixed_Wav_24.Put (WF, PCM);
            when 32 =>
               PCM_Fixed_Wav_32.Put (WF, PCM);
            when others =>
               raise Wavefile_Unsupported;
         end case;
      end if;

   end Put;

end Audio.Wavefiles.Generic_Float_PCM_IO;
