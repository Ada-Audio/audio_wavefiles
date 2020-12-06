------------------------------------------------------------------------------
--                                                                          --
--          THIS IS AN AUTOMATICALLY GENERATED FILE! DO NOT EDIT!           --
--                                                                          --
--                               WAVEFILES                                  --
--                                                                          --
--                  Wavefile I/O operations for PCM buffers                 --
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

with Audio.Wavefiles.Data_Types;   use Audio.Wavefiles.Data_Types;

with Audio.Wavefiles.Generic_Float_Wav_Float_PCM_IO;
with Audio.Wavefiles.Generic_Fixed_Wav_Float_PCM_IO;

package body Audio.Wavefiles.Generic_Float_PCM_IO is

   package PCM_Fixed_Wav_16 is new
     Audio.Wavefiles.Generic_Fixed_Wav_Float_PCM_IO
     (Wav_Sample    => Wav_Fixed_16,
      PCM_Sample    => PCM_Sample,
      Channel_Range => Channel_Range,
      PCM_MC_Sample => PCM_MC_Sample);

   package PCM_Fixed_Wav_24 is new
     Audio.Wavefiles.Generic_Fixed_Wav_Float_PCM_IO
     (Wav_Sample    => Wav_Fixed_24,
      PCM_Sample    => PCM_Sample,
      Channel_Range => Channel_Range,
      PCM_MC_Sample => PCM_MC_Sample);

   package PCM_Fixed_Wav_32 is new
     Audio.Wavefiles.Generic_Fixed_Wav_Float_PCM_IO
     (Wav_Sample    => Wav_Fixed_32,
      PCM_Sample    => PCM_Sample,
      Channel_Range => Channel_Range,
      PCM_MC_Sample => PCM_MC_Sample);

   package PCM_Float_Wav_32 is new
     Audio.Wavefiles.Generic_Float_Wav_Float_PCM_IO
     (Wav_Sample    => Wav_Float_32,
      PCM_Sample    => PCM_Sample,
      Channel_Range => Channel_Range,
      PCM_MC_Sample => PCM_MC_Sample);

   package PCM_Float_Wav_64 is new
     Audio.Wavefiles.Generic_Float_Wav_Float_PCM_IO
     (Wav_Sample    => Wav_Float_64,
      PCM_Sample    => PCM_Sample,
      Channel_Range => Channel_Range,
      PCM_MC_Sample => PCM_MC_Sample);

   ---------
   -- Get --
   ---------

   function Get
     (WF   : in out Wavefile) return PCM_MC_Sample
   is
      Dummy_MC_Sample :
        PCM_MC_Sample (Channel_Range'Succ (Channel_Range'First) ..
                       Channel_Range'First);
   begin
      if not WF.Is_Opened then
         WF.Set_Error (Wavefile_Error_File_Not_Open);
         return Dummy_MC_Sample;
      end if;

      if not Is_Supported_Format (WF.Wave_Format) then
         WF.Set_Error (Wavefile_Error_Unsupported_Wavefile_Format);
         return Dummy_MC_Sample;
      end if;

      if WF.Wave_Format.Is_Float_Format then
         case WF.Wave_Format.Bits_Per_Sample is
            when Bit_Depth_8 | Bit_Depth_16 | Bit_Depth_24 =>
               WF.Set_Error (Wavefile_Error_Unsupported_Bit_Depth);
               return Dummy_MC_Sample;
            when Bit_Depth_32 =>
               return PCM_Float_Wav_32.Get (WF);
            when Bit_Depth_64 =>
               return PCM_Float_Wav_64.Get (WF);
         end case;
      else
         --  Always assume fixed-point PCM format
         case WF.Wave_Format.Bits_Per_Sample is
            when Bit_Depth_8 =>
               WF.Set_Error (Wavefile_Error_Unsupported_Bit_Depth);
               return Dummy_MC_Sample;
            when Bit_Depth_16 =>
               return PCM_Fixed_Wav_16.Get (WF);
            when Bit_Depth_24 =>
               return PCM_Fixed_Wav_24.Get (WF);
            when Bit_Depth_32 =>
               return PCM_Fixed_Wav_32.Get (WF);
            when Bit_Depth_64 =>
               WF.Set_Error (Wavefile_Error_Unsupported_Bit_Depth);
               return Dummy_MC_Sample;
         end case;
      end if;

   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (WF   : in out Wavefile;
      PCM  :    out PCM_MC_Sample) is
   begin
      if not WF.Is_Opened then
         WF.Set_Error (Wavefile_Error_File_Not_Open);
         PCM := (others => 0.0);
      end if;

      if not Is_Supported_Format (WF.Wave_Format) then
         WF.Set_Error (Wavefile_Error_Unsupported_Wavefile_Format);
         PCM := (others => 0.0);
      end if;

      if WF.Wave_Format.Is_Float_Format then
         case WF.Wave_Format.Bits_Per_Sample is
            when Bit_Depth_8 | Bit_Depth_16 | Bit_Depth_24 =>
               WF.Set_Error (Wavefile_Error_Unsupported_Bit_Depth);
               PCM := (others => 0.0);
            when Bit_Depth_32 =>
               PCM_Float_Wav_32.Get (WF, PCM);
            when Bit_Depth_64 =>
               PCM_Float_Wav_64.Get (WF, PCM);
         end case;
      else
         --  Always assume fixed-point PCM format
         case WF.Wave_Format.Bits_Per_Sample is
            when Bit_Depth_8 =>
               WF.Set_Error (Wavefile_Error_Unsupported_Bit_Depth);
               PCM := (others => 0.0);
            when Bit_Depth_16 =>
               PCM_Fixed_Wav_16.Get (WF, PCM);
            when Bit_Depth_24 =>
               PCM_Fixed_Wav_24.Get (WF, PCM);
            when Bit_Depth_32 =>
               PCM_Fixed_Wav_32.Get (WF, PCM);
            when Bit_Depth_64 =>
               WF.Set_Error (Wavefile_Error_Unsupported_Bit_Depth);
               PCM := (others => 0.0);
         end case;
      end if;
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (WF   : in out Wavefile;
      PCM  :        PCM_MC_Sample) is
   begin
      if not WF.Is_Opened then
         WF.Set_Error (Wavefile_Error_File_Not_Open);
         return;
      end if;

      if not Is_Supported_Format (WF.Wave_Format) then
         WF.Set_Error (Wavefile_Error_Unsupported_Wavefile_Format);
         return;
      end if;

      if WF.Wave_Format.Is_Float_Format then
         case WF.Wave_Format.Bits_Per_Sample is
            when Bit_Depth_8 | Bit_Depth_16 | Bit_Depth_24 =>
               WF.Set_Error (Wavefile_Error_Unsupported_Bit_Depth);
               return;
            when Bit_Depth_32 =>
               PCM_Float_Wav_32.Put (WF, PCM);
            when Bit_Depth_64 =>
               PCM_Float_Wav_64.Put (WF, PCM);
         end case;
      else
         --  Always assume fixed-point PCM format
         case WF.Wave_Format.Bits_Per_Sample is
            when Bit_Depth_8 =>
               WF.Set_Error (Wavefile_Error_Unsupported_Bit_Depth);
               return;
            when Bit_Depth_16 =>
               PCM_Fixed_Wav_16.Put (WF, PCM);
            when Bit_Depth_24 =>
               PCM_Fixed_Wav_24.Put (WF, PCM);
            when Bit_Depth_32 =>
               PCM_Fixed_Wav_32.Put (WF, PCM);
            when Bit_Depth_64 =>
               WF.Set_Error (Wavefile_Error_Unsupported_Bit_Depth);
               return;
         end case;
      end if;

   end Put;

end Audio.Wavefiles.Generic_Float_PCM_IO;
