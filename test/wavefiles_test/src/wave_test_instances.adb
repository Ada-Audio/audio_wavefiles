------------------------------------------------------------------------------
--                                                                          --
--                               WAVEFILES                                  --
--                                                                          --
--                            Test application                              --
--                                                                          --
--  The MIT License (MIT)                                                   --
--                                                                          --
--  Copyright (c) 2020 Gustavo A. Hoffmann                                  --
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

with Audio.Wavefiles.Data_Types; use Audio.Wavefiles.Data_Types;

with Generic_Fixed_Wave_Test;
with Generic_Float_Wave_Test;

package body Wave_Test_Instances is

   PCM_Bits  : Positive := 32;
   PCM_Fixed : Boolean  := True;

   package Wave_Test_Fixed_8 is new Generic_Fixed_Wave_Test
     (PCM_Sample    => Wav_Fixed_8,
      PCM_MC_Sample => Wav_Buffer_Fixed_8);
   package Wave_Test_Fixed_16 is new Generic_Fixed_Wave_Test
     (PCM_Sample    => Wav_Fixed_16,
      PCM_MC_Sample => Wav_Buffer_Fixed_16);
   package Wave_Test_Fixed_24 is new Generic_Fixed_Wave_Test
     (PCM_Sample    => Wav_Fixed_24,
      PCM_MC_Sample => Wav_Buffer_Fixed_24);
   package Wave_Test_Fixed_32 is new Generic_Fixed_Wave_Test
     (PCM_Sample    => Wav_Fixed_32,
      PCM_MC_Sample => Wav_Buffer_Fixed_32);
   package Wave_Test_Fixed_64 is new Generic_Fixed_Wave_Test
     (PCM_Sample    => Wav_Fixed_64,
      PCM_MC_Sample => Wav_Buffer_Fixed_64);

   package Wave_Test_Float_32 is new Generic_Float_Wave_Test
     (PCM_Sample    => Wav_Float_32,
      PCM_MC_Sample => Wav_Buffer_Float_32);
   package Wave_Test_Float_64 is new Generic_Float_Wave_Test
     (PCM_Sample    => Wav_Float_64,
      PCM_MC_Sample => Wav_Buffer_Float_64);
   package Wave_Test_Float_128 is new Generic_Float_Wave_Test
     (PCM_Sample    => Wav_Float_128,
      PCM_MC_Sample => Wav_Buffer_Float_128);

   Proc_Display_Info_File : access procedure (File_In : String)
     := Wave_Test_Fixed_32.Display_Info_File'Access;

   Proc_Copy_File : access procedure
     (File_In        : String;
      File_Out       : String) := Wave_Test_Fixed_32.Copy_File'Access;

   Proc_Compare_Files : access procedure
     (File_Ref    : String;
      File_DUT    : String) := Wave_Test_Fixed_32.Compare_Files'Access;

   Proc_Diff_Files : access procedure
     (File_Ref       : String;
      File_DUT       : String;
      File_Diff      : String) := Wave_Test_Fixed_32.Diff_Files'Access;

   Proc_Mix_Files : access procedure
     (File_Ref        : String;
      File_DUT        : String;
      File_Mix        : String) := Wave_Test_Fixed_32.Mix_Files'Access;

   -----------------------
   -- Display_Info_File --
   -----------------------

   procedure Display_Info_File (File_In : String) is
   begin
      Proc_Display_Info_File (File_In);
   end Display_Info_File;

   ---------------
   -- Copy_File --
   ---------------

   procedure Copy_File
     (File_In         : String;
      File_Out        : String) is
   begin
      Proc_Copy_File (File_In, File_Out);
   end Copy_File;

   -------------------
   -- Compare_Files --
   -------------------

   procedure Compare_Files
     (File_Ref    : String;
      File_DUT    : String) is
   begin
      Proc_Compare_Files (File_Ref, File_DUT);
   end Compare_Files;

   ----------------
   -- Diff_Files --
   ----------------

   procedure Diff_Files
     (File_Ref       : String;
      File_DUT       : String;
      File_Diff      : String) is
   begin
      Proc_Diff_Files (File_Ref, File_DUT, File_Diff);
   end Diff_Files;

   ---------------
   -- Mix_Files --
   ---------------

   procedure Mix_Files
     (File_Ref        : String;
      File_DUT        : String;
      File_Mix        : String) is
   begin
      Proc_Mix_Files (File_Ref, File_DUT, File_Mix);
   end Mix_Files;

   -------------------------
   -- Set_Test_Procedures --
   -------------------------

   procedure Set_Test_Procedures (Bits   : Positive;
                                  Fixed  : Boolean;
                                  Status : out Boolean)
   is
      procedure Set_It;

      procedure Set_It is
      begin
         Status    := True;
         PCM_Bits  := Bits;
         PCM_Fixed := Fixed;
      end Set_It;
   begin
      Status := False;

      case Fixed is
         when False =>  -- Floating-point PCM buffer
            case Bits is
            when 32 =>
               Proc_Display_Info_File
                 := Wave_Test_Float_32.Display_Info_File'Access;
               Proc_Copy_File
                 := Wave_Test_Float_32.Copy_File'Access;
               Proc_Compare_Files
                 := Wave_Test_Float_32.Compare_Files'Access;
               Proc_Diff_Files
                 := Wave_Test_Float_32.Diff_Files'Access;
               Proc_Mix_Files
                 := Wave_Test_Float_32.Mix_Files'Access;
               Set_It;
            when 64 =>
               Proc_Display_Info_File
                 := Wave_Test_Float_64.Display_Info_File'Access;
               Proc_Copy_File
                 := Wave_Test_Float_64.Copy_File'Access;
               Proc_Compare_Files
                 := Wave_Test_Float_64.Compare_Files'Access;
               Proc_Diff_Files
                 := Wave_Test_Float_64.Diff_Files'Access;
               Proc_Mix_Files
                 := Wave_Test_Float_64.Mix_Files'Access;
               Set_It;
            when 128 =>
               Proc_Display_Info_File
                 := Wave_Test_Float_128.Display_Info_File'Access;
               Proc_Copy_File
                 := Wave_Test_Float_128.Copy_File'Access;
               Proc_Compare_Files
                 := Wave_Test_Float_128.Compare_Files'Access;
               Proc_Diff_Files
                 := Wave_Test_Float_128.Diff_Files'Access;
               Proc_Mix_Files
                 := Wave_Test_Float_128.Mix_Files'Access;
               Set_It;
            when others =>
               null;
            end case;
         when True =>   -- Fixed-point PCM buffer
            case Bits is
            when 8 =>
               Proc_Display_Info_File
                 := Wave_Test_Fixed_8.Display_Info_File'Access;
               Proc_Copy_File
                 := Wave_Test_Fixed_8.Copy_File'Access;
               Proc_Compare_Files
                 := Wave_Test_Fixed_8.Compare_Files'Access;
               Proc_Diff_Files
                 := Wave_Test_Fixed_8.Diff_Files'Access;
               Proc_Mix_Files
                 := Wave_Test_Fixed_8.Mix_Files'Access;
               Set_It;
            when 16 =>
               Proc_Display_Info_File
                 := Wave_Test_Fixed_16.Display_Info_File'Access;
               Proc_Copy_File
                 := Wave_Test_Fixed_16.Copy_File'Access;
               Proc_Compare_Files
                 := Wave_Test_Fixed_16.Compare_Files'Access;
               Proc_Diff_Files
                 := Wave_Test_Fixed_16.Diff_Files'Access;
               Proc_Mix_Files
                 := Wave_Test_Fixed_16.Mix_Files'Access;
               Set_It;
            when 24 =>
               Proc_Display_Info_File
                 := Wave_Test_Fixed_24.Display_Info_File'Access;
               Proc_Copy_File
                 := Wave_Test_Fixed_24.Copy_File'Access;
               Proc_Compare_Files
                 := Wave_Test_Fixed_24.Compare_Files'Access;
               Proc_Diff_Files
                 := Wave_Test_Fixed_24.Diff_Files'Access;
               Proc_Mix_Files
                 := Wave_Test_Fixed_24.Mix_Files'Access;
               Set_It;
            when 32 =>
               Proc_Display_Info_File
                 := Wave_Test_Fixed_32.Display_Info_File'Access;
               Proc_Copy_File
                 := Wave_Test_Fixed_32.Copy_File'Access;
               Proc_Compare_Files
                 := Wave_Test_Fixed_32.Compare_Files'Access;
               Proc_Diff_Files
                 := Wave_Test_Fixed_32.Diff_Files'Access;
               Proc_Mix_Files
                 := Wave_Test_Fixed_32.Mix_Files'Access;
               Set_It;
            when 64 =>
               Proc_Display_Info_File
                 := Wave_Test_Fixed_64.Display_Info_File'Access;
               Proc_Copy_File
                 := Wave_Test_Fixed_64.Copy_File'Access;
               Proc_Compare_Files
                 := Wave_Test_Fixed_64.Compare_Files'Access;
               Proc_Diff_Files
                 := Wave_Test_Fixed_64.Diff_Files'Access;
               Proc_Mix_Files
                 := Wave_Test_Fixed_64.Mix_Files'Access;
               Set_It;
            when others =>
               null;
            end case;
      end case;
   end Set_Test_Procedures;

   --------------
   -- Get_Bits --
   --------------

   function Get_Bits return Positive is (PCM_Bits);

   --------------
   -- Is_Fixed --
   --------------

   function Is_Fixed return Boolean is (PCM_Fixed);

end Wave_Test_Instances;
