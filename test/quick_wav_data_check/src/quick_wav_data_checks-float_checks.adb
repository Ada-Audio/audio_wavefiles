-------------------------------------------------------------------------------
--
--                                WAVEFILES
--
--                         Quick Wave Data I/O Check
--
--  The MIT License (MIT)
--
--  Copyright (c) 2020 Gustavo A. Hoffmann
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

with Ada.Text_IO;                          use Ada.Text_IO;
with Ada.Strings.Fixed;                    use Ada.Strings.Fixed;

with Audio.Wavefiles;                      use Audio.Wavefiles;
with Audio.Wavefiles.Data_Types;           use Audio.Wavefiles.Data_Types;

with Audio.Wavefiles.Data_Types.Text_IO;
use  Audio.Wavefiles.Data_Types.Text_IO;

with Audio.Wavefiles.Generic_Float_PCM_IO;
with Audio.RIFF;                           use Audio.RIFF;
with Interfaces;                           use Interfaces;

package body Quick_Wav_Data_Checks.Float_Checks is

   Verbose : constant Boolean := False;

   type Integer_64 is range -2 ** 63 .. 2 ** 63 - 1
     with Size => 64;

   package Fixed_64_PCM_As_Integer_Text_IO is new
     Ada.Text_IO.Integer_IO (Integer_64);

   subtype PCM_Sample is Wav_Float_128;
   subtype PCM_Buffer is Wav_Buffer_Float_128;

   package PCM_Sample_Text_IO renames Wav_Float_128_Text_IO;

   package PCM_IO is new Audio.Wavefiles.Generic_Float_PCM_IO
     (PCM_Sample    => PCM_Sample,
      PCM_MC_Sample => PCM_Buffer);
   use PCM_IO;

   PCM_Ref : constant PCM_Buffer (1 .. 11) :=
               (1      =>   2#1.0#e-0,
                2      =>  -2#1.0#e-0,
                3      =>   2#1.0#e-1,
                4      =>  -2#1.0#e-1,
                5      =>  (2#1.0#e-0 + 2#1.0#e-23),
                6      => (-2#1.0#e-0 - 2#1.0#e-23),
                7      =>  (2#1.0#e-0 + 2#1.0#e-52),
                8      => (-2#1.0#e-0 - 2#1.0#e-52),
                9      =>  (2#1.0#e-0 + 2#1.0#e-56),
                10     => (-2#1.0#e-0 - 2#1.0#e-56),
                others => 0.0);

   --   32-bit Float:  23-bit mantissa
   --   64-bit Float:  52-bit mantissa
   --  128-bit Float: 112-bit mantissa

   PCM_Ref_32 : constant PCM_Buffer (1 .. 11) :=
                  (1      =>   2#1.0#e-0,
                   2      =>  -2#1.0#e-0,
                   3      =>   2#1.0#e-1,
                   4      =>  -2#1.0#e-1,
                   5      =>  (2#1.0#e-0 + 2#1.0#e-23),
                   6      => (-2#1.0#e-0 - 2#1.0#e-23),
                   7      =>  (2#1.0#e-0),
                   8      => (-2#1.0#e-0),
                   9      =>  (2#1.0#e-0),
                   10     => (-2#1.0#e-0),
                   others => 0.0);

   PCM_Ref_64 : constant PCM_Buffer (1 .. 11) :=
                  (1      =>   2#1.0#e-0,
                   2      =>  -2#1.0#e-0,
                   3      =>   2#1.0#e-1,
                   4      =>  -2#1.0#e-1,
                   5      =>  (2#1.0#e-0 + 2#1.0#e-23),
                   6      => (-2#1.0#e-0 - 2#1.0#e-23),
                   7      =>  (2#1.0#e-0 + 2#1.0#e-52),
                   8      => (-2#1.0#e-0 - 2#1.0#e-52),
                   9      =>  (2#1.0#e-0),
                   10     => (-2#1.0#e-0),
                   others => 0.0);

   procedure Display_PCM_Vals
     (PCM_Vals : PCM_Buffer;
      Header   : String);

   procedure Write_PCM_Vals
     (WF       : in out Wavefile;
      PCM_Vals :        PCM_Buffer);

   function PCM_Data_Is_OK
     (PCM_Ref, PCM_DUT : PCM_Buffer) return Boolean;

   function PCM_Data_Is_OK
     (Test_Bits : Unsigned_16;
      PCM_DUT   : PCM_Buffer) return Boolean;

   function Wav_IO_OK_For_Audio_Resolution
     (Test_Bits          : Unsigned_16;
      Wav_Test_File_Name : String) return Boolean;

   procedure Display_Info
     (WF     : Wavefile;
      Header : String);

   procedure Write_Wavefile
     (Wav_File_Name : String;
      Test_Bits     : Unsigned_16);

   procedure Read_Wavefile
     (Wav_File_Name :     String;
      PCM_DUT       : out PCM_Buffer);

   procedure Display_PCM_Vals (PCM_Vals : PCM_Buffer;
                               Header   : String)
   is
      Display_Integer_Value : constant Boolean := False;
   begin
      Put_Line (Header);
      for Sample_Count in PCM_Vals'Range loop
         declare
            PCM_Integer : array (1 .. 2) of Integer_64
              with
                Address => PCM_Vals (Sample_Count)'Address,
              Size => 128;
         begin

            Put ("    Val: ");
            PCM_Sample_Text_IO.Put (Item => PCM_Vals (Sample_Count),
                                    Fore => 5,
                                    Aft  => 60,
                                    Exp  => 5);
            if Display_Integer_Value then
               Put (" - ");
               for I in PCM_Integer'Range loop
                  Fixed_64_PCM_As_Integer_Text_IO.Put
                    (PCM_Integer (I), Base => 2, Width => 68);
               end loop;
            end if;
            New_Line;
         end;
      end loop;
   end Display_PCM_Vals;

   procedure Write_PCM_Vals (WF       : in out Wavefile;
                             PCM_Vals : PCM_Buffer) is
      PCM_Buf : PCM_Buffer (1 .. Number_Of_Channels (WF));
   begin
      for Sample_Count in PCM_Vals'Range loop
         for J in PCM_Buf'Range loop
            PCM_Buf (J) := PCM_Vals (Sample_Count);
         end loop;
         Put (WF, PCM_Buf);
      end loop;
   end Write_PCM_Vals;

   type Bits_Per_Sample_List is array (Positive range <>) of Unsigned_16;
   Test_Bits_Per_Sample : constant Bits_Per_Sample_List := (32, 64);

   procedure Display_Info
     (WF     : Wavefile;
      Header : String)
   is
      Separator        : constant String
        := "===========================================================";
   begin
      Put_Line (Separator);
      Put_Line (Header);
      Display_Info (WF);
      Put_Line (Separator);
   end Display_Info;

   procedure Write_Wavefile
     (Wav_File_Name : String;
      Test_Bits     : Unsigned_16)
   is
      WF_Out      : Wavefile;
      Wave_Format : Wave_Format_Extensible;
   begin
      Set_Default (Wave_Format);
      Wave_Format.Bits_Per_Sample := Test_Bits;
      Wave_Format.Sub_Format := GUID_IEEE_Float;

      Open (WF_Out, Out_File, Wav_File_Name, Wave_Format);

      Write_PCM_Vals (WF_Out, PCM_Ref);

      Close (WF_Out);
   end Write_Wavefile;

   procedure Read_Wavefile
     (Wav_File_Name :     String;
      PCM_DUT       : out PCM_Buffer)
   is
      WF_In       : Wavefile;
      Wave_Format : Wave_Format_Extensible;
      EOF         : Boolean;
      Samples     : Integer := 0;
   begin
      Open (WF_In, In_File, Wav_File_Name, Wave_Format);

      if Verbose then
         Display_Info (WF_In, "Input File:");
      end if;

      Samples := 0;
      PCM_DUT := (others => 0.0);
      loop
         Samples := Samples + 1;
         --  Put ("[" & Integer'Image (Samples) & "]");

         declare
            PCM_Buf : constant PCM_Buffer := Get (WF_In);
         begin
            PCM_DUT (Samples) := PCM_Buf (PCM_Buf'First);
         end;
         EOF := Is_EOF (WF_In);

         exit when EOF;
      end loop;

      if Verbose then
         Display_PCM_Vals (PCM_Ref,
                           "Constant PCM values for testing:");

         Display_PCM_Vals (PCM_DUT,
                           "Read PCM values:");
      end if;

      Close (WF_In);
   end Read_Wavefile;

   function PCM_Data_Is_OK
     (PCM_Ref, PCM_DUT : PCM_Buffer) return Boolean
   is
      Success : Boolean := True;
   begin
      for I in PCM_DUT'Range loop
         if PCM_DUT (I) /= PCM_Ref (I) then
            Put_Line ("- Error for check at index"
                      & Integer'Image (I));
            Success := False;
         end if;
      end loop;
      return Success;

   end PCM_Data_Is_OK;

   function PCM_Data_Is_OK
     (Test_Bits : Unsigned_16;
      PCM_DUT   : PCM_Buffer) return Boolean is
   begin
      case Test_Bits is
         when 32 =>
            return PCM_Data_Is_OK (PCM_Ref_32, PCM_DUT);
         when 64 =>
            return PCM_Data_Is_OK (PCM_Ref_64, PCM_DUT);
         when others =>
            Put_Line ("Unknown test for "
                      & Unsigned_16'Image (Test_Bits)
                      & " bits");
            return False;
      end case;
   end PCM_Data_Is_OK;

   function Wav_IO_OK_For_Audio_Resolution
     (Test_Bits          : Unsigned_16;
      Wav_Test_File_Name : String) return Boolean
   is
      Test_Bits_String : constant String
        := Trim (Unsigned_16'Image (Test_Bits), Ada.Strings.Both);
      Wav_File_Name    : constant String
        := Wav_Test_File_Name & "_" & Test_Bits_String & ".wav";
      Test_Message     : constant String
        := "Float " & Unsigned_16'Image (Test_Bits);
      PCM_DUT          : PCM_Buffer (PCM_Ref'Range);
      Success          : Boolean := True;
   begin
      Write_Wavefile (Wav_File_Name, Test_Bits);

      Read_Wavefile (Wav_File_Name, PCM_DUT);

      if PCM_Data_Is_OK (Test_Bits, PCM_DUT) then
         Put_Line ("PASS : " & Test_Message);
      else
         Put_Line ("FAIL : " & Test_Message);
         Success := False;
      end if;

      return Success;
   end Wav_IO_OK_For_Audio_Resolution;

   function Wav_IO_OK
     (Wav_Filename_Prefix : String) return Boolean
   is
      Wav_Test_File_Name : constant String
        := Wav_Filename_Prefix & "check_extremes_float";

      Success : Boolean := True;
   begin
      for Test_Bits of Test_Bits_Per_Sample loop
         if not Wav_IO_OK_For_Audio_Resolution (Test_Bits,
                                                Wav_Test_File_Name)
         then
            Success := False;
         end if;
      end loop;

      return Success;
   end Wav_IO_OK;

end Quick_Wav_Data_Checks.Float_Checks;
