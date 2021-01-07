------------------------------------------------------------------------------
--                                                                          --
--                               WAVEFILES                                  --
--                                                                          --
--                            Test application                              --
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

with Ada.Command_Line;    use Ada.Command_Line;
with Ada.Text_IO;         use Ada.Text_IO;

with Wave_Test_Instances; use Wave_Test_Instances;

procedure Wavefiles_Test is
   Command_Line_OK : Boolean := False;

   procedure Print_Usage;

   procedure Print_Usage is
   begin
      Put_Line ("Usage: " & Command_Name & " <command>");
      New_Line;
      Put_Line ("   info    <input_wavefile>");
      Put_Line ("   copy    <input_wavefile> <output_wavefile>");
      Put_Line ("   compare <ref_wavefile>   <dut_wavefile>");
      Put_Line ("   diff    <ref_wavefile>   <dut_wavefile>     "
                & "<diff_wavefile>");
      Put_Line ("   mix     <wavefile_1>     <wavefile_1>       "
                & "<mix_wavefile>");
   end Print_Usage;

begin
   if Argument_Count >= 1 then
      if Argument (1) = "info" and then Argument_Count = 2 then
         Command_Line_OK := True;
         Put_Line ("Information from: " & Argument (2));
         Display_Info_File (Argument (2));
      elsif Argument (1) = "copy" and then Argument_Count = 3 then
         Command_Line_OK := True;
         Put_Line ("Copying from: " & Argument (2));
         Put_Line ("Copying to:   " & Argument (3));
         Copy_File (Argument (2), Argument (3));
      elsif Argument (1) = "compare" and then Argument_Count = 3 then
         Command_Line_OK := True;
         Put_Line ("Reference: " & Argument (2));
         Put_Line ("DUT:       " & Argument (3));
         Compare_Files (Argument (2), Argument (3));
      elsif Argument (1) = "diff" and then Argument_Count = 4 then
         Command_Line_OK := True;
         Put_Line ("Reference: " & Argument (2));
         Put_Line ("DUT:       " & Argument (3));
         Put_Line ("Diff:      " & Argument (4));
         Diff_Files (Argument (2), Argument (3), Argument (4));
      elsif Argument (1) = "mix" and then Argument_Count = 4 then
         Command_Line_OK := True;
         Put_Line ("Wavefile #1: " & Argument (2));
         Put_Line ("Wavefile #2: " & Argument (3));
         Put_Line ("Mix file:    " & Argument (4));
         Mix_Files (Argument (2), Argument (3), Argument (4));
      end if;
   end if;

   if not Command_Line_OK then
      Print_Usage;
   end if;

end Wavefiles_Test;
