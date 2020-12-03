------------------------------------------------------------------------------
--                                                                          --
--          THIS IS AN AUTOMATICALLY GENERATED FILE! DO NOT EDIT!           --
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

generic
#if NUM_TYPE'Defined and then (NUM_TYPE = "FLOAT") then
   type PCM_Sample is digits <>;
#else
   type PCM_Sample is delta <>;
#end if;
   type PCM_MC_Sample is array (Positive range <>) of PCM_Sample;
#if NUM_TYPE'Defined and then (NUM_TYPE = "FLOAT") then
package Generic_Float_Wave_Test is
#else
package Generic_Fixed_Wave_Test is
#end if;

   procedure Display_Info_File
     (File_In : String);

   procedure Copy_File
     (File_In         : String;
      File_Out        : String);

   procedure Compare_Files
     (File_Ref    : String;
      File_DUT    : String);

   procedure Diff_Files
     (File_Ref       : String;
      File_DUT       : String;
      File_Diff      : String);

   procedure Mix_Files
     (File_Ref        : String;
      File_DUT        : String;
      File_Mix        : String);

#if NUM_TYPE'Defined and then (NUM_TYPE = "FLOAT") then
end Generic_Float_Wave_Test;
#else
end Generic_Fixed_Wave_Test;
#end if;
