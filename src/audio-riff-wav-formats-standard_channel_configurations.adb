------------------------------------------------------------------------------
--                                                                          --
--                         AUDIO / RIFF / WAV                               --
--                                                                          --
--              Standard channel configurations for wavefiles               --
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

package body Audio.RIFF.Wav.Formats.Standard_Channel_Configurations is

   -----------------------------------
   -- Guessed_Channel_Configuration --
   -----------------------------------

   function Guessed_Channel_Configuration
     (Number_Of_Channels : Positive) return Channel_Configuration is
   begin
      case Number_Of_Channels is
         when  1 => return Channel_Config_1_0;
         when  2 => return Channel_Config_2_0;
         when  3 => return Channel_Config_3_0;
         when  4 => return Channel_Config_4_0;
         when  5 => return Channel_Config_5_0;
         when  6 => return Channel_Config_5_1;
         when  7 => return Channel_Config_7_0;
         when  8 => return Channel_Config_7_1;
         when  9 => return Channel_Config_7_1_BC;
         when 10 => return Channel_Config_5_1_4;
            --      return Channel_Config_7_1_2;
         when 11 => return Channel_Config_7_0_4;
         when 12 => return Channel_Config_7_1_4;
         when others => return Channel_Config_Empty;
      end case;
   end Guessed_Channel_Configuration;

end Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;
