------------------------------------------------------------------------------
--                                                                          --
--                               WAVEFILES                                  --
--                                                                          --
--                         Wavefile benchmarking                            --
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

with Ada.Text_IO; use Ada.Text_IO;

package body Time_Span_Conversions is

   Display_Debug_Info : constant Boolean := False;

   ------------
   -- To_MHz --
   ------------

   function To_MHz (Elapsed_Time : Time_Span;
                    CPU_MHz      : Float;
                    Factor       : Long_Long_Float) return Float
   is
      Dur : constant Long_Long_Float :=
              Long_Long_Float (To_Duration (Elapsed_Time));
      --  Elapsed time in seconds
   begin
      if Display_Debug_Info then
         Put_Line ("Dur:    " & Long_Long_Float'Image (Dur));
         Put_Line ("Factor: " & Long_Long_Float'Image (Factor));
      end if;

      return Float
        (Long_Long_Float (CPU_MHz) * Dur / Factor);
   end To_MHz;

   ------------
   -- To_kHz --
   ------------

   function To_kHz (Elapsed_Time : Time_Span;
                    CPU_MHz      : Float;
                    Factor       : Long_Long_Float) return Float is
     (To_MHz (Elapsed_Time, CPU_MHz, Factor) * 10.0 ** 3);

   --------------------
   -- To_Miliseconds --
   --------------------

   function To_Miliseconds (Elapsed_Time : Time_Span) return Float is

      T : constant Float := Float (To_Duration (Elapsed_Time));
   begin
      return T * 10.0 ** 3;
   end To_Miliseconds;

   --------------------
   -- To_Nanoseconds --
   --------------------

   function To_Nanoseconds (Elapsed_Time : Time_Span) return Float is

      T : constant Float := Float (To_Duration (Elapsed_Time));
   begin
      return T * 10.0 ** 9;
   end To_Nanoseconds;

end Time_Span_Conversions;
