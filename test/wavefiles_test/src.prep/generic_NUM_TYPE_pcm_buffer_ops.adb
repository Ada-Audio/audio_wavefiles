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

#if NUM_TYPE'Defined and then (NUM_TYPE = "FLOAT") then
package body Generic_Float_PCM_Buffer_Ops is
#else
package body Generic_Fixed_PCM_Buffer_Ops is
#end if;

   ---------
   -- "+" --
   ---------

   function "+" (PCM_Ref : PCM_MC_Sample;
                 PCM_DUT : PCM_MC_Sample)
                    return PCM_MC_Sample
   is
      Max_Last : constant Positive :=
                   Positive'Max (PCM_Ref'Last, PCM_DUT'Last);
      PCM_Sum  :          PCM_MC_Sample (1 .. Max_Last);
   begin
      for I in 1 .. Max_Last loop
         PCM_Sum (I) := PCM_Ref (I) + PCM_DUT (I);
      end loop;
      return PCM_Sum;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (PCM_Ref : PCM_MC_Sample;
                 PCM_DUT : PCM_MC_Sample)
                    return PCM_MC_Sample
   is
      Max_Last : constant Positive :=
                   Positive'Max (PCM_Ref'Last, PCM_DUT'Last);
      PCM_Diff :          PCM_MC_Sample (1 .. Max_Last);
   begin
      for I in 1 .. Max_Last loop
         PCM_Diff (I) := PCM_Ref (I) - PCM_DUT (I);
      end loop;
      return PCM_Diff;
   end "-";

#if NUM_TYPE'Defined and then (NUM_TYPE = "FLOAT") then
end Generic_Float_PCM_Buffer_Ops;
#else
end Generic_Fixed_PCM_Buffer_Ops;
#end if;
