-------------------------------------------------------------------------------
--
--                                WAVEFILES
--
--                             Test application
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

package body Gen_Float_PCM_Buffer_Ops is

   function "+" (PCM_Ref : MC_Samples;
                 PCM_DUT : MC_Samples)
                    return MC_Samples
   is
      Max_Last : constant Positive :=
                   Positive'Max (PCM_Ref'Last, PCM_DUT'Last);
      PCM_Sum  :          MC_Samples (1 .. Max_Last);
   begin
      for I in 1 .. Max_Last loop
         PCM_Sum (I) := PCM_Ref (I) + PCM_DUT (I);
      end loop;
      return PCM_Sum;
   end "+";

   function "-" (PCM_Ref : MC_Samples;
                 PCM_DUT : MC_Samples)
                    return MC_Samples
   is
      Max_Last : constant Positive :=
                   Positive'Max (PCM_Ref'Last, PCM_DUT'Last);
      PCM_Diff :          MC_Samples (1 .. Max_Last);
   begin
      for I in 1 .. Max_Last loop
         PCM_Diff (I) := PCM_Ref (I) - PCM_DUT (I);
      end loop;
      return PCM_Diff;
   end "-";

end Gen_Float_PCM_Buffer_Ops;
