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

package body Wavefile_Benchmarking.Statistics is

   --------------------------
   -- Calculate_Statistics --
   --------------------------

   procedure Calculate_Statistics
     (Raw_Info :     Wavefile_Benchmark_Infos;
      Results  : out Wavefile_Benchmark_Statistics)
   is
      Total : Wavefile_Benchmark_kHz := (0.0, 0.0);
      Res   : Wavefile_Benchmark_Statistics renames Results;
   begin
      for R of Raw_Info loop
         for B in Wavefile_Benchmark_Routine loop
            if Res.Max (B) < R (B) then
               Res.Max (B) := R (B);
            end if;

            if Res.Min (B) > R (B) then
               Res.Min (B) := R (B);
            end if;

            Total (B) := Total (B) + R (B);
         end loop;
      end loop;

      for B in Wavefile_Benchmark_Routine loop
         Res.Avg (B) := Total (B) / Float (Raw_Info'Length);
      end loop;
   end Calculate_Statistics;

   -------------
   -- Display --
   -------------

   procedure Display (Stats : Wavefile_Benchmark_Statistics) is
      package F_IO is new Ada.Text_IO.Float_IO (Float);
   begin
      for B in Stats.Avg'Range loop
         Put ("Average Perf. " & B'Image & ": ");
         F_IO.Put (Item => Stats.Avg (B),
                   Fore => 5, Aft => 4, Exp => 0);
         Put (" kHz (per channel and per 16-bit sample)");
         New_Line;
      end loop;
      for B in Stats.Max'Range loop
         Put ("Max Perf. " & B'Image & ": ");
         F_IO.Put (Item => Stats.Max (B),
                   Fore => 5, Aft => 4, Exp => 0);
         Put (" kHz (per channel and per 16-bit sample)");
         New_Line;
      end loop;
      for B in Stats.Min'Range loop
         Put ("Min Perf. " & B'Image & ": ");
         F_IO.Put (Item => Stats.Min (B),
                   Fore => 5, Aft => 4, Exp => 0);
         Put (" kHz (per channel and per 16-bit sample)");
         New_Line;
      end loop;
   end Display;

end Wavefile_Benchmarking.Statistics;
