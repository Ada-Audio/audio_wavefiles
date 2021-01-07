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

with Ada.Command_Line;                 use Ada.Command_Line;
with Ada.Text_IO;                      use Ada.Text_IO;

with System.Multiprocessors;
with System.Multiprocessors.Dispatching_Domains;

with Wavefile_Benchmarking;            use Wavefile_Benchmarking;
with Wavefile_Benchmarking.Statistics; use Wavefile_Benchmarking.Statistics;

procedure Simple_Benchmarking is

   CPU_MHz : Float := 2650.0;

   Verbose : constant Boolean := True;

   Results : Wavefile_Benchmark_Infos (1 .. 5);

   task type Wavefile_Benchmark
     with CPU => 1
   is
      entry Finish (Res : out Wavefile_Benchmark_Infos);
   end Wavefile_Benchmark;

   task body Wavefile_Benchmark is
      Local_Results : Wavefile_Benchmark_Infos (Results'Range);
   begin
      if Verbose then
         Display_Current_CPU : declare
            use System.Multiprocessors;
            use System.Multiprocessors.Dispatching_Domains;
         begin
            Put_Line ("Current CPU : " & CPU_Range'Image (Get_CPU));
         end Display_Current_CPU;
      end if;

      Benchm_CPU_Time (CPU_MHz, Local_Results);

      accept Finish (Res : out Wavefile_Benchmark_Infos) do
         Res := Local_Results;
      end Finish;
   end Wavefile_Benchmark;

   Benchmark_Using_Tasking : constant Boolean := False;

begin
   if Argument_Count >= 1 then
      CPU_MHz := Float'Value (Argument (1));
   end if;

   if Verbose then
      Put_Line ("Using CPU @ " & Float'Image (CPU_MHz) & " MHz");
   end if;

   if Benchmark_Using_Tasking then
      declare
         Wav_Benchmark : Wavefile_Benchmark;
      begin
         Wav_Benchmark.Finish (Results);
      end;
   else
      Benchm_CPU_Time (CPU_MHz, Results);
   end if;

   Calc_Statistics : declare
      Stats : Wavefile_Benchmark_Statistics;
   begin
      Calculate_Statistics (Results, Stats);
      Display (Stats);
   end Calc_Statistics;

end Simple_Benchmarking;
