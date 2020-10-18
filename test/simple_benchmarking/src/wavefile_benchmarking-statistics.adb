with Ada.Text_IO; use Ada.Text_IO;

package body Wavefile_Benchmarking.Statistics is

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

   procedure Display (Stats : Wavefile_Benchmark_Statistics) is
      package F_IO is new Ada.Text_IO.Float_IO (Float);
   begin
      for B in Stats.Avg'Range loop
         Put ("Average Perf. " & B'Image & ": ");
         F_IO.Put (Item => Stats.Avg (B),
                   Fore => 5, Aft => 4, Exp => 0);
         Put (" kHz (per channel and per sample)");
         New_Line;
      end loop;
      for B in Stats.Max'Range loop
         Put ("Max Perf. " & B'Image & ": ");
         F_IO.Put (Item => Stats.Max (B),
                   Fore => 5, Aft => 4, Exp => 0);
         Put (" kHz (per channel and per sample)");
         New_Line;
      end loop;
      for B in Stats.Min'Range loop
         Put ("Min Perf. " & B'Image & ": ");
         F_IO.Put (Item => Stats.Min (B),
                   Fore => 5, Aft => 4, Exp => 0);
         Put (" kHz (per channel and per sample)");
         New_Line;
      end loop;
   end Display;

end Wavefile_Benchmarking.Statistics;
