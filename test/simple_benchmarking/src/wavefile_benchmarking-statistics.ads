package Wavefile_Benchmarking.Statistics is

   type Wavefile_Benchmark_Statistics is record
      Max : Wavefile_Benchmark_kHz := (Float'First, Float'First);
      Min : Wavefile_Benchmark_kHz := (Float'Last,  Float'Last);
      Avg : Wavefile_Benchmark_kHz := (0.0,         0.0);
   end record;

   procedure Calculate_Statistics
     (Raw_Info :     Wavefile_Benchmark_Infos;
      Results  : out Wavefile_Benchmark_Statistics);

   procedure Display (Stats : Wavefile_Benchmark_Statistics);

end Wavefile_Benchmarking.Statistics;
