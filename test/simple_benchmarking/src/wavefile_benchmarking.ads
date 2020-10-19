package Wavefile_Benchmarking is

   type Wavefile_Benchmark_Routine is (Wavefile_Read_Benchmark,
                                       Wavefile_Write_Benchmark);

   type Wavefile_Benchmark_kHz is
     array (Wavefile_Benchmark_Routine'Range) of Float;

   type Wavefile_Benchmark_Infos is array (Positive range <>) of
     Wavefile_Benchmark_kHz;

   function Benchm_CPU_Time (CPU_MHz : Float) return Wavefile_Benchmark_kHz;

   procedure Benchm_CPU_Time (CPU_MHz :     Float;
                              Results : out Wavefile_Benchmark_Infos);

end Wavefile_Benchmarking;
