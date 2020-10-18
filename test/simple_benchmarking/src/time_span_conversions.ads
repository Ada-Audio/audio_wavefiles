with Ada.Real_Time; use Ada.Real_Time;

package Time_Span_Conversions is

   function To_MHz (Elapsed_Time : Time_Span;
                    CPU_MHz      : Float;
                    Factor       : Long_Long_Float) return Float;

   function To_kHz (Elapsed_Time : Time_Span;
                    CPU_MHz      : Float;
                    Factor       : Long_Long_Float) return Float;

   function To_Miliseconds (Elapsed_Time : Time_Span) return Float;

   function To_Nanoseconds (Elapsed_Time : Time_Span) return Float;

end Time_Span_Conversions;
