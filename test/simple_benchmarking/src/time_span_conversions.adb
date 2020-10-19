with Ada.Text_IO; use Ada.Text_IO;

package body Time_Span_Conversions is

   Display_Debug_Info : constant Boolean := False;

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

   function To_kHz (Elapsed_Time : Time_Span;
                    CPU_MHz      : Float;
                    Factor       : Long_Long_Float) return Float is
     (To_MHz (Elapsed_Time, CPU_MHz, Factor) * 10.0 ** 3);

   function To_Miliseconds (Elapsed_Time : Time_Span) return Float is

      T : constant Float := Float (To_Duration (Elapsed_Time));
   begin
      return T * 10.0 ** 3;
   end To_Miliseconds;

   function To_Nanoseconds (Elapsed_Time : Time_Span) return Float is

      T : constant Float := Float (To_Duration (Elapsed_Time));
   begin
      return T * 10.0 ** 9;
   end To_Nanoseconds;

end Time_Span_Conversions;
