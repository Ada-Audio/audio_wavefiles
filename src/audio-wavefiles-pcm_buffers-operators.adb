-------------------------------------------------------------------------------
--
--                                WAVEFILES
--
--                          Operators for PCM buffers
--
--  The MIT License (MIT)
--
--  Copyright (c) 2015 Gustavo A. Hoffmann
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

package body Audio.Wavefiles.PCM_Buffers.Operators is

   function "+" (Left, Right : PCM_Buffer) return PCM_Buffer is
      PCM_Mix : PCM_Buffer (Positive'Max (Left.Channels, Right.Channels));
   begin
      PCM_Mix.Info.Samples_Valid := Natural'Max (Left.Info.Samples_Valid,
                                             Right.Info.Samples_Valid);

      for I in 1 .. PCM_Mix.Channels loop
         if Left.Info.Active (I) or Right.Info.Active (I) then
            PCM_Mix.Info.Active (I) := True;
            for J in 1 .. PCM_Mix.Info.Samples_Valid loop
               Reset (PCM_Mix.Audio_Data (I) (J));
            end loop;

            if Left.Info.Active (I) then
               for J in 1 .. Left.Info.Samples_Valid loop
                  PCM_Mix.Audio_Data (I) (J) := Left.Audio_Data (I) (J);
               end loop;
            end if;

            if Right.Info.Active (I) then
               for J in 1 .. Right.Info.Samples_Valid loop
                  PCM_Mix.Audio_Data (I) (J) := PCM_Mix.Audio_Data (I) (J)
                    + Right.Audio_Data (I) (J);
               end loop;
            end if;
         else
            PCM_Mix.Info.Active (I) := False;
         end if;
      end loop;

      return PCM_Mix;
   end "+";

   function "-" (Left, Right : PCM_Buffer) return PCM_Buffer is
      PCM_Mix : PCM_Buffer (Positive'Max (Left.Channels, Right.Channels));
   begin
      PCM_Mix.Info.Samples_Valid := Natural'Max (Left.Info.Samples_Valid,
                                             Right.Info.Samples_Valid);

      for I in 1 .. PCM_Mix.Channels loop
         if Left.Info.Active (I) or Right.Info.Active (I) then
            PCM_Mix.Info.Active (I) := True;
            for J in 1 .. PCM_Mix.Info.Samples_Valid loop
               Reset (PCM_Mix.Audio_Data (I) (J));
            end loop;

            if Left.Info.Active (I) then
               for J in 1 .. Left.Info.Samples_Valid loop
                  PCM_Mix.Audio_Data (I) (J) := Left.Audio_Data (I) (J);
               end loop;
            end if;

            if Right.Info.Active (I) then
               for J in 1 .. Right.Info.Samples_Valid loop
                  PCM_Mix.Audio_Data (I) (J) := PCM_Mix.Audio_Data (I) (J)
                    - Right.Audio_Data (I) (J);
               end loop;
            end if;
         else
            PCM_Mix.Info.Active (I) := False;
         end if;
      end loop;

      return PCM_Mix;
   end "-";

   function "*"
     (Left  : PCM_Buffer;
      Right : PCM_Type)
      return PCM_Buffer
   is
      PCM_Out : PCM_Buffer (Left.Channels);
   begin
      PCM_Out.Info.Samples_Valid := Left.Info.Samples_Valid;

      for I in 1 .. PCM_Out.Channels loop
         PCM_Out.Info.Active (I) := Left.Info.Active (I);
         if PCM_Out.Info.Active (I) then
            for J in 1 .. Left.Info.Samples_Valid loop
               PCM_Out.Audio_Data (I) (J) := Left.Audio_Data (I) (J) * Right;
            end loop;
         end if;
      end loop;

      return PCM_Out;
   end "*";

end Audio.Wavefiles.PCM_Buffers.Operators;
