-------------------------------------------------------------------------------
--
--                                WAVEFILES
--
--                       PCM buffers for wavefile I/O
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

with Ada.Text_IO; use Ada.Text_IO;

package body Audio.Wavefiles.PCM_Buffers is

   function Is_Channel_Active
     (PCM_Buf : PCM_Buffer;
      Channel : Positive) return Boolean is
   begin
      return PCM_Buf.Info.Active (Channel);
   end Is_Channel_Active;

   function Get_Number_Valid_Samples (PCM_Buf : PCM_Buffer) return Natural is
   begin
      return PCM_Buf.Info.Samples_Valid;
   end Get_Number_Valid_Samples;

   function "=" (Left, Right : PCM_Buffer) return Boolean is
      Verbose : constant Boolean := False;
   begin
      if Left.Channels /= Right.Channels then
         if Verbose then
            Put_Line ("Difference in num. of channels: "
                      & Integer'Image (Left.Channels) & " vs. "
                      & Integer'Image (Right.Channels));
         end if;
         return False;
      end if;
      if Left.Info.Samples_Valid /= Right.Info.Samples_Valid then
         if Verbose then
            Put_Line ("Difference in num. of valid samples: "
                      & Integer'Image (Left.Info.Samples_Valid) & " vs. "
                      & Integer'Image (Right.Info.Samples_Valid));
         end if;
         return False;
      end if;
      for I in 1 .. Left.Channels loop
         if Left.Info.Active (I) /= Right.Info.Active (I) then
            if Verbose then
               Put_Line ("Difference in Info.Active channel: "
                         & Boolean'Image (Left.Info.Active (I)) & " vs. "
                         & Boolean'Image (Right.Info.Active (I)));
            end if;
            return False;
         end if;
      end loop;

      for I in 1 .. Left.Channels loop
         if Left.Info.Active (I) then
            for J in 1 .. Left.Info.Samples_Valid loop
               if Left.Audio_Data (I) (J) /= Right.Audio_Data (I) (J) then
                  if Verbose then
                     Put_Line ("Difference in sample : "
                               & Integer'Image (I) & " vs. "
                               & Integer'Image (J));
                  end if;
                  return False;
               end if;
            end loop;
         end if;
      end loop;

      return True;
   end "=";

   function Perform
     (Left, Right : PCM_Buffer;
      Op          : PCM_Buffer_Op)
      return PCM_Buffer
   is
      PCM_Out : PCM_Buffer (Positive'Max (Left.Channels, Right.Channels));
   begin
      PCM_Out.Info.Samples_Valid := Natural'Max (Left.Info.Samples_Valid,
                                                 Right.Info.Samples_Valid);

      for I in 1 .. PCM_Out.Channels loop
         if Left.Info.Active (I) or Right.Info.Active (I) then
            PCM_Out.Info.Active (I) := True;
            for J in 1 .. PCM_Out.Info.Samples_Valid loop
               Reset (PCM_Out.Audio_Data (I) (J));
            end loop;

            if Left.Info.Active (I) then
               for J in 1 .. Left.Info.Samples_Valid loop
                  PCM_Out.Audio_Data (I) (J) := Left.Audio_Data (I) (J);
               end loop;
            end if;

            if Right.Info.Active (I) then
               for J in 1 .. Right.Info.Samples_Valid loop
                  PCM_Out.Audio_Data (I) (J) := Op (PCM_Out.Audio_Data (I) (J),
                                                    Right.Audio_Data (I) (J));
               end loop;
            end if;
         else
            PCM_Out.Info.Active (I) := False;
         end if;
      end loop;

      return PCM_Out;
   end Perform;

end Audio.Wavefiles.PCM_Buffers;
