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


with Ada.Assertions;

with Audio.Wavefiles.Read;
with Audio.Wavefiles.Write;

package body Audio.Fixed_PCM_Buffers is

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
               PCM_Out.Audio_Data (I) (J) := 0.0;
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

   ----------------------------------------------------------------------------
   --  OPERATORS
   ----------------------------------------------------------------------------

   function "+" (Left, Right : PCM_Buffer) return PCM_Buffer is
      PCM_Mix : PCM_Buffer (Positive'Max (Left.Channels, Right.Channels));
   begin
      PCM_Mix.Info.Samples_Valid := Natural'Max (Left.Info.Samples_Valid,
                                             Right.Info.Samples_Valid);

      for I in 1 .. PCM_Mix.Channels loop
         if Left.Info.Active (I) or Right.Info.Active (I) then
            PCM_Mix.Info.Active (I) := True;
            for J in 1 .. PCM_Mix.Info.Samples_Valid loop
               PCM_Mix.Audio_Data (I) (J) := 0.0;
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
               PCM_Mix.Audio_Data (I) (J) := 0.0;
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

   ----------------------------------------------------------------------------
   --  WAVEFILE I/O
   ----------------------------------------------------------------------------

   type MC_Samples is array (Positive range <>) of PCM_Type;

   function Get_Sample (Buf : PCM_Buffer;
                        Ch  : Positive;
                        I   : Positive) return MC_Samples;

   procedure Get
     (WF  : in out Wavefile;
      Buf : in out PCM_Buffer;
      EOF :    out Boolean)
   is
      function Get is new Audio.Wavefiles.Read.Get_Fixed
        (PCM_Type => PCM_Type,
         MC_Samples => MC_Samples);

      Ch : constant Positive := Audio.Wavefiles.Get_Channels (WF);
   begin
      Buf.Info.Samples_Valid := 0;

      for I in 1 .. Audio.Fixed_PCM_Buffers.Samples loop
         declare
            P : constant MC_Samples := Get (WF);
         begin
            Buf.Info.Samples_Valid := Buf.Info.Samples_Valid + 1;
            for J in P'Range loop
               Buf.Audio_Data (J) (I) := P (J);
            end loop;
         end;

         EOF := Audio.Wavefiles.Read.Is_EOF (WF);
         exit when EOF;
      end loop;
      for J in 1 .. Ch loop
         Buf.Info.Active (J) := True;
      end loop;
      for J in Ch + 1 .. Buf.Channels loop
         Buf.Info.Active (J) := False;
      end loop;
   end Get;

   function Get_Sample (Buf : PCM_Buffer;
                        Ch  : Positive;
                        I   : Positive) return MC_Samples is
   begin
      return P : MC_Samples (1 .. Ch) do
         for J in P'Range loop
            P (J) := Buf.Audio_Data (J) (I);
         end loop;
      end return;
   end Get_Sample;

   procedure Put
     (WF  : in out Wavefile;
      Buf : in PCM_Buffer)
   is
      procedure Put is new Audio.Wavefiles.Write.Put_Fixed
        (PCM_Type   => PCM_Type,
         MC_Samples => MC_Samples);

      Ch : constant Positive := Audio.Wavefiles.Get_Channels (WF);
   begin
      Ada.Assertions.Assert (Ch <= Buf.Channels,
                             "Unsufficient number of channel in buffer");

      for I in 1 .. Buf.Info.Samples_Valid loop
         declare
            P : constant MC_Samples := Get_Sample (Buf, Ch, I);
         begin
            Put (WF, P);
         end;
      end loop;

   end Put;

end Audio.Fixed_PCM_Buffers;
