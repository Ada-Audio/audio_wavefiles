-------------------------------------------------------------------------------
--
--                                WAVEFILES
--
--                      Wavefile data I/O operations
--
--  The MIT License (MIT)
--
--  Copyright (c) 2015 -- 2020 Gustavo A. Hoffmann
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

package body Audio.Wavefiles.Generic_Fixed_Wav_IO is

   procedure Read_Bytes (File_Access :     Ada.Streams.Stream_IO.Stream_Access;
                         Sample      : out Wav_Sample);
   procedure Write_Bytes (File_Access :    Ada.Streams.Stream_IO.Stream_Access;
                          Sample      :    Wav_Sample);

   procedure Read_Bytes (File_Access :     Ada.Streams.Stream_IO.Stream_Access;
                         Sample      : out Wav_Sample)
   is
      Bytes : Byte_Array (1 .. Sample'Size / 8)
        with Address => Sample'Address, Import, Volatile;

      Last_Valid_Byte : constant Long_Integer := Wav_Sample'Size / 8;

      use type Byte;
   begin
      Byte_Array'Read (File_Access,
                       Bytes (1 .. Wav_Sample'Size / 8));

      --  Account for sign bit in internal representation,
      --  which might not match the wavefile representation.
      if Sample'Size > Wav_Sample'Size then
         Bytes (Last_Valid_Byte + 1 .. Bytes'Last) :=
           (others => (if Bytes (Last_Valid_Byte) >= 16#80#
                       then 16#FF# else 16#00#));
      end if;
   end Read_Bytes;

   procedure Write_Bytes (File_Access :    Ada.Streams.Stream_IO.Stream_Access;
                          Sample      :    Wav_Sample)
   is
      Bytes : Byte_Array (1 .. Wav_Sample'Size / 8)
        with Address => Sample'Address, Import, Volatile;
   begin
      Byte_Array'Write (File_Access, Bytes);
   end Write_Bytes;

   function Get (WF  : in out Wavefile) return Wav_MC_Sample
   is
      N_Ch   : constant Positive := Number_Of_Channels (WF);
      Sample : Wav_Sample;

      use Ada.Streams.Stream_IO;

      Prev_File_Index  : constant Positive_Count := Index (WF.File)
        with Ghost;
      Expected_Byte_IO : constant Positive_Count
        := Positive_Count
          (To_Positive (WF.Wave_Format.Bits_Per_Sample) * N_Ch / 8)
        with Ghost;
   begin
      return Wav : Wav_MC_Sample (1 .. N_Ch) do
         for J in 1 .. N_Ch loop

            --  Patch for 24-bit wavefiles
            if Wav_Sample'Size = 24 then
               Read_Bytes (WF.File_Access, Sample);
            else
               Wav_Sample'Read (WF.File_Access, Sample);
            end if;

            Wav (J) := Sample;
            if Ada.Streams.Stream_IO.End_Of_File (WF.File) and then
              J < N_Ch
            then
               --  Cannot read data for all channels
               WF.Set_Error (Wavefile_Error_File_Too_Short);
            end if;
         end loop;

         WF.Sample_Pos.Current := WF.Sample_Pos.Current + 1;

         pragma Assert (Ada.Streams.Stream_IO.Index (WF.File) =
                          Prev_File_Index + Expected_Byte_IO);
      end return;
   end Get;

   procedure Put (WF  : in out Wavefile;
                  Wav :        Wav_MC_Sample) is
      N_Ch : constant Positive := Number_Of_Channels (WF);

      use Ada.Streams.Stream_IO;

      Prev_File_Index  : constant Positive_Count := Index (WF.File)
        with Ghost;
      Expected_Byte_IO : constant Positive_Count
        := Positive_Count
          (To_Positive (WF.Wave_Format.Bits_Per_Sample) * N_Ch / 8)
        with Ghost;
   begin
      if Wav_Sample'Size = 24 then
         for Sample of Wav loop
            Write_Bytes (WF.File_Access, Sample);
         end loop;
      else
         Wav_MC_Sample'Write (WF.File_Access, Wav);
      end if;

      WF.Sample_Pos := (Total   => WF.Sample_Pos.Total   + 1,
                        Current => WF.Sample_Pos.Current + 1);

      pragma Assert (Ada.Streams.Stream_IO.Index (WF.File) =
                       Prev_File_Index + Expected_Byte_IO);
   end Put;

end Audio.Wavefiles.Generic_Fixed_Wav_IO;
