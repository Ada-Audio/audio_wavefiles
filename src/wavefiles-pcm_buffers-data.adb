-------------------------------------------------------------------------------
--
--                                WAVEFILES
--
--                   Internal I/O operations for PCM buffers
--
-- The MIT License (MIT)
--
-- Copyright (c) 2015 Gustavo A. Hoffmann
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to
-- deal in the Software without restriction, including without limitation the
-- rights to use, copy, modify, merge, publish, distribute, sublicense, and /
-- or sell copies of the Software, and to permit persons to whom the Software
-- is furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
-- IN THE SOFTWARE.
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions;

package body Wavefiles.PCM_Buffers.Data is

   type PCM_Bit_Array is array (0 .. PCM_Type'Size - 1) of Boolean;
   pragma Pack (PCM_Bit_Array);

   type Audio_Res_Bit_Array is array (0 .. Audio_Res'Size - 1) of Boolean;
   pragma Pack (Audio_Res_Bit_Array);

   Bool_Image  : constant array (Boolean'Range) of Character := ('0', '1');
   Convert_Sample_Debug : constant Boolean := False;

   procedure Print_Sample_Read (Sample_In     : Audio_Res;
                                Sample_Out    : PCM_Type);

   procedure Print_Sample_Write (Sample_In     : PCM_Type;
                                 Sample_Out    : Audio_Res);

   function Convert_Sample (Sample : Audio_Res) return PCM_Type;

   function Convert_Sample (Sample : PCM_Type) return Audio_Res;


   procedure Print_Sample_Read (Sample_In     : Audio_Res;
                                Sample_Out    : PCM_Type) is
      Bits_In     : Audio_Res_Bit_Array;
      Bits_Out    : PCM_Bit_Array;
      for Bits_In'Address  use Sample_In'Address;
      for Bits_Out'Address use Sample_Out'Address;
   begin
      Put_Line ("In (Wav):  " & Integer'Image (Integer (Sample_In)));
      for K in reverse Audio_Res_Bit_Array'Range loop
         Put (Bool_Image (Bits_In (K)));
      end loop;
      New_Line;

      Put_Line ("Out (Buf): " & Float'Image (Float (Sample_Out)));
      for K in reverse PCM_Bit_Array'Range loop
         Put (Bool_Image (Bits_Out (K)));
      end loop;
      New_Line;
   end Print_Sample_Read;


   procedure Print_Sample_Write (Sample_In     : PCM_Type;
                                 Sample_Out    : Audio_Res) is
      Bits_In     : PCM_Bit_Array;
      Bits_Out    : Audio_Res_Bit_Array;
      for Bits_In'Address  use Sample_In'Address;
      for Bits_Out'Address use Sample_Out'Address;
   begin
      Put_Line ("In (Buf):  " & Float'Image (Float (Sample_In)));
      for K in reverse PCM_Bit_Array'Range loop
         Put (Bool_Image (Bits_In (K)));
      end loop;
      New_Line;

      Put_Line ("Out (Wav): " & Integer'Image (Integer (Sample_Out)));
      for K in reverse Audio_Res_Bit_Array'Range loop
         Put (Bool_Image (Bits_Out (K)));
      end loop;
      New_Line;
   end Print_Sample_Write;


   function Convert_Sample (Sample : Audio_Res) return PCM_Type is

      Sample_In   : Audio_Res := Sample;
      Sample_Out  : PCM_Type := 0.0;
      Bits_In     : Audio_Res_Bit_Array;
      Bits_Out    : PCM_Bit_Array;
      for Bits_In'Address  use Sample_In'Address;
      for Bits_Out'Address use Sample_Out'Address;

   begin
      if Audio_Res'Size <= PCM_Type'Size then
         for B in 0 .. Audio_Res'Size - 1 loop
            --  Todo: better handling of small negative values
            Bits_Out (B + PCM_Type'Size - Audio_Res'Size) := Bits_In (B);
         end loop;
      else
         for B in 0 .. PCM_Type'Size - 1 loop
            Bits_Out (B) := Bits_In (B + Audio_Res'Size - PCM_Type'Size);
         end loop;
      end if;

      if Convert_Sample_Debug then
         Print_Sample_Read (Sample_In, Sample_Out);
      end if;

      return Sample_Out;
   end Convert_Sample;


   function Convert_Sample (Sample : PCM_Type) return Audio_Res is

      Sample_In   : PCM_Type := Sample;
      Sample_Out  : Audio_Res := 0;
      Bits_In     : PCM_Bit_Array;
      Bits_Out    : Audio_Res_Bit_Array;
      for Bits_In'Address  use Sample_In'Address;
      for Bits_Out'Address use Sample_Out'Address;
   begin
      if PCM_Type'Size <= Audio_Res'Size then
         for B in 0 .. PCM_Type'Size - 1 loop
            --  Todo: better handling of small negative values
            Bits_Out (B + Audio_Res'Size - PCM_Type'Size) := Bits_In (B);
         end loop;
      else
         for B in 0 .. Audio_Res'Size - 1 loop
            Bits_Out (B) := Bits_In (B + PCM_Type'Size - Audio_Res'Size);
         end loop;
      end if;

      if Convert_Sample_Debug then
         Print_Sample_Write (Sample_In, Sample_Out);
      end if;

      return Sample_Out;
   end Convert_Sample;


   procedure Read_Data (WF  : in out Wavefile;
                        Buf : in out PCM_Buffer) is
      Ch : constant Positive := Positive (WF.Wave_Format.Channels);
      type Audio_Sample is array (1 .. Ch) of Audio_Res;
      B  : Audio_Sample;
      BB : Audio_Res;

   begin
      Ada.Assertions.Assert (Ch <= Buf.Channels,
                             "Unsufficient number of channel in buffer");

      for I in 1 .. Wavefiles.PCM_Buffers.Samples loop
         for J in 1 .. Ch loop

            Audio_Res'Read (WF.File_Access, BB);
            B (J) := BB;
            if Ada.Streams.Stream_IO.End_Of_File (WF.File) and then
              J < Ch
            then
               --  Cannot read data for all channels
               raise Wavefile_Error;
            end if;
         end loop;

         WF.Samples_Read := WF.Samples_Read + Long_Integer (Ch);
         Buf.Info.Samples_Valid := Buf.Info.Samples_Valid + 1;
         for J in 1 .. Ch loop
            Buf.Audio_Data (J) (I) := Convert_Sample (B (J));
         end loop;

         exit when WF.Samples_Read >= WF.Samples or
           Ada.Streams.Stream_IO.End_Of_File (WF.File);
         --  exit when ;
      end loop;
      for J in 1 .. Ch loop
         Buf.Info.Active (J) := True;
      end loop;
      for J in Ch + 1 .. Buf.Channels loop
         Buf.Info.Active (J) := False;
      end loop;
   end Read_Data;


   procedure Write_Data (WF  : in out Wavefile;
                         Buf : in PCM_Buffer) is
      Ch : constant Positive := Positive (WF.Wave_Format.Channels);
      type Audio_Sample is array (1 .. Ch) of Audio_Res;
      B  : Audio_Sample;
   begin
      Ada.Assertions.Assert (Ch <= Buf.Channels,
                             "Unsufficient number of channel in buffer");

      for I in 1 .. Buf.Info.Samples_Valid loop
         for J in 1 .. Ch loop
            B (J) := Convert_Sample (Buf.Audio_Data (J) (I));
         end loop;
         Audio_Sample'Write (WF.File_Access, B);
         WF.Samples := WF.Samples + Long_Integer (Ch);
      end loop;

   end Write_Data;

end Wavefiles.PCM_Buffers.Data;
