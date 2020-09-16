-------------------------------------------------------------------------------
--
--                                WAVEFILES
--
--               Type conversion for wavefile I/O operations
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

with Ada.Text_IO; use Ada.Text_IO;

package body Audio.Wavefiles.Fixed_Types is

   procedure Print_Sample_Read
     (Sample_In     : Audio_Res;
      Sample_Out    : PCM_Type)
   is
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

      Put_Line ("Out (Buf): " & Long_Float'Image (Long_Float (Sample_Out)));
      for K in reverse PCM_Bit_Array'Range loop
         Put (Bool_Image (Bits_Out (K)));
      end loop;
      New_Line;
   end Print_Sample_Read;


   procedure Print_Sample_Write
     (Sample_In     : PCM_Type;
      Sample_Out    : Audio_Res)
   is
      Bits_In     : PCM_Bit_Array;
      Bits_Out    : Audio_Res_Bit_Array;
      for Bits_In'Address  use Sample_In'Address;
      for Bits_Out'Address use Sample_Out'Address;
   begin
      Put_Line ("In (Buf):  " & Long_Float'Image (Long_Float (Sample_In)));
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
      Sample_Out  : PCM_Type;

   begin
      if Fixed then
         declare
            Bits_In     : Audio_Res_Bit_Array;
            Bits_Out    : PCM_Bit_Array;
            for Bits_In'Address  use Sample_In'Address;
            for Bits_Out'Address use Sample_Out'Address;
         begin
            Sample_Out := 0.0;

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
         end;
      else
         case Audio_Res'Size is

         when 32 =>
            declare
               F : Float;
               for F'Address use Sample_In'Address;
               pragma Assert (F'Size = 32);
            begin
               if F > Float (PCM_Type'Last) then
                  Sample_Out := PCM_Type'Last;
               elsif F < Float (PCM_Type'First) then
                  Sample_Out := PCM_Type'First;
               else
                  Sample_Out := PCM_Type (F);
               end if;
            end;

         when 64 =>
            declare
               F : Long_Float;
               for F'Address use Sample_In'Address;
               pragma Assert (F'Size = 64);
            begin
               if F > Long_Float (PCM_Type'Last) then
                  Sample_Out := PCM_Type'Last;
               elsif F < Long_Float (PCM_Type'First) then
                  Sample_Out := PCM_Type'First;
               else
                  Sample_Out := PCM_Type (F);
               end if;
            end;

         when others =>
            Sample_Out := 0.0;
         end case;
      end if;

      if Convert_Sample_Debug then
         Print_Sample_Read (Sample_In, Sample_Out);
      end if;

      return Sample_Out;
   end Convert_Sample;


   function Convert_Sample (Sample : PCM_Type) return Audio_Res is
      Sample_In   : PCM_Type := Sample;
      Sample_Out  : Audio_Res;
      Bits_In     : PCM_Bit_Array;
      Bits_Out    : Audio_Res_Bit_Array;
      for Bits_In'Address  use Sample_In'Address;
      for Bits_Out'Address use Sample_Out'Address;
   begin
      if Fixed then
         Sample_Out := 0;
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
      else
         case Audio_Res'Size is
         when 32 =>
            declare
               F : Float;
               for F'Address use Sample_Out'Address;
               pragma Assert (F'Size = 32);
            begin
               F := Float (Sample_In);
            end;

         when 64 =>
            declare
               F : Long_Float;
               for F'Address use Sample_Out'Address;
               pragma Assert (F'Size = 64);
            begin
               F := Long_Float (Sample_In);
            end;
         when others =>
            Sample_Out := 0;
         end case;
      end if;

      if Convert_Sample_Debug then
         Print_Sample_Write (Sample_In, Sample_Out);
      end if;

      return Sample_Out;
   end Convert_Sample;

end Audio.Wavefiles.Fixed_Types;
