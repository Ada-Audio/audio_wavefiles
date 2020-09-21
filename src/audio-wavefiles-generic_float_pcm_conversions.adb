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

package body Audio.Wavefiles.Generic_Float_PCM_Conversions is

   type PCM_Bits_Type is array (0 .. PCM_Type'Size - 1) of Boolean
     with Pack;

   type Wav_Data_Bits_Type is array (0 .. Wav_Data_Type'Size - 1) of Boolean
     with Pack;

   Bool_Image  : constant array (Boolean'Range) of Character := ('0', '1');

   procedure Print_Sample_Read
     (Wav_Sample : Wav_Data_Type;
      PCM_Sample : PCM_Type)
   is
      Wav_Sample_Bits : Wav_Data_Bits_Type;
      PCM_Sample_Bits : PCM_Bits_Type;
      for Wav_Sample_Bits'Address use Wav_Sample'Address;
      for PCM_Sample_Bits'Address use PCM_Sample'Address;
   begin
      Put_Line ("In (Wav):  " & Integer'Image (Integer (Wav_Sample)));
      for K in reverse Wav_Data_Bits_Type'Range loop
         Put (Bool_Image (Wav_Sample_Bits (K)));
      end loop;
      New_Line;

      Put_Line ("Out (Buf): " & Long_Float'Image (Long_Float (PCM_Sample)));
      for K in reverse PCM_Bits_Type'Range loop
         Put (Bool_Image (PCM_Sample_Bits (K)));
      end loop;
      New_Line;
   end Print_Sample_Read;


   procedure Print_Sample_Write
     (PCM_Sample : PCM_Type;
      Wav_Sample : Wav_Data_Type)
   is
      PCM_Sample_Bits : PCM_Bits_Type;
      Wav_Sample_Bits : Wav_Data_Bits_Type;
      for PCM_Sample_Bits'Address use PCM_Sample'Address;
      for Wav_Sample_Bits'Address use Wav_Sample'Address;
   begin
      Put_Line ("In (Buf):  " & Long_Float'Image (Long_Float (PCM_Sample)));
      for K in reverse PCM_Bits_Type'Range loop
         Put (Bool_Image (PCM_Sample_Bits (K)));
      end loop;
      New_Line;

      Put_Line ("Out (Wav): " & Integer'Image (Integer (Wav_Sample)));
      for K in reverse Wav_Data_Bits_Type'Range loop
         Put (Bool_Image (Wav_Sample_Bits (K)));
      end loop;
      New_Line;
   end Print_Sample_Write;


   function Convert_Sample (Wav_Sample : Wav_Data_Type) return PCM_Type is
      Wav_Sample_In  : constant Wav_Data_Type := Wav_Sample;
      PCM_Sample_Out : PCM_Type;
   begin
      case Wav_Num_Type is
      when Wav_Fixed_Data =>
         PCM_Sample_Out := PCM_Type (Long_Float (Wav_Sample_In)
                                     / Long_Float (Wav_Data_Type'Last));
      when Wav_Float_Data =>
         case Wav_Data_Type'Size is
         when 32 =>
            declare
               Wav_Sample_Float : Float;
               for Wav_Sample_Float'Address use Wav_Sample_In'Address;
               pragma Assert (Float'Size = 32);
            begin
               PCM_Sample_Out := PCM_Type (Wav_Sample_Float);
            end;

         when 64 =>
            declare
               Wav_Sample_Float : Long_Float;
               for Wav_Sample_Float'Address use Wav_Sample_In'Address;
               pragma Assert (Long_Float'Size = 64);
            begin
               PCM_Sample_Out := PCM_Type (Wav_Sample_Float);
            end;

         when others =>
            PCM_Sample_Out := 0.0;
         end case;

      end case;

      if Convert_Sample_Debug then
         Print_Sample_Read (Wav_Sample_In, PCM_Sample_Out);
      end if;

      return PCM_Sample_Out;
   end Convert_Sample;


   function Convert_Sample (PCM_Sample : PCM_Type) return Wav_Data_Type is
      PCM_Sample_In   : constant PCM_Type := PCM_Sample;
      Wav_Sample_Out  : Wav_Data_Type;
   begin
      case Wav_Num_Type is
      when Wav_Fixed_Data =>
         Wav_Sample_Out := Wav_Data_Type (Long_Float (PCM_Sample_In)
                                          * Long_Float (Wav_Data_Type'Last));
      when Wav_Float_Data =>
         case Wav_Data_Type'Size is
         when 32 =>
            declare
               Wav_Sample_Float : Float;
               for Wav_Sample_Float'Address use Wav_Sample_Out'Address;
               pragma Assert (Float'Size = 32);
            begin
               Wav_Sample_Float := Float (PCM_Sample_In);
            end;

         when 64 =>
            declare
               Wav_Sample_Float : Long_Float;
               for Wav_Sample_Float'Address use Wav_Sample_Out'Address;
               pragma Assert (Long_Float'Size = 64);
            begin
               Wav_Sample_Float := Long_Float (PCM_Sample_In);
            end;

         when others =>
            Wav_Sample_Out := 0;
         end case;
      end case;

      if Convert_Sample_Debug then
         Print_Sample_Write (PCM_Sample_In, Wav_Sample_Out);
      end if;

      return Wav_Sample_Out;
   end Convert_Sample;

end Audio.Wavefiles.Generic_Float_PCM_Conversions;
