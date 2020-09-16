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

private generic
   Fixed : Boolean;
   type Audio_Res is range <>;
   type PCM_Type is delta <>;
package Audio.Wavefiles.Fixed_Types is

   type PCM_Bit_Array is array (0 .. PCM_Type'Size - 1) of Boolean;
   pragma Pack (PCM_Bit_Array);

   type Audio_Res_Bit_Array is array (0 .. Audio_Res'Size - 1) of Boolean;
   pragma Pack (Audio_Res_Bit_Array);

   Bool_Image  : constant array (Boolean'Range) of Character := ('0', '1');
   Convert_Sample_Debug : constant Boolean := False;

   procedure Print_Sample_Read
     (Sample_In     : Audio_Res;
      Sample_Out    : PCM_Type);

   procedure Print_Sample_Write
     (Sample_In     : PCM_Type;
      Sample_Out    : Audio_Res);

   function Convert_Sample (Sample : Audio_Res) return PCM_Type;

   function Convert_Sample (Sample : PCM_Type) return Audio_Res;

end Audio.Wavefiles.Fixed_Types;
