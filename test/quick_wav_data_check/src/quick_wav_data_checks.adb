-------------------------------------------------------------------------------
--
--                                WAVEFILES
--
--                         Quick Wave Data I/O Check
--
--  The MIT License (MIT)
--
--  Copyright (c) 2020 Gustavo A. Hoffmann
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

with Quick_Wav_Data_Checks.Float_Checks;
with Quick_Wav_Data_Checks.Fixed_Checks;

package body Quick_Wav_Data_Checks is

   function Wav_IO_OK
     (Wav_Filename_Prefix : String) return Boolean
   is
      Success : Boolean := True;
   begin
      if not Fixed_Checks.Wav_IO_OK (Wav_Filename_Prefix) then
         Success := False;
      end if;
      if not Float_Checks.Wav_IO_OK (Wav_Filename_Prefix) then
         Success := False;
      end if;

      return Success;
   end Wav_IO_OK;


end Quick_Wav_Data_Checks;
