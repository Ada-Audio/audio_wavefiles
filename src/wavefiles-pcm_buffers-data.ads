-------------------------------------------------------------------------------
--
--                                WAVEFILES
--
--                   Internal I/O operations for PCM buffers
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

private generic
   type Audio_Res is range <>;
   PCM_Float_Type_Support : in Boolean;
   with function To_Long_Float (A : PCM_Type)   return Long_Float is <>;
   with function To_PCM_Type   (A : Long_Float) return PCM_Type   is <>;
package Wavefiles.PCM_Buffers.Data is

   procedure Read_Data
     (WF  : in out Wavefile;
      Buf : in out PCM_Buffer);

   procedure Write_Data
     (WF  : in out Wavefile;
      Buf : in PCM_Buffer);

end Wavefiles.PCM_Buffers.Data;
