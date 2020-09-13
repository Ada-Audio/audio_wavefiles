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

generic
   Samples : Positive;
   type PCM_Type is private;
   with procedure Reset (A : out PCM_Type) is <>;
   with function "=" (A, B : PCM_Type) return Boolean is <>;

package Audio.Wavefiles.PCM_Buffers is

   type PCM_Channel_Buffer_Type is array (1 .. Samples) of PCM_Type;

   type Audio_Data_Type is array (Positive range <>)
     of PCM_Channel_Buffer_Type;

   type PCM_Buffer_Info (Channels : Positive) is private;

   type PCM_Buffer (Channels : Positive) is
      record
         Audio_Data      : Audio_Data_Type (1 .. Channels);
         Info            : PCM_Buffer_Info (Channels);
      end record;

   type PCM_Buffer_Op is access function (A, B : PCM_Type) return PCM_Type;

   function Is_Channel_Active
     (PCM_Buf : PCM_Buffer;
      Channel : Positive) return Boolean;

   function Get_Number_Valid_Samples (PCM_Buf : PCM_Buffer) return Natural;

   function "=" (Left, Right : PCM_Buffer) return Boolean;

   function Perform
     (Left, Right : PCM_Buffer;
      Op          : PCM_Buffer_Op) return PCM_Buffer;

private
   type Audio_Active_Type is array (Positive range <>) of Boolean;

   type PCM_Buffer_Info (Channels : Positive) is
      record
         Active          : Audio_Active_Type (1 .. Channels);
         Samples_Valid   : Natural := 0;
      end record;

end Audio.Wavefiles.PCM_Buffers;
