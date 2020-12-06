------------------------------------------------------------------------------
--                                                                          --
--                               WAVEFILES                                  --
--                                                                          --
--                Common data types for wavefile buffer I/O                 --
--                                                                          --
--  The MIT License (MIT)                                                   --
--                                                                          --
--  Copyright (c) 2020 Gustavo A. Hoffmann                                  --
--                                                                          --
--  Permission is hereby granted, free of charge, to any person obtaining   --
--  a copy of this software and associated documentation files (the         --
--  "Software"), to deal in the Software without restriction, including     --
--  without limitation the rights to use, copy, modify, merge, publish,     --
--  distribute, sublicense, and / or sell copies of the Software, and to    --
--  permit persons to whom the Software is furnished to do so, subject to   --
--  the following conditions:                                               --
--                                                                          --
--  The above copyright notice and this permission notice shall be          --
--  included in all copies or substantial portions of the Software.         --
--                                                                          --
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         --
--  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      --
--  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  --
--  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY    --
--  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,    --
--  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE       --
--  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                  --
------------------------------------------------------------------------------

package Audio.Wavefiles.Data_Types is

   D_8      : constant := 1.0 / 2.0 ** (8  - 1);
   D_16     : constant := 1.0 / 2.0 ** (16 - 1);
   D_24     : constant := 1.0 / 2.0 ** (24 - 1);
   D_32     : constant := 1.0 / 2.0 ** (32 - 1);
   D_64     : constant := 1.0 / 2.0 ** (64 - 1);

   type Wav_Unsigned_Fixed_8 is delta D_8 range 0.0 .. 2.0 - D_8
     with Size => 8,  Stream_Size => 8;
   type Wav_Fixed_8 is delta D_8 range -1.0 .. 1.0 - D_8
     with Size => 8,  Stream_Size => 8;
   type Wav_Fixed_16 is delta D_16 range -1.0 .. 1.0 - D_16
     with Size => 16, Stream_Size => 16;

   --  Originally:
   --
   --     type Wav_Fixed_24 is delta D_24 range -1.0 .. 1.0 - D_24
   --       with Size => 24, Stream_Size => 24;
   --
   --  Using "Stream_Size => 24" only works for GNAT Community 2020 or newer!
   --  The error message for older versions of GNAT is: "stream size for
   --  elementary type must be a power of 2 and at least 8."
   --
   --  For the moment, we use a patch in packages
   --  Audio.Wavefiles.Generic_<Fixed|Float>_Wav_IO, see procedures Get_Bytes
   --  and Put_Bytes.
   --
   type Wav_Fixed_24 is delta D_24 range -1.0 .. 1.0 - D_24
     with Size => 24;

   type Wav_Fixed_32 is delta D_32 range -1.0 .. 1.0 - D_32
     with Size => 32, Stream_Size => 32;
   type Wav_Fixed_64 is delta D_64 range -1.0 .. 1.0 - D_64
     with Size => 64, Stream_Size => 64;

   type Wav_Float_32 is digits 6
     with Size => 32, Stream_Size => 32;
   type Wav_Float_64 is digits 15
     with Size => 64, Stream_Size => 64;
   type Wav_Float_128 is digits 18
     with Size => 128;

   subtype Wav_Buffer_Range is Positive;

   type Wav_Buffer_Unsigned_Fixed_8   is
     array (Wav_Buffer_Range range <>) of Wav_Unsigned_Fixed_8;
   type Wav_Buffer_Fixed_8   is
     array (Wav_Buffer_Range range <>) of Wav_Fixed_8;
   type Wav_Buffer_Fixed_16  is
     array (Wav_Buffer_Range range <>) of Wav_Fixed_16;
   type Wav_Buffer_Fixed_24  is
     array (Wav_Buffer_Range range <>) of Wav_Fixed_24;
   type Wav_Buffer_Fixed_32  is
     array (Wav_Buffer_Range range <>) of Wav_Fixed_32;
   type Wav_Buffer_Fixed_64  is
     array (Wav_Buffer_Range range <>) of Wav_Fixed_64;

   type Wav_Buffer_Float_32  is
     array (Wav_Buffer_Range range <>) of Wav_Float_32;
   type Wav_Buffer_Float_64  is
     array (Wav_Buffer_Range range <>) of Wav_Float_64;
   type Wav_Buffer_Float_128 is
     array (Wav_Buffer_Range range <>) of Wav_Float_128;

end Audio.Wavefiles.Data_Types;
