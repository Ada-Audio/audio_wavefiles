-------------------------------------------------------------------------------
--
--                                WAVEFILES
--
--                   Wavefile I/O operations for PCM buffers
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

with Interfaces;

with Wavefiles.Internals;
with Wavefiles.PCM_Buffers.Data;
with RIFF; use RIFF;

package body Wavefiles.PCM_Buffers.IO is

   package Int     is new Wavefiles.Internals;

   use Interfaces;
   package Data_16 is new Wavefiles.PCM_Buffers.Data (Integer_16);
   package Data_24 is new Wavefiles.PCM_Buffers.Data (Int.Integer_24);
   package Data_32 is new Wavefiles.PCM_Buffers.Data (Integer_32);

   function Is_Supported_Format (W : Wave_Format_Extensible) return Boolean;

   function Is_Supported_Format (W : Wave_Format_Extensible) return Boolean is
   begin
      if not (W.Sub_Format = GUID_Undefined or W.Sub_Format = GUID_PCM)
      then
         return False;
      end if;

      return True;
   end Is_Supported_Format;


   procedure Read (WF  : in out Wavefile;
                   Buf : out    PCM_Buffer;
                   EOF : out    Boolean) is
   begin
      if not WF.Is_Opened then
         raise Wavefile_Error;
      end if;
      if not Is_Supported_Format (WF.Wave_Format) then
         raise Wavefile_Unsupported;
      end if;

      Buf.Info.Samples_Valid := 0;

      case WF.Wave_Format.Bits_Per_Sample is
         when 8 =>
            raise Wavefile_Unsupported;
         when 16 =>
            Data_16.Read_Data (WF, Buf);
         when 24 =>
            Data_24.Read_Data (WF, Buf);
         when 32 =>
            Data_32.Read_Data (WF, Buf);
         when others =>
            raise Wavefile_Unsupported;
      end case;

      if WF.Samples_Read >= WF.Samples or
        Ada.Streams.Stream_IO.End_Of_File (WF.File)
      then
         EOF := True;
      else
         EOF := False;
      end if;
   end Read;

   procedure Write (WF  : in out Wavefile;
                    Buf : in     PCM_Buffer) is
   begin
      if not WF.Is_Opened then
         raise Wavefile_Error;
      end if;
      if not Is_Supported_Format (WF.Wave_Format) then
         raise Wavefile_Unsupported;
      end if;

      case WF.Wave_Format.Bits_Per_Sample is
         when 16 =>
            Data_16.Write_Data (WF, Buf);
         when 24 =>
            Data_24.Write_Data (WF, Buf);
         when 32 =>
            Data_32.Write_Data (WF, Buf);
         when others =>
            raise Wavefile_Unsupported;
      end case;

   end Write;


end Wavefiles.PCM_Buffers.IO;
