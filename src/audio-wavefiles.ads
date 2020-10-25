-------------------------------------------------------------------------------
--
--                                WAVEFILES
--
--                              Main package
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

with Ada.Streams.Stream_IO;

with Audio.RIFF.Wav.Formats; use Audio.RIFF.Wav.Formats;

package Audio.Wavefiles is

   type Wavefile is limited private;

   type File_Mode is new Ada.Streams.Stream_IO.File_Mode;

   Wavefile_Error       : exception;
   Wavefile_Unsupported : exception;

   procedure Create
     (WF   : in out Wavefile;
      Mode :        File_Mode := Out_File;
      Name :        String    := "";
      Form :        String    := "");

   procedure Open
     (WF   : in out Wavefile;
      Mode :        File_Mode;
      Name :        String;
      Form :        String    := "");

   function Is_Open
     (WF : Wavefile) return Boolean;

   function End_Of_File
     (WF   : in out Wavefile) return Boolean
     with Inline, Pre => Mode (WF) = In_File;

   procedure Display_Info (WF : in Wavefile)
     with Pre => Mode (WF) = In_File;

   procedure Close (WF : in out Wavefile);

   procedure Set_Format_Of_Wavefile
     (WF     : in out Wavefile;
      Format :        Wave_Format_Extensible)
     with Pre => not Is_Open (WF);

   function Format_Of_Wavefile
     (W : Wavefile) return Wave_Format_Extensible;

   function Number_Of_Channels
     (W : Wavefile) return Positive;

   function Mode
     (W : Wavefile) return File_Mode;

   function Name
     (W : Wavefile) return String;

   function Is_Supported_Format
     (W : Wave_Format_Extensible) return Boolean;

private

   type Wav_Numeric_Data_Type is (Wav_Fixed_Data, Wav_Float_Data);

   type Wavefile is limited
      record
         Is_Opened        : Boolean      := False;
         File             : Ada.Streams.Stream_IO.File_Type;
         File_Access      : Ada.Streams.Stream_IO.Stream_Access;
         File_Index       : Ada.Streams.Stream_IO.Positive_Count;
         Wave_Format      : Wave_Format_Extensible := Default;
         Samples          : Long_Integer;
         Samples_Read     : Long_Integer;
      end record;

   function Is_Open
     (WF : Wavefile) return Boolean is (WF.Is_Opened);

end Audio.Wavefiles;
