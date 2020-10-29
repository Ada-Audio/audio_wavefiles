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
with Ada.Containers.Vectors;

with Audio.RIFF;
with Audio.RIFF.Wav.Formats; use Audio.RIFF.Wav.Formats;

package Audio.Wavefiles is

   type Wavefile is tagged limited private;

   type File_Mode is new Ada.Streams.Stream_IO.File_Mode;

   Wavefile_Error       : exception;
   Wavefile_Unsupported : exception;

   type Wav_Chunk_Element is
      record
         Chunk_Tag    : Wav_Chunk_Tag;
         ID           : Audio.RIFF.FOURCC_String;
         Size         : Long_Integer;
         Start_Index  : Ada.Streams.Stream_IO.Positive_Count;
         Consolidated : Boolean;
      end record;

   package Wav_Chunk_Element_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Wav_Chunk_Element);
   use Wav_Chunk_Element_Vectors;

   subtype Wav_Chunk_Elements is Wav_Chunk_Element_Vectors.Vector;

   type RIFF_Information is
      record
         Id     : RIFF_Identifier;
         Format : RIFF_Format;
         Chunks : Wav_Chunk_Elements;
      end record;

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

   procedure Get_RIFF_Info
     (WF     : in out Wavefile;
      Info   :    out RIFF_Information)
     with Pre => Is_Open (WF);

   function Is_Supported_Format
     (W : Wave_Format_Extensible) return Boolean;

private

   type Wav_Numeric_Data_Type is (Wav_Fixed_Data, Wav_Float_Data);

   type Wavefile is tagged limited
      record
         Is_Opened        : Boolean      := False;
         File             : Ada.Streams.Stream_IO.File_Type;
         File_Access      : Ada.Streams.Stream_IO.Stream_Access;
         Wave_Format      : Wave_Format_Extensible := Default;
         Samples          : Long_Integer;
         Samples_Read     : Long_Integer;
         RIFF_Info        : RIFF_Information;
      end record;

   function Is_Open
     (WF : Wavefile) return Boolean is (WF.Is_Opened);

end Audio.Wavefiles;
