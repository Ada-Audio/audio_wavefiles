------------------------------------------------------------------------------
--                                                                          --
--                               WAVEFILES                                  --
--                                                                          --
--                             Main package                                 --
--                                                                          --
--  The MIT License (MIT)                                                   --
--                                                                          --
--  Copyright (c) 2015 -- 2021 Gustavo A. Hoffmann                          --
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

with Ada.Streams.Stream_IO;
with Ada.Containers.Vectors;
with Interfaces;

with Audio.RIFF;
with Audio.RIFF.Wav.Formats; use Audio.RIFF.Wav.Formats;

package Audio.Wavefiles is

   type Wavefile is tagged limited private;

   type File_Mode is new Ada.Streams.Stream_IO.File_Mode;

   type Wavefile_Error_Code is
     (Wavefile_Error_File_Not_Open,
      Wavefile_Error_File_Already_Open,
      Wavefile_Error_File_Too_Short,
      Wavefile_Error_Format_Chuck_Not_Found,
      Wavefile_Error_Data_Chuck_Not_Found,
      Wavefile_Error_Unsupported_Wavefile_Format,
      Wavefile_Error_Unsupported_Bit_Depth,
      Wavefile_Error_Unsupported_Format_Size);

   type Wavefile_Errors is array (Wavefile_Error_Code) of Boolean
     with Pack;

   No_Wavefile_Errors : constant Wavefile_Errors := (others => False);

   type Wavefile_Warning_Code is
     (Wavefile_Warning_Inconsistent_Channel_Mask);

   type Wavefile_Warnings is array (Wavefile_Warning_Code) of Boolean
     with Pack;

   No_Wavefile_Warnings : constant Wavefile_Warnings := (others => False);

   subtype Byte is Interfaces.Unsigned_8;
   type Byte_Array is array (Long_Integer range <>) of Byte;

   type Wav_Chunk_Element is
      record
         Chunk_Tag    : Wav_Chunk_Tag;
         ID           : Audio.RIFF.FOURCC_String;
         Size         : Long_Integer;
         Start_Index  : Ada.Streams.Stream_IO.Positive_Count;
         Consolidated : Boolean;
      end record;

   function Chunk_Element_Data
     (WF            : Wavefile;
      Chunk_Element : Wav_Chunk_Element) return Byte_Array;

   package Wav_Chunk_Element_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Wav_Chunk_Element);
   use Wav_Chunk_Element_Vectors;

   subtype Wav_Chunk_Elements is Wav_Chunk_Element_Vectors.Vector;

   type Wav_Chunk_Element_Found (Success : Boolean := False) is
      record
         case Success is
            when False =>
               null;
            when True  =>
               Chunk_Element : Wav_Chunk_Element;
         end case;
      end record;

   procedure Find_First_Chunk
     (Chunks    :     Wav_Chunk_Elements;
      Chunk_Tag :     Wav_Chunk_Tag;
      Found     : out Wav_Chunk_Element_Found);

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

   function Errors (WF : Wavefile) return Wavefile_Errors
     with Inline;

   function Warnings (WF : Wavefile) return Wavefile_Warnings
     with Inline;

   procedure Set_Format_Of_Wavefile
     (WF     : in out Wavefile;
      Format :        Wave_Format_Extensible)
     with Pre => not Is_Open (WF);

   function Format_Of_Wavefile
     (W : Wavefile) return Wave_Format_Extensible;

   function Number_Of_Channels
     (W : Wavefile) return Positive;

   function Bit_Depth
     (W : Wavefile) return Wav_Bit_Depth;

   function Sample_Rate
     (W : Wavefile) return Wav_Sample_Rate;

   function Mode
     (W : Wavefile) return File_Mode;

   function Name
     (W : Wavefile) return String;

   procedure Get_RIFF_Info
     (WF     : in out Wavefile;
      Info   :    out RIFF_Information)
     with Pre => Is_Open (WF);

   subtype Sample_Count is Long_Long_Integer;

   function Current_Sample
     (WF : Wavefile) return Sample_Count;
   function First_Sample
     (WF : Wavefile) return Sample_Count;
   function Last_Sample
     (WF : Wavefile) return Sample_Count;
   function Total_Sample_Count
     (WF : Wavefile) return Sample_Count;

   procedure Set_Current_Sample
     (WF       : in out Wavefile;
      Position :        Sample_Count)
     with Pre => (Mode (WF) = In_File
                  and Position >= WF.First_Sample
                  and Position <= WF.Last_Sample);

   subtype Wavefile_Time_In_Seconds is Long_Long_Float;

   function Current_Time
     (WF : Wavefile) return Wavefile_Time_In_Seconds;
   function End_Time
     (WF : Wavefile) return Wavefile_Time_In_Seconds;

   procedure Set_Current_Time
     (WF      : in out Wavefile;
      At_Time :        Wavefile_Time_In_Seconds)
     with Pre => Mode (WF) = In_File;

   function Is_Supported_Format
     (W : Wave_Format_Extensible) return Boolean;

private
   --
   --  Constants that indicate a range of
   --  "First_Sample_Count .. <total_sample_count> - Total_To_Last_Diff"
   --
   --  Range used in this implementation: "0 .. <total_sample_count> - 1"
   --
   --  You can change this range to "1 .. <total_sample_count>" by changing
   --  the constants as follows:
   --
   --     First_Sample_Count : constant Sample_Count := 1;
   --     Total_To_Last_Diff : constant Sample_Count := 0;
   --
   First_Sample_Count : constant Sample_Count := 0;
   Total_To_Last_Diff : constant Sample_Count := 1;

   type Sample_Info is
      record
         Current : Sample_Count;
         Total   : Sample_Count;
      end record
     with Dynamic_Predicate =>
       Sample_Info.Current in
         First_Sample_Count .. Sample_Info.Total - Total_To_Last_Diff + 1;
   --  Note: the "+ 1" above indicates that the Current counter can be in the
   --        "end of file" position after a call to Get.

   procedure Set_Error (WF         : in out Wavefile;
                        Error_Code :        Wavefile_Error_Code);
   procedure Reset_Errors (WF      : in out Wavefile);

   procedure Set_Warning (WF           : in out Wavefile;
                          Warning_Code :        Wavefile_Warning_Code);
   procedure Reset_Warnings (WF        : in out Wavefile);

   type Wavefile is tagged limited
      record
         Is_Opened        : Boolean      := False;
         File             : Ada.Streams.Stream_IO.File_Type;
         File_Access      : Ada.Streams.Stream_IO.Stream_Access;
         Wave_Format      : Wave_Format_Extensible := Default;
         Sample_Pos       : Sample_Info;
         RIFF_Info        : RIFF_Information;
         Errors           : Wavefile_Errors   := (others => False);
         Warnings         : Wavefile_Warnings := (others => False);
      end record;

   function First_Sample
     (WF : Wavefile) return Sample_Count is (First_Sample_Count);

   function Is_Open
     (WF : Wavefile) return Boolean is (WF.Is_Opened);

   function Errors (WF : Wavefile) return Wavefile_Errors is
     (WF.Errors);

   function Warnings (WF : Wavefile) return Wavefile_Warnings is
      (WF.Warnings);

end Audio.Wavefiles;
