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

with Audio.Wavefiles.Read;
with Audio.Wavefiles.Write;
with Audio.Wavefiles.Internals;

with Audio.RIFF.Wav.GUIDs;          use Audio.RIFF.Wav.GUIDs;

package body Audio.Wavefiles is

   procedure Init_Data_For_File_Opening
     (WF   : in out Wavefile);
   procedure Check_Consistency
     (WF   : in out Wavefile);
   procedure Reset_RIFF_Info
     (Info :      out RIFF_Information);

   ---------------
   -- No_Errors --
   ---------------

   function No_Errors
     (WF   : in out Wavefile) return Boolean
   is (for all Error of WF.Errors => Error = False);

   --------------------------------
   -- Init_Data_For_File_Opening --
   --------------------------------

   procedure Init_Data_For_File_Opening
     (WF   : in out Wavefile) is
   begin
      WF.Sample_Pos := (Current => First_Sample_Count,
                        Total   => 0);
      Reset_RIFF_Info (WF.RIFF_Info);
   end Init_Data_For_File_Opening;

   -----------------------
   -- Check_Consistency --
   -----------------------

   procedure Check_Consistency
     (WF   : in out Wavefile) is
   begin
      if not Is_Consistent
        (Channel_Config      => WF.Wave_Format.Channel_Config,
         Number_Of_Channels  => Positive (WF.Wave_Format.Channels))
      then
         WF.Set_Warning (Wavefile_Warning_Inconsistent_Channel_Mask);
      end if;
   end Check_Consistency;

   ------------
   -- Create --
   ------------

   procedure Create
     (WF   : in out Wavefile;
      Mode :        File_Mode := Out_File;
      Name :        String    := "";
      Form :        String    := "")
   is
      pragma Unreferenced (Form);
   begin
      WF.Reset_Errors;
      WF.Reset_Warnings;

      Init_Data_For_File_Opening (WF);

      if Mode in In_File | Append_File then
         WF.Wave_Format := Default;
      end if;

      Check_Consistency (WF);

      Create_Wavefile : declare
         Stream_Mode : constant Ada.Streams.Stream_IO.File_Mode :=
                         Ada.Streams.Stream_IO.File_Mode (Mode);
      begin
         Ada.Streams.Stream_IO.Create (WF.File, Stream_Mode, Name);
         WF.File_Access := Ada.Streams.Stream_IO.Stream (WF.File);
      end Create_Wavefile;

      case Mode is
         when In_File | Append_File =>
            Audio.Wavefiles.Read.Read_Until_Data_Start (WF);
         when Out_File =>
            Audio.Wavefiles.Write.Write_Until_Data_Start (WF);
      end case;

      WF.Is_Opened := WF.No_Errors;
   end Create;

   ----------
   -- Open --
   ----------

   procedure Open
     (WF   : in out Wavefile;
      Mode :        File_Mode;
      Name :        String;
      Form :        String    := "")
   is
      pragma Unreferenced (Form);
   begin
      WF.Reset_Errors;
      WF.Reset_Warnings;

      if WF.Is_Opened then
         WF.Set_Error (Wavefile_Error_File_Already_Open);
         return;
      end if;

      Init_Data_For_File_Opening (WF);

      case Mode is
         when In_File =>
            WF.Wave_Format := Default;
         when Append_File =>
            Parse_Wavefile : declare
               use Ada.Streams.Stream_IO;
               Stream_Mode : constant Ada.Streams.Stream_IO.File_Mode :=
                               Ada.Streams.Stream_IO.File_Mode'(In_File);
            begin
               Open (WF.File, Stream_Mode, Name);
               WF.File_Access := Stream (WF.File);
               Audio.Wavefiles.Read.Read_Until_Data_Start (WF);
               Close (WF.File);
            end Parse_Wavefile;
         when Out_File =>
            null;
      end case;

      Open_Wavefile : declare
         Stream_Mode : constant Ada.Streams.Stream_IO.File_Mode :=
                         Ada.Streams.Stream_IO.File_Mode (Mode);
      begin
         Ada.Streams.Stream_IO.Open (WF.File, Stream_Mode, Name);
         WF.File_Access := Ada.Streams.Stream_IO.Stream (WF.File);
      end Open_Wavefile;

      case Mode is
         when In_File =>
            Audio.Wavefiles.Read.Read_Until_Data_Start (WF);
         when Append_File =>
            null;
         when Out_File =>
            Audio.Wavefiles.Write.Write_Until_Data_Start (WF);
      end case;

      WF.Is_Opened := WF.No_Errors;

      if WF.Is_Opened then
         Check_Consistency (WF);
      else
         --
         --  Close file in case of error
         --
         if Ada.Streams.Stream_IO.Is_Open (WF.File) then
            Ada.Streams.Stream_IO.Close (WF.File);
         end if;
      end if;
   end Open;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File
     (Sample_Pos : Sample_Info) return Boolean
   is (Sample_Pos.Current > (Sample_Pos.Total - Total_To_Last_Diff));

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File
     (WF : in out Wavefile) return Boolean
   is
   begin
      if End_Of_File (WF.Sample_Pos) or
        Ada.Streams.Stream_IO.End_Of_File (WF.File)
      then
         return True;
      else
         return False;
      end if;
   end End_Of_File;

   -----------
   -- Close --
   -----------

   procedure Close (WF : in out Wavefile) is
      use Ada.Streams.Stream_IO;
   begin
      WF.Reset_Errors;

      if not WF.Is_Opened then
         WF.Set_Error (Wavefile_Error_File_Not_Open);
         return;
      end if;

      if Mode (WF) = Out_File then
         Audio.Wavefiles.Write.Update_Data_Size (WF);
      end if;

      Close (WF.File);

      WF.Is_Opened := False;
   end Close;

   ----------------------------
   -- Set_Format_Of_Wavefile --
   ----------------------------

   procedure Set_Format_Of_Wavefile
     (WF     : in out Wavefile;
      Format :        Wave_Format_Extensible) is
   begin
      WF.Wave_Format := Format;
   end Set_Format_Of_Wavefile;

   ------------------------
   -- Format_Of_Wavefile --
   ------------------------

   function Format_Of_Wavefile
     (W : Wavefile) return  Wave_Format_Extensible is
   begin
      return W.Wave_Format;
   end Format_Of_Wavefile;

   ------------------------
   -- Number_Of_Channels --
   ------------------------

   function Number_Of_Channels
     (W : Wavefile) return Positive is (Positive (W.Wave_Format.Channels));

   ---------------
   -- Bit_Depth --
   ---------------

   function Bit_Depth
     (W : Wavefile) return Wav_Bit_Depth is
     (W.Wave_Format.Bits_Per_Sample);

   -----------------
   -- Sample_Rate --
   -----------------

   function Sample_Rate
     (W : Wavefile) return Wav_Sample_Rate is
     (W.Wave_Format.Samples_Per_Sec);

   ----------
   -- Mode --
   ----------

   function Mode (W : Wavefile) return File_Mode is
     (if Mode (W.File) = In_File then In_File else Out_File);

   ----------
   -- Name --
   ----------

   function Name
     (W : Wavefile) return String is
     (Ada.Streams.Stream_IO.Name (W.File));

   -------------------------
   -- Is_Supported_Format --
   -------------------------

   function Is_Supported_Format (W : Wave_Format_Extensible)
                                 return Boolean is
   begin
      if not (W.Sub_Format = GUID_Undefined
              or W.Sub_Format = GUID_PCM
              or W.Sub_Format = GUID_IEEE_Float)
      then
         return False;
      end if;

      return True;
   end Is_Supported_Format;

   -------------------
   -- Get_RIFF_Info --
   -------------------

   procedure Get_RIFF_Info
     (WF     : in out Wavefile;
      Info   :    out RIFF_Information)
   is
   begin
      if WF.RIFF_Info.Chunks.Is_Empty then
         Audio.Wavefiles.Read.Parse_Wav_Chunks (WF);
      end if;
      Info := WF.RIFF_Info;
   end Get_RIFF_Info;

   ---------------------
   -- Reset_RIFF_Info --
   ---------------------

   procedure Reset_RIFF_Info
     (Info :      out RIFF_Information) is
   begin
      Info.Format  := RIFF_Format_Unknown;
      Info.Id      := RIFF_Identifier_Unknown;
      Info.Chunks.Clear;
   end Reset_RIFF_Info;

   ------------------------
   -- Chunk_Element_Data --
   ------------------------

   function Chunk_Element_Data
     (WF            : Wavefile;
      Chunk_Element : Wav_Chunk_Element) return Byte_Array
   is
      subtype Bounded_Byte_Array is Byte_Array (1 .. Chunk_Element.Size);

      Data            : Bounded_Byte_Array;
      Prev_File_Index : constant Ada.Streams.Stream_IO.Positive_Count :=
                          Ada.Streams.Stream_IO.Index (WF.File);

      use Audio.Wavefiles.Internals;
   begin
      Set_File_Index_To_Chunk_Data_Start (WF.File, Chunk_Element.Start_Index);
      Bounded_Byte_Array'Read (WF.File_Access, Data);

      --  Set index to previous position
      Ada.Streams.Stream_IO.Set_Index (WF.File, Prev_File_Index);

      return Data;
   end Chunk_Element_Data;

   ----------------------
   -- Find_First_Chunk --
   ----------------------

   procedure Find_First_Chunk
     (Chunks    :     Wav_Chunk_Elements;
      Chunk_Tag :     Wav_Chunk_Tag;
      Found     : out Wav_Chunk_Element_Found) is
   begin
      Found := (Success => False);

      for C of Chunks loop
         if C.Chunk_Tag = Chunk_Tag then
            Found := (Success       => True,
                      Chunk_Element => C);
            exit;
         end if;
      end loop;
   end Find_First_Chunk;

   --------------------
   -- Current_Sample --
   --------------------

   function Current_Sample
     (WF : Wavefile) return Sample_Count is (WF.Sample_Pos.Current);

   -----------------
   -- Last_Sample --
   -----------------

   function Last_Sample
     (WF : Wavefile) return Sample_Count
   is (WF.Sample_Pos.Total - Total_To_Last_Diff);

   ------------------------
   -- Total_Sample_Count --
   ------------------------

   function Total_Sample_Count
     (WF : Wavefile) return Sample_Count is (WF.Sample_Pos.Total);

   ------------------------
   -- Set_Current_Sample --
   ------------------------

   procedure Set_Current_Sample
     (WF       : in out Wavefile;
      Position :        Sample_Count) is
   begin
      Wavefiles.Read.Set_Current_Sample (WF, Position);
   end Set_Current_Sample;

   ---------------------------------
   -- To_Wavefile_Time_In_Seconds --
   ---------------------------------

   function To_Wavefile_Time_In_Seconds
     (WF     : Wavefile;
      Sample : Sample_Count) return Wavefile_Time_In_Seconds
   is (Wavefile_Time_In_Seconds (Sample) /
         Wavefile_Time_In_Seconds
           (To_Positive (WF.Wave_Format.Samples_Per_Sec)));

   ---------------------
   -- To_Sample_Count --
   ---------------------

   function To_Sample_Count
     (WF      : Wavefile;
      At_Time : Wavefile_Time_In_Seconds) return Sample_Count
   is (Sample_Count
       (At_Time *
          Wavefile_Time_In_Seconds
            (To_Positive (WF.Wave_Format.Samples_Per_Sec)))
       + WF.First_Sample);

   ------------------
   -- Current_Time --
   ------------------

   function Current_Time
     (WF : Wavefile) return Wavefile_Time_In_Seconds
   is (To_Wavefile_Time_In_Seconds (WF,
                                    WF.Sample_Pos.Current - WF.First_Sample));

   --------------
   -- End_Time --
   --------------

   function End_Time
     (WF : Wavefile) return Wavefile_Time_In_Seconds
   is (To_Wavefile_Time_In_Seconds (WF, WF.Sample_Pos.Total));

   ----------------------
   -- Set_Current_Time --
   ----------------------

   procedure Set_Current_Time
     (WF      : in out Wavefile;
      At_Time :        Wavefile_Time_In_Seconds)
   is
      Position : constant Sample_Count := To_Sample_Count (WF, At_Time);
   begin
      Set_Current_Sample (WF, Position);
   end Set_Current_Time;

   ---------------
   -- Set_Error --
   ---------------

   procedure Set_Error (WF         : in out Wavefile;
                        Error_Code :        Wavefile_Error_Code) is
   begin
      WF.Errors (Error_Code) := True;
   end Set_Error;

   ------------------
   -- Reset_Errors --
   ------------------

   procedure Reset_Errors (WF      : in out Wavefile) is
   begin
      WF.Errors := (others => False);
   end Reset_Errors;

   -----------------
   -- Set_Warning --
   -----------------

   procedure Set_Warning (WF           : in out Wavefile;
                          Warning_Code :        Wavefile_Warning_Code) is
   begin
      WF.Warnings (Warning_Code) := True;
   end Set_Warning;

   --------------------
   -- Reset_Warnings --
   --------------------

   procedure Reset_Warnings (WF        : in out Wavefile) is
   begin
      WF.Warnings := (others => False);
   end Reset_Warnings;

end Audio.Wavefiles;
