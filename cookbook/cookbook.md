# Cookbook

## Opening & closing a wavefile for reading

~~~~~~~~~~ada
--
-------------------------------------------------------------------------------
--
--  Opening & closing a wavefile for reading
--
-------------------------------------------------------------------------------
--
with Ada.Text_IO;     use Ada.Text_IO;

with Audio.Wavefiles; use Audio.Wavefiles;

procedure Open_Close_Wavefile_For_Reading is
   WF            : Wavefile;
   Wav_File_Name : constant String := "data/2ch_silence.wav";
begin
   --
   --  Opening the wavefile
   --
   WF.Open (In_File, Wav_File_Name);

   --
   --  Verifying that the wavefile is opened
   --
   if WF.Is_Open then
      Put_Line ("File is open!");
   end if;

   --
   --  Closing the wavefile
   --
   WF.Close;

   --
   --  Verifying that the wavefile is closed
   --
   if not WF.Is_Open then
      Put_Line ("File is closed!");
   end if;

end Open_Close_Wavefile_For_Reading;
~~~~~~~~~~

## Opening & closing a wavefile for writing

~~~~~~~~~~ada
--
-------------------------------------------------------------------------------
--
--  Opening & closing a wavefile for writing with CD quality
--
-------------------------------------------------------------------------------
--
with Ada.Text_IO;            use Ada.Text_IO;

with Audio.Wavefiles;        use Audio.Wavefiles;
with Audio.RIFF.Wav.Formats; use Audio.RIFF.Wav.Formats;

procedure Open_Close_Wavefile_For_Writing is
   WF            : Wavefile;
   Wav_File_Name : constant String := "out/test.wav";
begin
   --
   --  Set format of the wavefile
   --
   WF.Set_Format_Of_Wavefile (Init (Bit_Depth          => Bit_Depth_16,
                                    Sample_Rate        => Sample_Rate_44100,
                                    Number_Of_Channels => 2,
                                    Use_Float          => False));
   --
   --  Create the wavefile
   --
   WF.Create (Out_File, Wav_File_Name);

   --
   --  Verifying that the wavefile is opened
   --
   if WF.Is_Open then
      Put_Line ("File is open!");
   end if;

   --
   --  Closing the wavefile
   --
   WF.Close;

   --
   --  Verifying that the wavefile is closed
   --
   if not WF.Is_Open then
      Put_Line ("File is closed!");
   end if;

end Open_Close_Wavefile_For_Writing;
~~~~~~~~~~

## Displaying errors and warnings while handling wavefiles

~~~~~~~~~~ada
with Audio.Wavefiles;        use Audio.Wavefiles;
with Audio.Wavefiles.Report; use Audio.Wavefiles.Report;

procedure Display_Errors_For_Wavefiles is
   WF            : Wavefile;
   Wav_File_Name : constant String := "data/2ch_silence.wav";
begin
   WF.Open (In_File, Wav_File_Name);

   Display_Errors (WF);
   Display_Warnings (WF);

   --  Trying to open a file twice
   --  This will be detected and indicated as an error
   WF.Open (In_File, Wav_File_Name);

   Display_Errors (WF);
   Display_Warnings (WF);

   WF.Close;
end Display_Errors_For_Wavefiles;
~~~~~~~~~~

## Displaying RIFF chunks of a wavefile

~~~~~~~~~~ada
with Audio.Wavefiles;        use Audio.Wavefiles;
with Audio.Wavefiles.Report; use Audio.Wavefiles.Report;

procedure Display_RIFF_Chunks is
   WF            : Wavefile;
   Wav_File_Name : constant String := "data/2ch_silence.wav";
   RIFF_Info     : RIFF_Information;
begin
   WF.Open (In_File, Wav_File_Name);

   if WF.Is_Open then
      WF.Get_RIFF_Info (RIFF_Info);

      Display_Info (RIFF_Info);
   end if;

   WF.Close;
end Display_RIFF_Chunks;
~~~~~~~~~~

## Reading data from a wavefile

~~~~~~~~~~ada
with Audio.Wavefiles;                      use Audio.Wavefiles;

procedure Put_Time (Item : Wavefile_Time_In_Seconds);

with Ada.Text_IO;                          use Ada.Text_IO;

procedure Put_Time (Item : Wavefile_Time_In_Seconds)
is
   package Time_Text_IO is new Ada.Text_IO.Float_IO
     (Wavefile_Time_In_Seconds);
begin
   Time_Text_IO.Put (Item, Fore => 3, Aft => 6, Exp => 0);
   Put_Line (" seconds.");
end Put_Time;

with Ada.Text_IO;                          use Ada.Text_IO;

with Audio.Wavefiles;                      use Audio.Wavefiles;
with Audio.Wavefiles.Generic_Float_PCM_IO;

with Put_Time;

procedure Read_Display_Wavefile_Data is
   type Float_Array is array (Positive range <>) of Float;

   package PCM_IO is new Audio.Wavefiles.Generic_Float_PCM_IO
     (PCM_Sample    => Float,
      Channel_Range => Positive,
      PCM_MC_Sample => Float_Array);
   use PCM_IO;

   WF            : Wavefile;
   Wav_File_Name : constant String := "data/2ch_silence.wav";
begin
   WF.Open (In_File, Wav_File_Name);

   if WF.Is_Open then
      Put_Line ("Start reading: " & Wav_File_Name);
      New_Line;

      loop
         Put_Line ("Reading sample #"
                   & Sample_Count'Image (WF.Current_Sample) & ".");
         Put ("Current time: ");
         Put_Time (WF.Current_Time);

         Read_One_Sample : declare
            PCM_Buf : constant Float_Array := Get (WF);
         begin
            Display_Sample : begin
               for Channel_Number in PCM_Buf'Range loop
                  Put_Line ("    Channel # " & Positive'Image (Channel_Number)
                            & ": "  & Float'Image (PCM_Buf (Channel_Number)));
               end loop;
            end Display_Sample;

            exit when WF.End_Of_File;

         end Read_One_Sample;
      end loop;

      New_Line;
      Put_Line ("Finished reading "
                & Sample_Count'Image (WF.Total_Sample_Count) & " samples.");
      Put ("End time: ");
      Put_Time (WF.End_Time);

      WF.Close;
   end if;

end Read_Display_Wavefile_Data;
~~~~~~~~~~

## Writing mono wavefile with silence

~~~~~~~~~~ada
with Audio.Wavefiles;                      use Audio.Wavefiles;
with Audio.Wavefiles.Data_Types;           use Audio.Wavefiles.Data_Types;
with Audio.Wavefiles.Generic_Float_PCM_IO;

with Audio.RIFF.Wav.Formats;               use Audio.RIFF.Wav.Formats;

procedure Write_Mono_Silence_Wavefile is
   package PCM_IO is new Audio.Wavefiles.Generic_Float_PCM_IO
     (PCM_Sample    => Wav_Float_32,
      Channel_Range => Wav_Buffer_Range,
      PCM_MC_Sample => Wav_Buffer_Float_32);
   use PCM_IO;

   Wav_File_Name    : constant String := "out/1ch_silence.wav";
   Sample_Rate_Enum : constant Wav_Sample_Rate := Sample_Rate_44100;
   Num_Channels     : constant Positive := 1;
   Duration_In_Secs : constant := 0.1;
   Sample_Rate      : constant Wav_Float_32
     := Wav_Float_32 (To_Float (Sample_Rate_Enum));

   WF                 : Wavefile;
begin
   WF.Set_Format_Of_Wavefile (Init (Bit_Depth          => Bit_Depth_16,
                                    Sample_Rate        => Sample_Rate_Enum,
                                    Number_Of_Channels => Num_Channels,
                                    Use_Float          => False));

   WF.Create (Out_File, Wav_File_Name);

   if WF.Is_Open then

      Write_Silence : declare
         Last_Sample : constant Positive
           := Positive (Sample_Rate * Duration_In_Secs);
         PCM_Buf     : Wav_Buffer_Float_32 (1 .. Num_Channels);
      begin
         for Sample in 1 .. Last_Sample loop
            for Channel_Number in PCM_Buf'Range loop
               PCM_Buf (Channel_Number) := 0.0;
            end loop;
            Put (WF, PCM_Buf);
         end loop;
      end Write_Silence;

      WF.Close;

   end if;
end Write_Mono_Silence_Wavefile;
~~~~~~~~~~

## Writing stereo wavefile with sine tone

~~~~~~~~~~ada
with Audio.Wavefiles; use Audio.Wavefiles;

procedure Write_Stereo_Sine_Tone (WF           : in out Wavefile;
                                  Sample_Rate  :        Float;
                                  Num_Channels :        Positive);

with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

with Audio.Wavefiles.Data_Types;           use Audio.Wavefiles.Data_Types;
with Audio.Wavefiles.Generic_Float_PCM_IO;

procedure Write_Stereo_Sine_Tone (WF           : in out Wavefile;
                                  Sample_Rate  :        Float;
                                  Num_Channels :        Positive)
is
   package PCM_IO is new Audio.Wavefiles.Generic_Float_PCM_IO
     (PCM_Sample    => Wav_Float_32,
      Channel_Range => Wav_Buffer_Range,
      PCM_MC_Sample => Wav_Buffer_Float_32);
   use PCM_IO;

   package PCM_Elementary_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Wav_Float_32);
   use PCM_Elementary_Functions;

   Freq             : constant Wav_Buffer_Float_32 (1 .. Num_Channels)
     := (440.0, 220.0);
   Amp              : constant Wav_Buffer_Float_32 (1 .. Num_Channels)
     := (0.5, 0.25);
   Duration_In_Secs : constant := 0.2;
   Last_Sample      : constant Positive
     := Positive (Sample_Rate * Duration_In_Secs);
   Two_Pi           : constant := 2.0 * Ada.Numerics.Pi;

   PCM_Buf          : Wav_Buffer_Float_32 (1 .. Num_Channels);
begin
   for Sample in 1 .. Last_Sample loop

      Write_Sine_Sample : declare
         P : constant Wav_Float_32 :=
               Wav_Float_32 (Two_Pi * Float (Sample) / Sample_Rate);
      begin
         for Ch_Num in PCM_Buf'Range loop
            PCM_Buf (Ch_Num) := Amp (Ch_Num) * Sin (P * Freq (Ch_Num));
         end loop;
         Put (WF, PCM_Buf);
      end Write_Sine_Sample;

   end loop;
end Write_Stereo_Sine_Tone;

with Audio.Wavefiles;        use Audio.Wavefiles;
with Audio.RIFF.Wav.Formats; use Audio.RIFF.Wav.Formats;

with Write_Stereo_Sine_Tone;

procedure Write_Stereo_Sine_Wavefile is
   Wav_File_Name    : constant String := "out/2ch_sine.wav";
   Sample_Rate_Enum : constant Wav_Sample_Rate := Sample_Rate_44100;
   Num_Channels     : constant Positive := 2;

   WF               : Wavefile;
begin
   WF.Set_Format_Of_Wavefile (Init (Bit_Depth          => Bit_Depth_16,
                                    Sample_Rate        => Sample_Rate_Enum,
                                    Number_Of_Channels => Num_Channels,
                                    Use_Float          => False));

   WF.Create (Out_File, Wav_File_Name);

   if WF.Is_Open then
      Write_Stereo_Sine_Tone
        (WF           => WF,
         Sample_Rate  => To_Float (Sample_Rate_Enum),
         Num_Channels => Num_Channels);

      WF.Close;
   end if;
end Write_Stereo_Sine_Wavefile;
~~~~~~~~~~

## Writing 5.1-channel wavefile with sine tone

~~~~~~~~~~ada
with Audio.Wavefiles; use Audio.Wavefiles;

procedure Write_5_1_Channel_Sine_Tone (WF           : in out Wavefile;
                                       Sample_Rate  :        Float);

with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

with Audio.Wavefiles.Data_Types;           use Audio.Wavefiles.Data_Types;
with Audio.Wavefiles.Generic_Float_PCM_IO;

with Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;
use  Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;

procedure Write_5_1_Channel_Sine_Tone (WF           : in out Wavefile;
                                       Sample_Rate  :        Float)
is
   type Wav_Buffer_5_1_Float_32  is
     array (Channel_Position_5_1 range <>) of Wav_Float_32;

   package PCM_IO is new Audio.Wavefiles.Generic_Float_PCM_IO
     (PCM_Sample    => Wav_Float_32,
      Channel_Range => Channel_Position_5_1,
      PCM_MC_Sample => Wav_Buffer_5_1_Float_32);
   use PCM_IO;

   package PCM_Elementary_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Wav_Float_32);
   use PCM_Elementary_Functions;

   Freq  : constant Wav_Buffer_5_1_Float_32
     := (F_L => 440.0, F_R => 220.0, F_C => 110.0, LFE =>  55.0,
         B_L => 660.0, B_R => 880.0);
   Amp   : constant Wav_Buffer_5_1_Float_32
     := (F_L => 0.50, F_R => 0.25, F_C => 0.10, LFE => 0.05,
         B_L => 0.25, B_R => 0.50);
   Duration_In_Secs : constant := 0.2;
   Last_Sample      : constant Positive
     := Positive (Sample_Rate * Duration_In_Secs);
   Two_Pi           : constant := 2.0 * Ada.Numerics.Pi;

   PCM_Buf          : Wav_Buffer_5_1_Float_32 (Channel_Position_5_1);
begin
   for Sample in 1 .. Last_Sample loop

      Write_Sine_Sample : declare
         P : constant Wav_Float_32 :=
               Wav_Float_32 (Two_Pi * Float (Sample) / Sample_Rate);
      begin
         for Ch_Num in PCM_Buf'Range loop
            PCM_Buf (Ch_Num) := Amp (Ch_Num) * Sin (P * Freq (Ch_Num));
         end loop;
         Put (WF, PCM_Buf);
      end Write_Sine_Sample;

   end loop;
end Write_5_1_Channel_Sine_Tone;

with Audio.Wavefiles;        use Audio.Wavefiles;
with Audio.RIFF.Wav.Formats; use Audio.RIFF.Wav.Formats;

with Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;
use  Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;

with Write_5_1_Channel_Sine_Tone;

procedure Write_5_1_Channel_Sine_Wavefile is
   Wav_File_Name    : constant String := "out/5_1ch_sine.wav";
   Sample_Rate_Enum : constant Wav_Sample_Rate := Sample_Rate_44100;
   Num_Channels     : constant Positive := 5 + 1;

   WF               : Wavefile;
   Wave_Format      : Wave_Format_Extensible;
begin
   Wave_Format := Init (Bit_Depth          => Bit_Depth_16,
                        Sample_Rate        => Sample_Rate_Enum,
                        Number_Of_Channels => Num_Channels,
                        Use_Float          => False);
   Wave_Format.Channel_Config := Channel_Config_5_1;

   WF.Set_Format_Of_Wavefile (Wave_Format);

   WF.Create (Out_File, Wav_File_Name);

   if WF.Is_Open then
      Write_5_1_Channel_Sine_Tone
        (WF           => WF,
         Sample_Rate  => To_Float (Sample_Rate_Enum));

      WF.Close;
   end if;
end Write_5_1_Channel_Sine_Wavefile;
~~~~~~~~~~

## Writing 7.1.4-channel wavefile with sine tone

~~~~~~~~~~ada
with Audio.Wavefiles; use Audio.Wavefiles;

procedure Write_7_1_4_Channel_Sine_Tone (WF           : in out Wavefile;
                                         Sample_Rate  :        Float);

with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

with Audio.Wavefiles.Data_Types;           use Audio.Wavefiles.Data_Types;
with Audio.Wavefiles.Generic_Float_PCM_IO;

with Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;
use  Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;

procedure Write_7_1_4_Channel_Sine_Tone (WF           : in out Wavefile;
                                         Sample_Rate  :        Float)
is
   type Wav_Buffer_7_1_4_Float_32  is
     array (Channel_Position_7_1_4 range <>) of Wav_Float_32;

   package PCM_IO is new Audio.Wavefiles.Generic_Float_PCM_IO
     (PCM_Sample    => Wav_Float_32,
      Channel_Range => Channel_Position_7_1_4,
      PCM_MC_Sample => Wav_Buffer_7_1_4_Float_32);
   use PCM_IO;

   package PCM_Elementary_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Wav_Float_32);
   use PCM_Elementary_Functions;

   Freq  : constant Wav_Buffer_7_1_4_Float_32
     := (F_L   =>  440.0, F_R   =>  220.0, F_C =>  110.0, LFE =>   55.0,
         B_L   =>  660.0, B_R   =>  880.0, S_L =>  330.0, S_R =>  550.0,
         T_F_L => 1320.0, T_F_R => 1540.0,
         T_B_L => 1760.0, T_B_R => 2200.0);
   Amp   : constant Wav_Buffer_7_1_4_Float_32
     := (F_L   => 0.50, F_R   => 0.25, F_C => 0.10, LFE =>  0.05,
         B_L   => 0.25, B_R   => 0.50, S_L => 0.20, S_R =>  0.60,
         T_F_L => 0.30, T_F_R => 0.40,
         T_B_L => 0.35, T_B_R => 0.15);
   Duration_In_Secs : constant := 0.2;
   Last_Sample      : constant Positive
     := Positive (Sample_Rate * Duration_In_Secs);
   Two_Pi           : constant := 2.0 * Ada.Numerics.Pi;

   PCM_Buf : Wav_Buffer_7_1_4_Float_32 (Channel_Position_7_1_4);
begin
   for Sample in 1 .. Last_Sample loop

      Write_Sine_Sample : declare
         P : constant Wav_Float_32 :=
               Wav_Float_32 (Two_Pi * Float (Sample) / Sample_Rate);
      begin
         for Ch_Num in PCM_Buf'Range loop
            PCM_Buf (Ch_Num) := Amp (Ch_Num) * Sin (P * Freq (Ch_Num));
         end loop;
         Put (WF, PCM_Buf);
      end Write_Sine_Sample;

   end loop;
end Write_7_1_4_Channel_Sine_Tone;

with Audio.Wavefiles;        use Audio.Wavefiles;
with Audio.RIFF.Wav.Formats; use Audio.RIFF.Wav.Formats;

with Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;
use  Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;

with Write_7_1_4_Channel_Sine_Tone;

procedure Write_7_1_4_Channel_Sine_Wavefile is
   Wav_File_Name    : constant String := "out/7_1_4ch_sine.wav";
   Sample_Rate_Enum : constant Wav_Sample_Rate := Sample_Rate_12000;
   Num_Channels     : constant Positive := 7 + 1 + 4;

   WF               : Wavefile;
   Wave_Format      : Wave_Format_Extensible;

begin
   Wave_Format := Init (Bit_Depth          => Bit_Depth_16,
                        Sample_Rate        => Sample_Rate_Enum,
                        Number_Of_Channels => Num_Channels,
                        Use_Float          => False);
   Wave_Format.Channel_Config := Channel_Config_7_1_4;

   WF.Set_Format_Of_Wavefile (Wave_Format);

   WF.Create (Out_File, Wav_File_Name);

   if WF.Is_Open then
      Write_7_1_4_Channel_Sine_Tone
        (WF           => WF,
         Sample_Rate  => To_Float (Sample_Rate_Enum));

      WF.Close;
   end if;
end Write_7_1_4_Channel_Sine_Wavefile;
~~~~~~~~~~

## Display channel configuration of a wavefile

~~~~~~~~~~ada
with Audio.RIFF.Wav.Formats; use Audio.RIFF.Wav.Formats;

procedure Display (Channel_Config : Channel_Configuration);

with Ada.Text_IO;            use Ada.Text_IO;

with Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;
use  Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;

procedure Display (Channel_Config : Channel_Configuration) is
begin
   if Channel_Config = Channel_Config_1_0 then
      Put_Line ("1.0 channels (mono)");
   elsif Channel_Config = Channel_Config_2_0 then
      Put_Line ("2.0 channels (stereo)");
   elsif Channel_Config = Channel_Config_3_0 then
      Put_Line ("3.0 channels");
   elsif Channel_Config = Channel_Config_4_0 then
      Put_Line ("4.0 channels (quad)");
   elsif Channel_Config = Channel_Config_5_0 then
      Put_Line ("5.0 channels");
   elsif Channel_Config = Channel_Config_5_1 then
      Put_Line ("5.1 channels");
   elsif Channel_Config = Channel_Config_7_0 then
      Put_Line ("7.0 channels");
   elsif Channel_Config = Channel_Config_7_1 then
      Put_Line ("7.1 channels");
   elsif Channel_Config = Channel_Config_7_1_BC then
      Put_Line ("7.1 channels + back channel");
   elsif Channel_Config = Channel_Config_5_1_2 then
      Put_Line ("5.1.2 channels");
   elsif Channel_Config = Channel_Config_5_1_4 then
      Put_Line ("5.1.4 channels");
   elsif Channel_Config = Channel_Config_7_0_4 then
      Put_Line ("7.0.4 channels");
   elsif Channel_Config = Channel_Config_7_1_2 then
      Put_Line ("7.1.2 channels");
   elsif Channel_Config = Channel_Config_7_1_4 then
      Put_Line ("7.1.4 channels");
   elsif Channel_Config = Channel_Config_Empty then
      Put_Line ("Unknown configuration");
   else
      Put_Line ("WARNING: configuration is not listed!");
   end if;
end Display;

with Audio.Wavefiles;        use Audio.Wavefiles;
with Audio.RIFF.Wav.Formats; use Audio.RIFF.Wav.Formats;

with Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;
use  Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;

with Display;

procedure Display_Channel_Config is
   WF            : Wavefile;
   Wav_File_Name : constant String := "ref/7_1_4ch_sine.wav";
begin
   WF.Open (In_File, Wav_File_Name);

   if WF.Is_Open then
      declare
         Channel_Config : constant Channel_Configuration :=
           Guessed_Channel_Configuration
             (WF.Number_Of_Channels);
      begin
         Display (Channel_Config);
      end;
   end if;

   WF.Close;
end Display_Channel_Config;
~~~~~~~~~~

## Append wavefile

~~~~~~~~~~ada
with Ada.Directories;

with Audio.Wavefiles;                      use Audio.Wavefiles;
with Audio.Wavefiles.Data_Types;           use Audio.Wavefiles.Data_Types;
with Audio.Wavefiles.Generic_Float_PCM_IO;

procedure Append_Wavefile is
   Wav_Ref_File_Name    : constant String := "ref/2ch_sine.wav";
   Wav_In_File_Name     : constant String := "ref/2ch_sine.wav";
   Wav_Append_File_Name : constant String := "out/2ch_sine_append.wav";

   WF_In     : Wavefile;
   WF_Append : Wavefile;
begin
   --  Create a copy of the reference wavefile, which we'll then append
   Copy_File_For_Appending : declare
      use Ada.Directories;
   begin
      Copy_File (Wav_Ref_File_Name, Wav_Append_File_Name);
   end Copy_File_For_Appending;

   WF_In.Open (In_File, Wav_In_File_Name);

   WF_Append.Open (Append_File, Wav_Append_File_Name);

   loop
      Append_PCM_MC_Sample : declare
         package PCM_IO is new Audio.Wavefiles.Generic_Float_PCM_IO
           (PCM_Sample    => Wav_Float_64,
            Channel_Range => Wav_Buffer_Range,
            PCM_MC_Sample => Wav_Buffer_Float_64);
         use PCM_IO;

         PCM_Buf : constant Wav_Buffer_Float_64 := Get (WF_In);
      begin
         Put (WF_Append, PCM_Buf);
         exit when WF_In.End_Of_File;
      end Append_PCM_MC_Sample;
   end loop;

   WF_In.Close;
   WF_Append.Close;
end Append_Wavefile;
~~~~~~~~~~


## Copy complete wavefile

~~~~~~~~~~ada
with Audio.Wavefiles;                      use Audio.Wavefiles;
with Audio.Wavefiles.Data_Types;           use Audio.Wavefiles.Data_Types;
with Audio.Wavefiles.Generic_Float_PCM_IO;

procedure Copy_Wavefile is
   Wav_In_File_Name    : constant String := "ref/2ch_sine.wav";
   Wav_Out_File_Name   : constant String := "out/2ch_sine.wav";

   WF_In  : Wavefile;
   WF_Out : Wavefile;
begin
   WF_In.Open (In_File, Wav_In_File_Name);

   WF_Out.Set_Format_Of_Wavefile (WF_In.Format_Of_Wavefile);

   WF_Out.Create (Out_File, Wav_Out_File_Name);

   loop
      Copy_PCM_MC_Sample : declare
         package PCM_IO is new Audio.Wavefiles.Generic_Float_PCM_IO
           (PCM_Sample    => Wav_Float_64,
            Channel_Range => Wav_Buffer_Range,
            PCM_MC_Sample => Wav_Buffer_Float_64);
         use PCM_IO;

         PCM_Buf : Wav_Buffer_Float_64 (1 .. WF_In.Number_Of_Channels);
      begin
         Get (WF_In,  PCM_Buf);
         Put (WF_Out, PCM_Buf);
         exit when WF_In.End_Of_File;
      end Copy_PCM_MC_Sample;
   end loop;

   WF_In.Close;
   WF_Out.Close;
end Copy_Wavefile;
~~~~~~~~~~

Alternatively:

```
      Copy_PCM_MC_Sample : declare
         --  ...

         PCM_Buf : constant Wav_Buffer_Float_64 := Get (WF_In);
      begin
         Put (WF_Out, PCM_Buf);
         exit when WF_In.End_Of_File;
      end Copy_PCM_MC_Sample;
```


## Copy complete wavefile using fixed-point buffer

~~~~~~~~~~ada
with Audio.Wavefiles;                      use Audio.Wavefiles;
with Audio.Wavefiles.Data_Types;           use Audio.Wavefiles.Data_Types;
with Audio.Wavefiles.Generic_Fixed_PCM_IO;

procedure Copy_Wavefile_Using_Fixed_Point_Buffer is
   Wav_In_File_Name  : constant String := "ref/2ch_sine.wav";
   Wav_Out_File_Name : constant String := "out/2ch_sine.wav";

   WF_In  : Wavefile;
   WF_Out : Wavefile;
begin
   WF_In.Open (In_File, Wav_In_File_Name);

   WF_Out.Set_Format_Of_Wavefile (WF_In.Format_Of_Wavefile);

   WF_Out.Create (Out_File, Wav_Out_File_Name);

   loop
      Copy_PCM_MC_Sample : declare
         package PCM_IO is new Audio.Wavefiles.Generic_Fixed_PCM_IO
           (PCM_Sample    => Wav_Fixed_16,
            Channel_Range => Wav_Buffer_Range,
            PCM_MC_Sample => Wav_Buffer_Fixed_16);
         use PCM_IO;

         PCM_Buf : Wav_Buffer_Fixed_16 (1 .. WF_In.Number_Of_Channels);
      begin
         Get (WF_In,  PCM_Buf);
         Put (WF_Out, PCM_Buf);
         exit when WF_In.End_Of_File;
      end Copy_PCM_MC_Sample;
   end loop;

   WF_In.Close;
   WF_Out.Close;
end Copy_Wavefile_Using_Fixed_Point_Buffer;
~~~~~~~~~~

## Copy parts of wavefile multiple times

~~~~~~~~~~ada
with Audio.Wavefiles; use Audio.Wavefiles;

procedure Display_Time_Info (WF       : Wavefile;
                             Preamble : String);

with Ada.Text_IO;

procedure Display_Time_Info (WF       : Wavefile;
                             Preamble : String) is
begin
   Ada.Text_IO.Put_Line (Preamble & " at "
                         & Wavefile_Time_In_Seconds'Image
                           (WF.Current_Time) & " seconds (at sample #"
                         & Sample_Count'Image (WF.Current_Sample)
                         & ")");

end Display_Time_Info;

with Audio.Wavefiles;                      use Audio.Wavefiles;
with Audio.Wavefiles.Data_Types;           use Audio.Wavefiles.Data_Types;
with Audio.Wavefiles.Generic_Float_PCM_IO;

with Display_Time_Info;

procedure Copy_Parts_Of_Wavefile is
   Wav_In_File_Name    : constant String := "data/2020-08-09.wav";
   Wav_Out_File_Name   : constant String := "out/looped_clip.wav";

   WF_In  : Wavefile;
   WF_Out : Wavefile;

   --  Start_Sample : constant Sample_Count := 4_607;

   Start_Time  : constant Wavefile_Time_In_Seconds := 0.095979166;
   Stop_Time   : constant Wavefile_Time_In_Seconds := 0.117084166;
   Repetitions : constant := 10;
begin
   WF_In.Open (In_File, Wav_In_File_Name);

   WF_Out.Set_Format_Of_Wavefile (WF_In.Format_Of_Wavefile);

   WF_Out.Create (Out_File, Wav_Out_File_Name);

   Display_Time_Info (WF_Out, "Writing wavefile");

   for I in 1 .. Repetitions loop
      --  We use Set_Current_Time to set the file index of the input wavefile
      --  to a specific position (indicated in seconds).
      --
      --  Note that Set_Current_Time performs an internal conversion of the
      --  time in seconds to retrieve the specific position in term of
      --  samples. We could set the wavefile position by indicating a sample
      --  position directly instead of a specific time. For example:
      --
      --     WF_In.Set_Current_Sample (Start_Sample);
      --
      WF_In.Set_Current_Time (Start_Time);

      Display_Time_Info (WF_In, "Starting loop");

      loop
         Copy_PCM_MC_Sample : declare
            package PCM_IO is new Audio.Wavefiles.Generic_Float_PCM_IO
              (PCM_Sample    => Wav_Float_64,
               Channel_Range => Wav_Buffer_Range,
               PCM_MC_Sample => Wav_Buffer_Float_64);
            use PCM_IO;

            PCM_Buf : Wav_Buffer_Float_64 (1 .. WF_In.Number_Of_Channels);
         begin
            Get (WF_In,  PCM_Buf);
            Put (WF_Out, PCM_Buf);
            exit when WF_In.Current_Time >= Stop_Time;
         end Copy_PCM_MC_Sample;
      end loop;

      Display_Time_Info (WF_In,  "Stopping loop");
      Display_Time_Info (WF_Out, "Writing wavefile");
   end loop;

   WF_In.Close;
   WF_Out.Close;
end Copy_Parts_Of_Wavefile;
~~~~~~~~~~

## Convert PCM wavefile to 32-bit floating-point PCM wavefile

~~~~~~~~~~ada
with Audio.Wavefiles;                      use Audio.Wavefiles;
with Audio.Wavefiles.Data_Types;           use Audio.Wavefiles.Data_Types;
with Audio.Wavefiles.Generic_Float_PCM_IO;
with Audio.RIFF.Wav.Formats;               use Audio.RIFF.Wav.Formats;

procedure Convert_Fixed_To_Float_Wavefile is
   Wav_In_File_Name  : constant String := "ref/2ch_sine.wav";
   Wav_Out_File_Name : constant String := "out/2ch_float_sine.wav";

   WF_In        : Wavefile;
   WF_Out       : Wavefile;
begin
   WF_In.Open (In_File,  Wav_In_File_Name);

   WF_Out.Set_Format_Of_Wavefile
     (Init (Bit_Depth          => Bit_Depth_32,
            Sample_Rate        => WF_In.Sample_Rate,
            Number_Of_Channels => WF_In.Number_Of_Channels,
            Use_Float          => True));

   WF_Out.Create (Out_File, Wav_Out_File_Name);

   loop
      Copy_PCM_MC_Sample : declare
         package PCM_IO is new Audio.Wavefiles.Generic_Float_PCM_IO
           (PCM_Sample    => Wav_Float_32,
            Channel_Range => Wav_Buffer_Range,
            PCM_MC_Sample => Wav_Buffer_Float_32);
         use PCM_IO;

         PCM_Buf : Wav_Buffer_Float_32 (1 .. WF_In.Number_Of_Channels);
      begin
         Get (WF_In,  PCM_Buf);
         Put (WF_Out, PCM_Buf);
         exit when WF_In.End_Of_File;
      end Copy_PCM_MC_Sample;
   end loop;

   WF_In.Close;
   WF_Out.Close;
end Convert_Fixed_To_Float_Wavefile;
~~~~~~~~~~

## Downmix stereo wavefile to mono wavefile

~~~~~~~~~~ada
with Audio.Wavefiles;                      use Audio.Wavefiles;
with Audio.Wavefiles.Data_Types;           use Audio.Wavefiles.Data_Types;
with Audio.Wavefiles.Generic_Float_PCM_IO;
with Audio.RIFF.Wav.Formats;               use Audio.RIFF.Wav.Formats;

procedure Downmix_Stereo_To_Mono_Wavefile is
   Wav_In_File_Name    : constant String := "ref/2ch_sine.wav";
   Wav_Out_File_Name   : constant String := "out/1ch_dmx_sine.wav";

   WF_In        : Wavefile;
   WF_Out       : Wavefile;
   WF_In_Format : Wave_Format_Extensible;
begin
   WF_In.Open (In_File,  Wav_In_File_Name);

   WF_In_Format := WF_In.Format_Of_Wavefile;

   WF_Out.Set_Format_Of_Wavefile
     (Init (Bit_Depth          => WF_In_Format.Bits_Per_Sample,
            Sample_Rate        => WF_In_Format.Samples_Per_Sec,
            Number_Of_Channels => 1,
            Use_Float          => WF_In_Format.Is_Float_Format));

   WF_Out.Create (Out_File, Wav_Out_File_Name);

   loop
      Downmix_PCM_MC_Sample : declare
         package PCM_IO is new Audio.Wavefiles.Generic_Float_PCM_IO
           (PCM_Sample    => Wav_Float_64,
            Channel_Range => Wav_Buffer_Range,
            PCM_MC_Sample => Wav_Buffer_Float_64);
         use PCM_IO;

         PCM_Buf_In  : Wav_Buffer_Float_64 (1 .. WF_In.Number_Of_Channels);
         PCM_Buf_Out : Wav_Buffer_Float_64 (1 .. 1);

         L : constant := 1;
         R : constant := 2;
      begin
         pragma Assert (PCM_Buf_In'Length = 2);

         Get (WF_In, PCM_Buf_In);
         PCM_Buf_Out (1) := PCM_Buf_In (L) * 0.5 + PCM_Buf_In (R) * 0.5;
         Put (WF_Out, PCM_Buf_Out);
         exit when WF_In.End_Of_File;
      end Downmix_PCM_MC_Sample;
   end loop;

   WF_In.Close;
   WF_Out.Close;
end Downmix_Stereo_To_Mono_Wavefile;
~~~~~~~~~~

## Downmix 5.1-channel wavefile to stereo wavefile

~~~~~~~~~~ada
with Audio.Wavefiles;                      use Audio.Wavefiles;
with Audio.Wavefiles.Data_Types;           use Audio.Wavefiles.Data_Types;
with Audio.Wavefiles.Generic_Float_PCM_IO;
with Audio.RIFF.Wav.Formats;               use Audio.RIFF.Wav.Formats;

with Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;
use  Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;

procedure Downmix_5_1_To_2_0_Wavefile is
   Wav_In_File_Name    : constant String := "out/5_1ch_sine.wav";
   Wav_Out_File_Name   : constant String := "out/2_0ch_dmx_sine.wav";

   WF_In        : Wavefile;
   WF_Out       : Wavefile;
   WF_In_Format : Wave_Format_Extensible;
begin
   WF_In.Open (In_File,  Wav_In_File_Name);

   WF_In_Format := WF_In.Format_Of_Wavefile;

   WF_Out.Set_Format_Of_Wavefile
     (Init (Bit_Depth          => WF_In_Format.Bits_Per_Sample,
            Sample_Rate        => WF_In_Format.Samples_Per_Sec,
            Number_Of_Channels => 2,
            Use_Float          => WF_In_Format.Is_Float_Format));

   WF_Out.Create (Out_File, Wav_Out_File_Name);

   loop
      Downmix_PCM_MC_Sample : declare
         type Wav_Buffer_2_0_Float_64  is
           array (Channel_Position_2_0 range <>) of Wav_Float_64;
         type Wav_Buffer_5_1_Float_64  is
           array (Channel_Position_5_1 range <>) of Wav_Float_64;

         package PCM_IO_2_0 is new Audio.Wavefiles.Generic_Float_PCM_IO
           (PCM_Sample    => Wav_Float_64,
            Channel_Range => Channel_Position_2_0,
            PCM_MC_Sample => Wav_Buffer_2_0_Float_64);
         use PCM_IO_2_0;

         package PCM_IO_5_1 is new Audio.Wavefiles.Generic_Float_PCM_IO
           (PCM_Sample    => Wav_Float_64,
            Channel_Range => Channel_Position_5_1,
            PCM_MC_Sample => Wav_Buffer_5_1_Float_64);
         use PCM_IO_5_1;

         PCM_Buf_In  : Wav_Buffer_5_1_Float_64 (Channel_Position_5_1);
         PCM_Buf_Out : Wav_Buffer_2_0_Float_64 (Channel_Position_2_0);
      begin
         pragma Assert (PCM_Buf_In'Length = 6);

         Get (WF_In, PCM_Buf_In);
         PCM_Buf_Out (F_L) :=   PCM_Buf_In (F_L) * 0.35
                              + PCM_Buf_In (F_C) * 0.25
                              + PCM_Buf_In (LFE) * 0.15
                              + PCM_Buf_In (B_L) * 0.25;
         PCM_Buf_Out (F_R) :=   PCM_Buf_In (F_R) * 0.35
                              + PCM_Buf_In (F_C) * 0.25
                              + PCM_Buf_In (LFE) * 0.15
                              + PCM_Buf_In (B_R) * 0.25;
         Put (WF_Out, PCM_Buf_Out);
         exit when WF_In.End_Of_File;
      end Downmix_PCM_MC_Sample;
   end loop;

   WF_In.Close;
   WF_Out.Close;
end Downmix_5_1_To_2_0_Wavefile;
~~~~~~~~~~

## Downmix 7.1.4-channel wavefile to 5.1-channel wavefile

~~~~~~~~~~ada
with Audio.Wavefiles;                      use Audio.Wavefiles;
with Audio.Wavefiles.Data_Types;           use Audio.Wavefiles.Data_Types;
with Audio.Wavefiles.Generic_Float_PCM_IO;
with Audio.RIFF.Wav.Formats;               use Audio.RIFF.Wav.Formats;

with Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;
use  Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;

procedure Downmix_7_1_4_To_5_1_Wavefile is
   Wav_In_File_Name    : constant String := "out/7_1_4ch_sine.wav";
   Wav_Out_File_Name   : constant String := "out/5_1ch_dmx_sine.wav";

   WF_In     : Wavefile;
   WF_Out    : Wavefile;
   WF_Format : Wave_Format_Extensible;
begin
   WF_In.Open (In_File,  Wav_In_File_Name);

   WF_Format := WF_In.Format_Of_Wavefile;
   WF_Format := Init (Bit_Depth          => WF_Format.Bits_Per_Sample,
                      Sample_Rate        => WF_Format.Samples_Per_Sec,
                      Number_Of_Channels => 5 + 1,
                      Use_Float          => WF_Format.Is_Float_Format);
   WF_Format.Channel_Config := Channel_Config_5_1;

   WF_Out.Set_Format_Of_Wavefile (WF_Format);

   WF_Out.Create (Out_File, Wav_Out_File_Name);

   loop
      Downmix_PCM_MC_Sample : declare
         type Wav_Buffer_5_1_Float_64  is
           array (Channel_Position_5_1 range <>) of Wav_Float_64;
         type Wav_Buffer_7_1_4_Float_64  is
           array (Channel_Position_7_1_4 range <>) of Wav_Float_64;

         package PCM_IO_5_1 is new Audio.Wavefiles.Generic_Float_PCM_IO
           (PCM_Sample    => Wav_Float_64,
            Channel_Range => Channel_Position_5_1,
            PCM_MC_Sample => Wav_Buffer_5_1_Float_64);
         use PCM_IO_5_1;

         package PCM_IO_7_1_4 is new Audio.Wavefiles.Generic_Float_PCM_IO
           (PCM_Sample    => Wav_Float_64,
            Channel_Range => Channel_Position_7_1_4,
            PCM_MC_Sample => Wav_Buffer_7_1_4_Float_64);
         use PCM_IO_7_1_4;

         PCM_Buf_In  : Wav_Buffer_7_1_4_Float_64 (Channel_Position_7_1_4);
         PCM_Buf_Out : Wav_Buffer_5_1_Float_64 (Channel_Position_5_1);
      begin
         pragma Assert (PCM_Buf_In'Length = 12);

         Get (WF_In, PCM_Buf_In);
         PCM_Buf_Out (F_L) :=   PCM_Buf_In (F_L)   * 0.4
                              + PCM_Buf_In (S_L)   * 0.2
                              + PCM_Buf_In (T_F_L) * 0.4;
         PCM_Buf_Out (F_R) :=   PCM_Buf_In (F_R)   * 0.4
                              + PCM_Buf_In (S_R)   * 0.2
                              + PCM_Buf_In (T_F_R) * 0.4;
         PCM_Buf_Out (F_C) :=   PCM_Buf_In (F_C)   * 0.4;
         PCM_Buf_Out (LFE) :=   PCM_Buf_In (LFE)   * 0.4;
         PCM_Buf_Out (B_L) :=   PCM_Buf_In (B_L)   * 0.4
                              + PCM_Buf_In (S_L)   * 0.2
                              + PCM_Buf_In (T_B_L) * 0.4;
         PCM_Buf_Out (B_R) :=   PCM_Buf_In (B_R)   * 0.4
                              + PCM_Buf_In (S_R)   * 0.2
                              + PCM_Buf_In (T_B_R) * 0.4;
         Put (WF_Out, PCM_Buf_Out);
         exit when WF_In.End_Of_File;
      end Downmix_PCM_MC_Sample;
   end loop;

   WF_In.Close;
   WF_Out.Close;
end Downmix_7_1_4_To_5_1_Wavefile;
~~~~~~~~~~

## Direct copy complete wavefile without PCM buffer conversion

~~~~~~~~~~ada
with Audio.Wavefiles;                      use Audio.Wavefiles;
with Audio.Wavefiles.Data_Types;           use Audio.Wavefiles.Data_Types;
with Audio.Wavefiles.Generic_Fixed_Wav_IO;
with Audio.RIFF.Wav.Formats;               use Audio.RIFF.Wav.Formats;

procedure Direct_Copy_Wavefile is
   Wav_In_File_Name  : constant String := "ref/2ch_sine.wav";
   Wav_Out_File_Name : constant String := "out/2ch_sine.wav";

   WF_In  : Wavefile;
   WF_Out : Wavefile;
begin
   WF_In.Open (In_File, Wav_In_File_Name);

   WF_Out.Set_Format_Of_Wavefile (WF_In.Format_Of_Wavefile);

   WF_Out.Create (Out_File, Wav_Out_File_Name);

   loop
      Copy_Wav_MC_Sample : declare
         pragma Assert
           (WF_In.Format_Of_Wavefile.Bits_Per_Sample = Bit_Depth_16
            and then not WF_In.Format_Of_Wavefile.Is_Float_Format);

         package Wav_IO is new Audio.Wavefiles.Generic_Fixed_Wav_IO
           (Wav_Sample    => Wav_Fixed_16,
            Channel_Range => Wav_Buffer_Range,
            Wav_MC_Sample => Wav_Buffer_Fixed_16);
         use Wav_IO;

         Wav_Buf : Wav_Buffer_Fixed_16 (1 .. WF_In.Number_Of_Channels);
      begin
         Get (WF_In,  Wav_Buf);
         Put (WF_Out, Wav_Buf);
         exit when WF_In.End_Of_File;
      end Copy_Wav_MC_Sample;
   end loop;

   WF_In.Close;
   WF_Out.Close;
end Direct_Copy_Wavefile;
~~~~~~~~~~

## Direct copy complete floating-point wavefile without PCM buffer conversion

~~~~~~~~~~ada
with Audio.Wavefiles;                      use Audio.Wavefiles;
with Audio.Wavefiles.Data_Types;           use Audio.Wavefiles.Data_Types;
with Audio.Wavefiles.Generic_Float_Wav_IO;
with Audio.RIFF.Wav.Formats;               use Audio.RIFF.Wav.Formats;

procedure Direct_Copy_Float_Wavefile is
   Wav_In_File_Name  : constant String := "ref/2ch_float_sine.wav";
   Wav_Out_File_Name : constant String := "out/2ch_float_sine.wav";

   WF_In  : Wavefile;
   WF_Out : Wavefile;
begin
   WF_In.Open (In_File, Wav_In_File_Name);

   WF_Out.Set_Format_Of_Wavefile (WF_In.Format_Of_Wavefile);

   WF_Out.Create (Out_File, Wav_Out_File_Name);

   loop
      Copy_Wav_MC_Sample : declare
         pragma Assert
           (WF_In.Format_Of_Wavefile.Bits_Per_Sample = Bit_Depth_32
            and then WF_In.Format_Of_Wavefile.Is_Float_Format);

         package Wav_IO is new Audio.Wavefiles.Generic_Float_Wav_IO
           (Wav_Sample    => Wav_Float_32,
            Channel_Range => Wav_Buffer_Range,
            Wav_MC_Sample => Wav_Buffer_Float_32);
         use Wav_IO;

         Wav_Buf : constant Wav_Buffer_Float_32 := Get (WF_In);
      begin
         Put (WF_Out, Wav_Buf);
         exit when WF_In.End_Of_File;
      end Copy_Wav_MC_Sample;
   end loop;

   WF_In.Close;
   WF_Out.Close;
end Direct_Copy_Float_Wavefile;
~~~~~~~~~~

## Convert 8-bit wavefile to 16-bit wavefile

~~~~~~~~~~ada
with Ada.Text_IO;                          use Ada.Text_IO;

with Audio.Wavefiles;                      use Audio.Wavefiles;
with Audio.Wavefiles.Data_Types;           use Audio.Wavefiles.Data_Types;
with Audio.Wavefiles.Generic_Fixed_Wav_IO;
with Audio.RIFF.Wav.Formats;               use Audio.RIFF.Wav.Formats;

procedure Convert_8_Bit_To_16_Bit_Wavefile is

   package Wav_IO_8 is new Audio.Wavefiles.Generic_Fixed_Wav_IO
     (Wav_Sample    => Wav_Unsigned_Fixed_8,
      Channel_Range => Wav_Buffer_Range,
      Wav_MC_Sample => Wav_Buffer_Unsigned_Fixed_8);
   use Wav_IO_8;

   package Wav_IO_16 is new Audio.Wavefiles.Generic_Fixed_Wav_IO
     (Wav_Sample    => Wav_Fixed_16,
      Channel_Range => Wav_Buffer_Range,
      Wav_MC_Sample => Wav_Buffer_Fixed_16);
   use Wav_IO_16;

   Wav_In_File_Name  : constant String := "data/2ch_8bit_sine.wav";
   Wav_Out_File_Name : constant String := "out/2ch_16bit_sine.wav";

   WF_In     : Wavefile;
   WF_Out    : Wavefile;
   WF_Format : Wave_Format_Extensible;
begin
   WF_In.Open (In_File, Wav_In_File_Name);

   WF_Format := WF_In.Format_Of_Wavefile;
   WF_Format.Bits_Per_Sample := Bit_Depth_16;

   WF_Out.Set_Format_Of_Wavefile (WF_Format);

   WF_Out.Create (Out_File, Wav_Out_File_Name);

   if WF_In.Is_Open then
      Put_Line ("Start conversion: " & Wav_In_File_Name);
      New_Line;

      loop
         Put_Line ("Converting sample #"
                   & Sample_Count'Image (WF_In.Current_Sample) & ".");

         Convert_Wav_MC_Sample : declare
            pragma Assert
              (WF_In.Format_Of_Wavefile.Bits_Per_Sample = Bit_Depth_8
               and then not WF_In.Format_Of_Wavefile.Is_Float_Format);

            Wav_Buf_In  : Wav_Buffer_Unsigned_Fixed_8
                            (1 .. WF_In.Number_Of_Channels);
            Wav_Buf_Out : Wav_Buffer_Fixed_16 (1 .. WF_In.Number_Of_Channels);

            --  Offset to convert from unsigned to signed range.
            --   8-bit wavefiles: [ 0.0, 2.0);
            --  16-bit wavefiles: [-1.0, 1.0);
            Offset : constant := -1.0;
         begin
            Get (WF_In,  Wav_Buf_In);

            for I in Wav_Buf_In'Range loop
               Wav_Buf_Out (I) := Wav_Fixed_16 (Wav_Buf_In (I) + Offset);
            end loop;

            Put (WF_Out, Wav_Buf_Out);

            Display_Sample : begin
               for Channel_Number in Wav_Buf_In'Range loop
                  Put_Line ("    Channel # " & Positive'Image (Channel_Number)
                            & ": "
                            & Wav_Unsigned_Fixed_8'Image
                               (Wav_Buf_In (Channel_Number))
                            & " => "
                            & Wav_Fixed_16'Image
                               (Wav_Buf_Out (Channel_Number)));
               end loop;
            end Display_Sample;

            exit when WF_In.End_Of_File;
         end Convert_Wav_MC_Sample;
      end loop;

      New_Line;
      Put_Line ("Finished converting "
                & Sample_Count'Image (WF_In.Total_Sample_Count) & " samples.");
      WF_In.Close;
   end if;

   WF_In.Close;
   WF_Out.Close;
end Convert_8_Bit_To_16_Bit_Wavefile;
~~~~~~~~~~

## Read complete wavefile into memory (channel-interleaved data)

~~~~~~~~~~ada
with Ada.Text_IO;

with Audio.Wavefiles;                      use Audio.Wavefiles;
with Audio.Wavefiles.Data_Types;           use Audio.Wavefiles.Data_Types;
with Audio.Wavefiles.Generic_Float_PCM_IO;

procedure Read_To_Memory_Channel_Interleaved is
   Wav_In_File_Name  : constant String := "ref/2ch_float_sine.wav";

   WF_In  : Wavefile;
begin
   WF_In.Open (In_File, Wav_In_File_Name);

   Read_To_Memory : declare
      subtype Wav_Bounded_Buffer_Float_32 is
        Wav_Buffer_Float_32 (1 .. WF_In.Number_Of_Channels);

      package PCM_IO is new Audio.Wavefiles.Generic_Float_PCM_IO
        (PCM_Sample    => Wav_Float_32,
         Channel_Range => Wav_Buffer_Range,
         PCM_MC_Sample => Wav_Buffer_Float_32);
      use PCM_IO;

      procedure Display_Sample (MC_Sample    : Wav_Bounded_Buffer_Float_32;
                                Sample_Count : Long_Long_Integer);

      procedure Display_Sample (MC_Sample    : Wav_Bounded_Buffer_Float_32;
                                Sample_Count : Long_Long_Integer)
      is
         use Ada.Text_IO;
      begin
         Put_Line ("Sample #" & Long_Long_Integer'Image (Sample_Count));
         for Channel_Count in MC_Sample'Range loop
            Put_Line ("    Channel # " & Positive'Image (Channel_Count)
                      & ": "
                      & Wav_Float_32'Image (MC_Sample (Channel_Count)));
         end loop;
      end Display_Sample;

      type PCM_Container is array (Long_Long_Integer range <>) of
        Wav_Bounded_Buffer_Float_32;

      PCM_Data : PCM_Container (WF_In.First_Sample .. WF_In.Last_Sample);

      Max_Samples_To_Display : constant := 10;
      Last_Sample_To_Display : constant Long_Long_Integer
        := Long_Long_Integer'Min (Max_Samples_To_Display + PCM_Data'First - 1,
                                  PCM_Data'Last);
   begin
      for Sample_Count in PCM_Data'Range loop
         pragma Assert (not WF_In.End_Of_File);
         PCM_Data (Sample_Count) := Get (WF_In);
      end loop;

      --  At this point, we have all PCM data from the wavefile stored in
      --  PCM_Data. Let's display a couple of samples:

      for Sample_Count in PCM_Data'First .. Last_Sample_To_Display loop
         Display_Sample (PCM_Data (Sample_Count), Sample_Count);
      end loop;

   end Read_To_Memory;

   WF_In.Close;
end Read_To_Memory_Channel_Interleaved;
~~~~~~~~~~

## Read complete wavefile into memory (data per channel)

~~~~~~~~~~ada
with Ada.Text_IO;

with Audio.Wavefiles;                      use Audio.Wavefiles;
with Audio.Wavefiles.Data_Types;           use Audio.Wavefiles.Data_Types;
with Audio.Wavefiles.Generic_Float_PCM_IO;

procedure Read_To_Memory_Per_Channel is

   type Channel_PCM_Data is array (Long_Long_Integer range <>) of
     Wav_Float_32;

   procedure Display_Samples (Samples                : Channel_PCM_Data;
                              Channel_Index          : Positive;
                              Last_Sample_To_Display : Long_Long_Integer);

   procedure Display_Samples (Samples                : Channel_PCM_Data;
                              Channel_Index          : Positive;
                              Last_Sample_To_Display : Long_Long_Integer)
   is
      use Ada.Text_IO;

      Samples_Last : constant Long_Long_Integer :=
                       Long_Long_Integer'Min (Samples'Last,
                                                 Last_Sample_To_Display);
   begin
      Put_Line ("Channel #" & Positive'Image (Channel_Index));
      for Sample_Count in Samples'First .. Samples_Last loop
         Put_Line ("    Sample # " & Long_Long_Integer'Image (Sample_Count)
                   & ": "
                   & Wav_Float_32'Image (Samples (Sample_Count)));
      end loop;
   end Display_Samples;

   Wav_In_File_Name  : constant String := "ref/2ch_float_sine.wav";

   WF_In  : Wavefile;
begin
   WF_In.Open (In_File, Wav_In_File_Name);

   Read_To_Memory : declare
      package PCM_IO is new Audio.Wavefiles.Generic_Float_PCM_IO
        (PCM_Sample    => Wav_Float_32,
         Channel_Range => Wav_Buffer_Range,
         PCM_MC_Sample => Wav_Buffer_Float_32);
      use PCM_IO;

      subtype Bounded_Channel_PCM_Data is
        Channel_PCM_Data (WF_In.First_Sample .. WF_In.Last_Sample);

      type PCM_Container is array (Positive range <>) of
        Bounded_Channel_PCM_Data;

      PCM_Data : PCM_Container (1 .. WF_In.Number_Of_Channels);

      Max_Samples_To_Display : constant := 10;
      Last_Sample_To_Display : constant Long_Long_Integer
        := Long_Long_Integer'Min (Max_Samples_To_Display
                                  + Bounded_Channel_PCM_Data'First - 1,
                                  Bounded_Channel_PCM_Data'Last);
   begin
      for Sample_Count in Bounded_Channel_PCM_Data'Range loop
         pragma Assert (not WF_In.End_Of_File);

         Read_Sample : declare
            Wav_Buf : constant Wav_Buffer_Float_32 := Get (WF_In);
         begin
            for Channel_Count in PCM_Data'Range loop
               PCM_Data (Channel_Count) (Sample_Count) :=
                 Wav_Buf (Channel_Count);
            end loop;
         end Read_Sample;
      end loop;

      --  At this point, we have all PCM data from the wavefile stored in
      --  PCM_Data. Let's display a couple of samples:

      for Channel_Count in PCM_Data'Range loop
         Display_Samples (Samples                => PCM_Data (Channel_Count),
                          Channel_Index          => Channel_Count,
                          Last_Sample_To_Display => Last_Sample_To_Display);
      end loop;
   end Read_To_Memory;

   WF_In.Close;
end Read_To_Memory_Per_Channel;
~~~~~~~~~~

## Extract iXML chunk from a wavefile

~~~~~~~~~~ada

with Ada.Text_IO;     use Ada.Text_IO;

with Audio.Wavefiles; use Audio.Wavefiles;

procedure Write_Data_As_Text (Text_File : File_Type;
                              Data      : Byte_Array);

procedure Write_Data_As_Text (Text_File : File_Type;
                              Data      : Byte_Array) is
begin
   for B of Data loop
      declare
         C : Character with Address => B'Address;
      begin
         Put (Text_File, C);
      end;
   end loop;
end Write_Data_As_Text;

with Ada.Text_IO;            use Ada.Text_IO;

with Audio.Wavefiles;        use Audio.Wavefiles;
with Audio.RIFF.Wav.Formats; use Audio.RIFF.Wav.Formats;

with Write_Data_As_Text;

procedure Extract_XML_Chunk is
   WF            : Wavefile;
   Xml_File      : Ada.Text_IO.File_Type;
   Wav_File_Name : constant String := "data/2020-08-09.wav";
   Xml_File_Name : constant String := "out/2020-08-09.xml";
   RIFF_Info     : RIFF_Information;
begin
   WF.Open (In_File, Wav_File_Name);

   Create (Xml_File, Out_File, Xml_File_Name);

   if WF.Is_Open then
      WF.Get_RIFF_Info (RIFF_Info);

      declare
         Chunk_Element  : Wav_Chunk_Element;
         Success        : Boolean;
      begin
         Get_First_Chunk (Chunks        => RIFF_Info.Chunks,
                          Chunk_Tag     => Wav_Chunk_IXML,
                          Chunk_Element => Chunk_Element,
                          Success       => Success);

         if Success then
            Write_Data_As_Text (Xml_File,
                                Chunk_Element_Data (WF, Chunk_Element));
         end if;
      end;
   end if;

   WF.Close;
   Close (Xml_File);
end Extract_XML_Chunk;
~~~~~~~~~~
