# Cookbook

## Opening & closing a wavefile for reading

```ada
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
   Open (WF, In_File, Wav_File_Name);

   --
   --  Verifying that the wavefile is opened
   --
   if Is_Opened (WF) then
      Put_Line ("File is open!");
   end if;

   --
   --  Closing the wavefile
   --
   Close (WF);

   --
   --  Verifying that the wavefile is closed
   --
   if not Is_Opened (WF) then
      Put_Line ("File is closed!");
   end if;

end Open_Close_Wavefile_For_Reading;
```

## Opening & closing a wavefile for writing

```ada
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
   Set_Format_Of_Wavefile (WF,
                           Init (Bit_Depth          => Bit_Depth_16,
                                 Sample_Rate        => Sample_Rate_44100,
                                 Number_Of_Channels => 2,
                                 Use_Float          => False));
   --
   --  Opening the wavefile
   --
   Open (WF, Out_File, Wav_File_Name);

   --
   --  Verifying that the wavefile is opened
   --
   if Is_Opened (WF) then
      Put_Line ("File is open!");
   end if;

   --
   --  Closing the wavefile
   --
   Close (WF);

   --
   --  Verifying that the wavefile is closed
   --
   if not Is_Opened (WF) then
      Put_Line ("File is closed!");
   end if;

end Open_Close_Wavefile_For_Writing;
```

## Reading data from a wavefile

```ada
with Ada.Text_IO;                          use Ada.Text_IO;

with Audio.Wavefiles;                      use Audio.Wavefiles;
with Audio.Wavefiles.Generic_Float_PCM_IO;

procedure Read_Display_Wavefile_Data is
   type Float_Array is array (Positive range <>) of Float;

   package PCM_IO is new Audio.Wavefiles.Generic_Float_PCM_IO
     (PCM_Sample    => Float,
      PCM_MC_Sample => Float_Array);
   use PCM_IO;

   WF            : Wavefile;
   Wav_File_Name : constant String := "data/2ch_silence.wav";
   Sample_Count  : Natural := 0;
   EOF           : Boolean;
begin
   Open (WF, In_File, Wav_File_Name);

   if Is_Opened (WF) then
      Put_Line ("Start reading: " & Wav_File_Name);
      New_Line;

      loop
         Read_One_Sample : declare
            PCM_Buf : constant Float_Array := Get (WF);
         begin
            EOF := Is_EOF (WF);
            exit when EOF;

            Sample_Count := Sample_Count + 1;

            Display_Sample : begin
               Put_Line ("Read sample #"
                         & Natural'Image (Sample_Count) & ".");

               for Channel_Number in PCM_Buf'Range loop
                  Put_Line ("    Channel # " & Positive'Image (Channel_Number)
                            & ": "  & Float'Image (PCM_Buf (Channel_Number)));
               end loop;
            end Display_Sample;

         end Read_One_Sample;
      end loop;

      New_Line;
      Put_Line ("Finished reading "
                & Positive'Image (Sample_Count) & " samples.");

      Close (WF);
   end if;

end Read_Display_Wavefile_Data;
```


## Writing mono wavefile with silence

```ada
with Audio.Wavefiles;                      use Audio.Wavefiles;
with Audio.Wavefiles.Data_Types;           use Audio.Wavefiles.Data_Types;
with Audio.Wavefiles.Generic_Float_PCM_IO;

with Audio.RIFF.Wav.Formats;               use Audio.RIFF.Wav.Formats;

procedure Write_Silence_Mono_Wavefile is
   package PCM_IO is new Audio.Wavefiles.Generic_Float_PCM_IO
     (PCM_Sample    => Wav_Float_32,
      PCM_MC_Sample => Wav_Buffer_Float_32);
   use PCM_IO;

   Wav_File_Name    : constant String := "out/1ch_silence.wav";
   Sample_Rate_Enum : constant Wav_Sample_Rate := Sample_Rate_44100;
   Num_Channels     : constant Positive := 1;
   Duration_In_Secs : constant := 0.1;
   Sample_Rate      : constant Wav_Float_32
     := Wav_Float_32 (To_Positive (Sample_Rate_Enum));

   WF                 : Wavefile;
begin
   Set_Format_Of_Wavefile (WF,
                           Init (Bit_Depth          => Bit_Depth_16,
                                 Sample_Rate        => Sample_Rate_Enum,
                                 Number_Of_Channels => Num_Channels,
                                 Use_Float          => False));

   Open (WF, Out_File, Wav_File_Name);

   if Is_Opened (WF) then

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

      Close (WF);

   end if;
end Write_Silence_Mono_Wavefile;
```

## Writing stereo wavefile with sine tone

```ada
with Audio.Wavefiles;                      use Audio.Wavefiles;

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
   Set_Format_Of_Wavefile (WF,
                           Init (Bit_Depth          => Bit_Depth_16,
                                 Sample_Rate        => Sample_Rate_Enum,
                                 Number_Of_Channels => Num_Channels,
                                 Use_Float          => False));

   Open (WF, Out_File, Wav_File_Name);

   if Is_Opened (WF) then
      Write_Stereo_Sine_Tone
        (WF           => WF,
         Sample_Rate  => Float (To_Positive (Sample_Rate_Enum)),
         Num_Channels => Num_Channels);

      Close (WF);
   end if;
end Write_Stereo_Sine_Wavefile;
```

## Writing 5.1-channel wavefile with sine tone

```ada
with Audio.Wavefiles;                      use Audio.Wavefiles;

procedure Write_5_1_Channel_Sine_Tone (WF           : in out Wavefile;
                                       Sample_Rate  :        Float;
                                       Num_Channels :        Positive);

with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

with Audio.Wavefiles.Data_Types;           use Audio.Wavefiles.Data_Types;
with Audio.Wavefiles.Generic_Float_PCM_IO;

procedure Write_5_1_Channel_Sine_Tone (WF           : in out Wavefile;
                                       Sample_Rate  :        Float;
                                       Num_Channels :        Positive)
is
   package PCM_IO is new Audio.Wavefiles.Generic_Float_PCM_IO
     (PCM_Sample    => Wav_Float_32,
      PCM_MC_Sample => Wav_Buffer_Float_32);
   use PCM_IO;

   package PCM_Elementary_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Wav_Float_32);
   use PCM_Elementary_Functions;

   Freq             : constant Wav_Buffer_Float_32 (1 .. Num_Channels)
     := (440.0, 220.0, 110.0, 55.0, 660.0, 880.0);
   Amp              : constant Wav_Buffer_Float_32 (1 .. Num_Channels)
     := (0.5, 0.25, 0.10, 0.05, 0.25, 0.5);
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
end Write_5_1_Channel_Sine_Tone;

with Audio.Wavefiles;        use Audio.Wavefiles;
with Audio.RIFF.Wav.Formats; use Audio.RIFF.Wav.Formats;

with Write_5_1_Channel_Sine_Tone;

procedure Write_5_1_Channel_Sine_Wavefile is
   Wav_File_Name    : constant String := "out/5_1ch_sine.wav";
   Sample_Rate_Enum : constant Wav_Sample_Rate := Sample_Rate_44100;
   Num_Channels     : constant Positive := 6;

   WF               : Wavefile;
   Wave_Format      : Wave_Format_Extensible;
   Channel_Config_5_1 : constant Channel_Mask_Type :=
     (Speaker_Front_Left    => True,
      Speaker_Front_Right   => True,
      Speaker_Front_Center  => True,
      Speaker_Low_Frequency => True,
      Speaker_Back_Left     => True,
      Speaker_Back_Right    => True,
      others                => False);

begin
   Wave_Format := Init (Bit_Depth          => Bit_Depth_16,
                        Sample_Rate        => Sample_Rate_Enum,
                        Number_Of_Channels => Num_Channels,
                        Use_Float          => False);
   Wave_Format.Channel_Mask := Channel_Config_5_1;

   Set_Format_Of_Wavefile (WF, Wave_Format);

   Open (WF, Out_File, Wav_File_Name);

   if Is_Opened (WF) then
      Write_5_1_Channel_Sine_Tone
        (WF           => WF,
         Sample_Rate  => Float (To_Positive (Sample_Rate_Enum)),
         Num_Channels => Num_Channels);

      Close (WF);
   end if;
end Write_5_1_Channel_Sine_Wavefile;
```
