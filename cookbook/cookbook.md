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

## Writing mono wavefile with silence

```ada
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

with Audio.Wavefiles;                      use Audio.Wavefiles;
with Audio.Wavefiles.Data_Types;           use Audio.Wavefiles.Data_Types;
with Audio.Wavefiles.Generic_Float_PCM_IO;

with Audio.RIFF.Wav.Formats;               use Audio.RIFF.Wav.Formats;

procedure Write_Silence_Mono_Wavefile is
   package PCM_IO is new Audio.Wavefiles.Generic_Float_PCM_IO
     (PCM_Sample    => Wav_Float_32,
      PCM_MC_Sample => Wav_Buffer_Float_32);
   use PCM_IO;

   package PCM_Elementary_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Wav_Float_32);

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

   Write_PCM_Vals : declare
      Last_Sample : constant Positive
        := Positive (Sample_Rate * Duration_In_Secs);
      PCM_Buf     : Wav_Buffer_Float_32 (1 .. Num_Channels);
   begin
      for Sample in 1 .. Last_Sample loop
         for J in PCM_Buf'Range loop
            PCM_Buf (J) := 0.0;
         end loop;
         Put (WF, PCM_Buf);
      end loop;
   end Write_PCM_Vals;

   if Is_Opened (WF) then
      Close (WF);
   end if;
end Write_Silence_Mono_Wavefile;
```

## Writing stereo wavefile with sine tone

```ada
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

with Audio.Wavefiles;                      use Audio.Wavefiles;
with Audio.Wavefiles.Data_Types;           use Audio.Wavefiles.Data_Types;
with Audio.Wavefiles.Generic_Float_PCM_IO;

with Audio.RIFF.Wav.Formats;               use Audio.RIFF.Wav.Formats;

procedure Write_Sine_Wavefile is

   package PCM_IO is new Audio.Wavefiles.Generic_Float_PCM_IO
     (PCM_Sample    => Wav_Float_32,
      PCM_MC_Sample => Wav_Buffer_Float_32);
   use PCM_IO;

   package PCM_Elementary_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Wav_Float_32);

   Wav_File_Name    : constant String := "out/2ch_sine.wav";
   Sample_Rate_Enum : constant Wav_Sample_Rate := Sample_Rate_44100;
   Num_Channels     : constant Positive := 2;
   Freq             : constant Wav_Buffer_Float_32 (1 .. Num_Channels)
     := (440.0, 220.0);
   Amp              : constant Wav_Buffer_Float_32 (1 .. Num_Channels)
     := (0.5, 0.25);
   Duration_In_Secs : constant := 0.2;
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

   Write_PCM_Vals : declare
      Last_Sample : constant Positive
        := Positive (Sample_Rate * Duration_In_Secs);
      Two_Pi      : constant := 2.0 * Ada.Numerics.Pi;
      PCM_Buf     : Wav_Buffer_Float_32 (1 .. Num_Channels);

      use PCM_Elementary_Functions;
   begin
      for Sample in 1 .. Last_Sample loop

         Write_PCM_Sample : declare
            P : constant Wav_Float_32 := Two_Pi * Wav_Float_32 (Sample)
                                         / Sample_Rate;
         begin
            for J in PCM_Buf'Range loop
               PCM_Buf (J) := Amp (J) * Sin (P * Freq (J));
            end loop;
            Put (WF, PCM_Buf);
         end Write_PCM_Sample;

      end loop;
   end Write_PCM_Vals;

   if Is_Opened (WF) then
      Close (WF);
   end if;
end Write_Sine_Wavefile;
```
