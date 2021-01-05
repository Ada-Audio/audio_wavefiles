---
title: Ada Wavefile Library
subtitle: Cookbook
description: Recipes for the Ada Wavefile Library with explanation.
lang: en-US
author: Gustavo A. Hoffmann
date: 2021-01
version: 2.0.0
keywords:
- Ada language
- Audio
- Wavefile
header-includes:
    - \usepackage{fancyhdr}
    - \pagestyle{fancy}
    - \fancyhead[LE,RO]{\title}
    - \usepackage{pmboxdraw}
    - \fvset{fontsize=\small}
monobackgroundcolor: grey
papersize: a4
margin-left: 3cm
margin-right: 2cm
margin-top: 2.5cm
margin-bottom: 2.5cm
toc: yes
link-citations: true
colorlinks: true
---

\pagebreak

# Preface

This *cookbook* contains a collection of source-code examples for various
use-cases. It starts with very simple examples, but soon evolves to more
complicated use-cases. The document provides an overview of the capabilities of
the Wavefile Library, and you can read it as a tutorial.

The goals of this document are twofold: we want to introduce the library to
developers using relevant source-code examples. Therefore, each section has a
small explanation that describes the most important aspects of the source-code
example. At the same, we want to implement the examples in such a way that the
occasional reader may reuse them in their own application with as few changes
as possible.


\pagebreak

# Opening & closing wavefiles

## Opening & closing a wavefile for reading

This example shows how to open a wavefile for reading. To do this, we make use
of  the `Wavefiles` package, which contains the main functionality of the
library. We first declare an object `WF` of the `Wavefile` type. We can then
open and close a wavefile by calling `WF.Open` and `WF.Close`. Note that we
use `In_File` as an argument to the `WF.Open` procedure to indicate that we
want an input file.

It is recommended that we call `WF.Is_Open` after a call to `WF.Open` to verify
whether the wavefile is actually open. In some situations, the library cannot
open a particular wavefile due to its format being unsupported.

As soon as a wavefile has been successfully opened, we can start reading data
from it. We'll discuss this topic in another example.

~~~~~~~~~~ada
with Ada.Text_IO;     use Ada.Text_IO;

with Audio.Wavefiles; use Audio.Wavefiles;

procedure Open_Close_Wavefile_For_Reading is
   WF : Wavefile;
begin
   WF.Open (In_File, "data/2ch_silence.wav");

   if WF.Is_Open then
      Put_Line ("File is open!");
   end if;

   WF.Close;

   if not WF.Is_Open then
      Put_Line ("File is closed!");
   end if;

end Open_Close_Wavefile_For_Reading;
~~~~~~~~~~


\pagebreak

## Opening & closing a wavefile for writing

This example shows how to open a wavefile for writing. First, we declare an
object `WF` of the `Wavefile` type. Then, it's recommended that we set the
format of the output wavefile by calling `Set_Format_Of_Wavefile`. Otherwise,
the default settings will be used. We can then specify the bit depth, sample
rate, and number of channels of the wavefile using this procedure. Note that we
first call the `Init` function from the `RIFF.Wav.Formats` package, which
returns an object of `Wave_Format_Extensible` type — based on the format
information that we've provided — and pass this object to the
`Set_Format_Of_Wavefile` procedure. In this example, we're specifying CD
quality (stereo, 16-bit, 44.1-kHz PCM audio) for the wavefile format. Also, we
use `False` for the `Use_Float` parameter to specify that we want to write
*linear* PCM data instead of floating-point PCM data.

We can then call the `Create` procedure to create the wavefile and the `Close`
procedure to finalize the wavefile and close it. Note that, instead of calling
`Create`, we could also call `Open` to open an existing wavefile. It is
recommended that we call `WF.Is_Open` after a call to `WF.Create` or `WF.Open`
to verify whether the wavefile is actually available.

~~~~~~~~~~ada
with Ada.Text_IO;            use Ada.Text_IO;

with Audio.Wavefiles;        use Audio.Wavefiles;
with Audio.RIFF.Wav.Formats; use Audio.RIFF.Wav.Formats;

procedure Open_Close_Wavefile_For_Writing is
   WF : Wavefile;
begin
   WF.Set_Format_Of_Wavefile (Init (Bit_Depth          => Bit_Depth_16,
                                    Sample_Rate        => Sample_Rate_44100,
                                    Number_Of_Channels => 2,
                                    Use_Float          => False));
   WF.Create (Out_File, "out/test.wav");

   if WF.Is_Open then
      Put_Line ("File is open!");
   end if;

   WF.Close;

   if not WF.Is_Open then
      Put_Line ("File is closed!");
   end if;

end Open_Close_Wavefile_For_Writing;
~~~~~~~~~~


\pagebreak

# Errors and warnings

## Displaying errors and warnings while handling wavefiles

We may encounter errors and warnings while handling wavefiles. In this
example, we call `Display_Errors` and `Display_Warnings` from the `Report`
child package to display errors and warnings found in the preceding subprogram
call on an object of `Wavefile` type — in this case, after calls to the `Open`
procedure.

Also, in this example, we intentionally create an error condition by trying
to open a file twice — we must always close a wavefile before opening another
one. Therefore, the application displays an error after the second call to the
`Open` procedure.

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

   --  Trying to open a file twice:
   --  this will be detected and indicated as an error.
   WF.Open (In_File, Wav_File_Name);

   Display_Errors (WF);
   Display_Warnings (WF);

   WF.Close;
end Display_Errors_For_Wavefiles;
~~~~~~~~~~


\pagebreak

## Listing errors and warnings while handling wavefiles

As mentioned
[earlier](#displaying-errors-and-warnings-while-handling-wavefiles), we may
encounter errors and warnings while handling wavefiles. Therefore, it's
recommended that we account for them when using the `Wavefiles` package. In
this example, we call the `Errors` and `Warnings` functions to retrieve the
errors and warnings found in the preceding subprogram call. Then, we evaluate
whether errors and warnings have been found and display them accordingly.

~~~~~~~~~~ada
with Ada.Text_IO;            use Ada.Text_IO;

with Audio.Wavefiles;        use Audio.Wavefiles;

procedure List_Errors_For_Wavefiles is
   WF            : Wavefile;
   Wav_File_Name : constant String := "data/2ch_silence.wav";
   Errors        : Wavefile_Errors;
   Warnings      : Wavefile_Warnings;
begin
   WF.Open (In_File, Wav_File_Name);

   --  Trying to open a file twice
   --  This will be detected and indicated as an error
   WF.Open (In_File, Wav_File_Name);

   Errors   := WF.Errors;
   Warnings := WF.Warnings;

   if Errors = No_Wavefile_Errors then
      Put_Line ("No errors!");
   else
      Put_Line ("Errors:");
      for E in Errors'Range loop
         if Errors (E) then
            Put_Line ("- " & Wavefile_Error_Code'Image (E));
         end if;
      end loop;
   end if;
   New_Line;

   if Warnings = No_Wavefile_Warnings then
      Put_Line ("No warnings!");
   else
      Put_Line ("Warnings:");
      for W in Warnings'Range loop
         if Warnings (W) then
            Put_Line ("- " & Wavefile_Warning_Code'Image (W));
         end if;
      end loop;
   end if;
   New_Line;

   WF.Close;
end List_Errors_For_Wavefiles;
~~~~~~~~~~


\pagebreak

# Reading PCM data

## Reading data from a wavefile

This example shows how to read data from a wavefile to a PCM buffer. First,
let's start by creating an auxiliary procedure that displays the current
position in the wavefile (in terms of time in seconds).

~~~~~~~~~~ada
with Audio.Wavefiles; use Audio.Wavefiles;

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
~~~~~~~~~~

After calling `Open` and `Is_Open` — to open the wavefile and verify whether it
is available —, we create a loop where we read the individual PCM samples from
the wavefile. Since wavefiles contain channel-interleaved PCM samples, each
call to the `Get` function retrieves the PCM samples for all channels in that
position.

Before we can use the `Get` function, however, we need to declare the `PCM_IO`
package, which is an instance of a generic package. We can instantiate a
package to create either fixed-point PCM buffers or floating-point PCM buffers.
In this example, we instantiate the `Generic_Float_PCM_IO` package (as the
`PCM_IO` package), which allows us to declare a floating-point PCM buffer. To
do this, we need to set the following formal parameters:

- the `PCM_Sample` type, which indicates the floating-point type for each PCM
  sample;

  - In this example, we're using the standard floating-point type (`Float`).

- the `Channel_Range` parameter, which indicates the range of the array
  containing the PCM samples for each channel;

  - Standard ranges such as `Positive` or `Natural` are obvious choices for
    this parameter.

  - In this example, we're using the `Positive` range, so that the index of the
    first PCM sample is one.

- the `PCM_MC_Sample` type, which indicates the array type of the PCM buffer
  containing one PCM sample for each channel.

  - Note that this type needs to match the previous two formal parameters, so
    that it matches the following type declaration:

    ```ada
    type PCM_MC_Sample is array (Channel_Range) of PCM_Sample;
    ```

In the `Read_One_Sample` block of the code below, we read the PCM samples.
These are the main steps we perform:

- we display the current position in the wavefile (in terms of sample count and
  time);

    - We retrieve the current sample position with a call to the
      `Current_Sample` function.

    - We also retrieve the current time with a call to the `Current_Time`
      function and display it using the `Put_Time` procedure that we
      implemented above.

- we get the current PCM sample of all channels by calling the `Get` function;

    - We store the PCM data from `Get` in a PCM buffer — in this example, it's
      the `PCM_Buf` array.

- we display the value of each sample of the PCM buffer (in the
  `Display_Sample` block);

- and we check whether we've reached the end of the file by calling the
  `End_Of_File` function.

After exiting the loop, we display the total number of samples and the end
time. We retrieve this information by calling the `Total_Sample_Count` function
and the `End_Time` function, respectively.

~~~~~~~~~~ada
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


\pagebreak

# Writing PCM data

## Writing a mono wavefile with silence

This example shows how to write PCM data to a short wavefile of 0.1 seconds. To
keep it simple, we just write digital silence into the output wavefile. The
first thing we need to do is declare the `PCM_IO` package. The
[previous section](#reading-data-from-a-wavefile) explains how to do that. In
this case, we're not using the standard `Float` type for the formal
`PCM_Sample` type. Instead, we use the custom `Wav_Float_32`,
`Wav_Buffer_Range` and `Wav_Buffer_Float_32` types — from the `Data_Types`
child package — for the formal parameters. The `Data_Types` package contains
type definitions for the most commonly used bit-depths associated with
audio and wavefile processing.

After creating the wavefile, we have the `Write_Silence` block, where we write
the PCM data. We calculate the index of the last sample — which is stored in
the `Last_Sample` constant — and use it in a loop that writes all samples. In
that loop, we initialize the PCM buffer (`PCM_Buf`) with silence and call the
`Put` procedure to write the data to the output wavefile.

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


\pagebreak

## Writing a stereo wavefile with sine tones

This example is similar to
[the previous one](#writing-a-mono-wavefile-with-silence), but we're now
creating a stereo wavefile using sine tones. In order to simplify the example,
we first implement the `Write_Stereo_Sine_Tone` procedure to just generate the
PCM samples and write them to the output wavefile.

We store the frequencies and amplitudes in the `Freq` and `Amp` constants,
respectively. Then, in the `Write_Sine_Sample`, we calculate the value of the
current sample for each channel using the `Sin` function and call the `Put`
procedure to write the PCM samples to the wavefile.

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
~~~~~~~~~~

The `Write_Stereo_Sine_Wavefile` just creates a new wavefile and calls the
`Write_Stereo_Sine_Tone` procedure to write the PCM samples.

~~~~~~~~~~ada
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


\pagebreak

## Writing a 5.1-channel wavefile with sine tones

This example is similar to
[the previous one](#writing-a-stereo-wavefile-with-sine-tones), but we're now
creating a 5.1-channel wavefile instead. Because of the increased number of
channels, we need to make sure that we're using the correct channel
configuration.

First, we implement the `Write_5_1_Channel_Sine_Tone` procedure that generates
the PCM data for the 5.1-channel configuration and writes the PCM samples to
the output wavefile. Note that we're using the `Channel_Position_5_1` range —
from the `RIFF.Wav.Formats.Standard_Channel_Configurations` child package — to
declare the `Wav_Buffer_5_1_Float_32` type, which is an array type specifically
designed for 5.1-channel configurations. We also use this range in the
declaration of the `PCM_IO` package and the `PCM_Buf` array.

To get the correct indices for the channel positions associated with the
standard 5.1-channel configuration, we're using the definitions from the
`Standard_Channel_Configurations`. For example, we use `F_L`, `F_R` and `B_L`
to access the front left, front right and back left channels, respectively.
Alternatively, we could use the full channel names for these channels — for the
previous example, this would be `Front_Left`, `Front_Right` and `Back_Left`.

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
~~~~~~~~~~

The `Write_5_1_Channel_Sine_Wavefile` creates a new wavefile and calls the
`Write_5_1_Channel_Sine_Tone` procedure to generate the PCM samples and write
them to the output wavefile.

Because we're now using a multichannel configuration, we should set the correct
channel configuration of the output wavefile. We do this by initializing the
channel configuration element of the wavefile format
(`Wave_Format.Channel_Config`) before calling the `Set_Format_Of_Wavefile`
procedure. Note that we're using the 5.1-channel configuration
(`Channel_Config_5_1`) defined in the
`RIFF.Wav.Formats.Standard_Channel_Configurations` child package.

~~~~~~~~~~ada
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


\pagebreak

## Writing a 7.1.4-channel wavefile with sine tones

This example is similar to
[the previous one](#writing-a-5.1-channel-wavefile-with-sine-tones), but we're
now creating a 7.1.4-channel wavefile instead.

First, we implement the `Write_7_1_4_Channel_Sine_Tone` procedure that
generates the PCM data for the 7.1.4-channel configuration and writes the
PCM samples to the output wavefile. Note that we're using the
`Channel_Position_7_1_4` range from the
`RIFF.Wav.Formats.Standard_Channel_Configurations` child package.

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
~~~~~~~~~~

The `Write_7_1_4_Channel_Sine_Wavefile` procedure contains the implementation
of the main application. Note that we now use the 7.1.4-channel configuration
(`Channel_Config_7_1_4`) for the channel configuration of the wavefile format
(`Wave_Format.Channel_Config`).

~~~~~~~~~~ada
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


\pagebreak

# Reading and Writing PCM data

## Appending a wavefile

We can use the library to append PCM data to an existing wavefile. In this
example, we're opening a wavefile (`WF_Append`) and appending it with the PCM
data from another file (`WF_In`). When we open an existing wavefile for
appending, its current position is automatically set to the end of the file,
so that we can directly start writing data to it.

The actual reading and writing process of this code example takes place in the
`Append_PCM_MC_Sample` block. It follows the same pattern as in previous
examples: we call `Get` to retrieve the PCM data from the input wavefile and
`Put` to append the PCM data to the output wavefile.

Note that in this example, in order to not overwrite existing reference files,
we're first copying an existing wavefile — using the `Copy_File` procedure from
the `Ada.Directories` package — into a new, temporary file. This new wavefile
is then appended with information from another (input) wavefile. We could of
course skip this step in a real application.

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


\pagebreak

## Copying a complete wavefile

This example shows how to create a copy of an existing wavefile by copy the PCM
data sample by sample. First, we need to make sure that the format of the input
wavefile is propagated to the output wavefile. We do this by retrieving the
format of the input wavefile (by calling `Format_Of_Wavefile` on the `WF_In`
object) and using it to set the format of the output wavefile (by calling
`Set_Format_Of_Wavefile` on the `WF_Out` object). The actual copy of the PCM
data is implemented in the `Copy_PCM_MC_Sample` block.

In this example, we're using a 64-bit floating-point PCM buffer to ensure that
the we have enough accuracy for all possible bit-depths that we might encounter
in the input wavefiles that we read.

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

         pragma Assert (Wav_Buffer_Range'First = 1);

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

Note that the `Copy_PCM_MC_Sample` block above makes use of the procedural
version of `Get` to retrieve the PCM data. We could have used the functional
version of `Get` instead, as we've done in some of the previous examples. This
is how the `Copy_PCM_MC_Sample` block would look like:

```ada
      Copy_PCM_MC_Sample : declare
         --  ...

         PCM_Buf : constant Wav_Buffer_Float_64 := Get (WF_In);
      begin
         Put (WF_Out, PCM_Buf);
         exit when WF_In.End_Of_File;
      end Copy_PCM_MC_Sample;
```


\pagebreak

## Copying a complete wavefile using fixed-point buffer

This example is very similar to the
[previous one](#copying-a-complete-wavefile). In this case, however, we use a
16-bit fixed-point PCM buffer instead of the 64-bit floating-point PCM buffer
that  we've used in that example. In this case, we declare the `PCM_IO` package
as an instance of the `Generic_Fixed_PCM_IO` package.

When we compare both source-code examples, we see that the basic structure of
the both implementations is still the same. This means that we don't need to
make substantial changes when converting an implementation to use
floating-point PCM buffers instead of fixed-point PCM buffers, and vice-versa.

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

         pragma Assert (Wav_Buffer_Range'First = 1);

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


\pagebreak

## Copying parts of a wavefile multiple times

This example shows how to generate an output wavefile by looping over a section
of an input wavefile. In other words, we'll copy parts of the input wavefile
to an output wavefile.

We start by implementing the auxilary `Display_Time_Info` procedure to display
the  current position — in terms of sample index and time — of an arbitrary
wavefile.

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
~~~~~~~~~~

The `Copy_Parts_Of_Wavefile` procedure performs the actual looping. Here, we're
setting arbitrary start and stop times (stored in the `Start_Time` and
`Stop_Time` constants), and a number of repetitions (stored in the `Repetitions`
constant). We use the `Set_Current_Time` procedure to set the current position
in the input wavefile at the begin of each iteration of the outer loop (the one
that uses the `Repetitions` constant). The inner loop — which contains the
`Copy_PCM_MC_Sample` block — does the copying of the PCM data. In order to
check whether we've reached the end of the inner loop, we retrieve the current
position in the wavefile with a call to the `Current_Time` function and compare
it to the `Stop_Time` constant.

~~~~~~~~~~ada
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

            pragma Assert (Wav_Buffer_Range'First = 1);

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


\pagebreak

## Converting a PCM wavefile to a 32-bit floating-point PCM wavefile

This example is very similar to a
[previous example](#copying-a-complete-wavefile) where we copy the complete
wavefile. In this case, however, we want to ensure that the data in the output
wavefile is stored in the 32-bit floating-point PCM format. This example can be
useful when converting wavefiles from linear PCM format to floating-point
format.

The major difference to the previous example is that, instead of just reusing
the format of the input wavefile in the call to the `Set_Format_Of_Wavefile`
procedure for the output wavefile, we use these constant values:

- for the `Bit_Depth` parameter: `Bit_Depth_32`, and

- for the `Use_Float` parameter: `True`.

Note that we're  using a 32-bit floating-point type in the declaration of the
`PCM_IO` package. We could, however, have used an arbitrary type here — for
example, a 16-bit fixed-point type. This implies that a conversion between the
PCM buffer type and the PCM format used in the wavefile needs to be performed.
Fortunately for us, the library makes sure that the correct conversion is
performed.


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

         pragma Assert (Wav_Buffer_Range'First = 1);

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


\pagebreak

# Downmixing Wavefiles

## Downmixing a stereo wavefile to a mono wavefile

This example shows how to downmix a stereo wavefile to a mono wavefile. In
principle, this example is similar to previous examples where we copy PCM data
from an input wavefile to an output wavefile. The main difference is that we're
hard-coding the number of channels by setting it to one
(`Number_Of_Channels => 1`) in the call to `Init` and, subsequently,
`Set_Format_Of_Wavefile`.  Also, we now need to distinguish between the PCM
buffer for the input wavefile (`PCM_Buf_In`) and the one for the output
wavefile (`PCM_Buf_Out`). We initialize the data of the output PCM buffer by
applying a gain value (0.5) to the data from the input PCM buffer.

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

         pragma Assert (Wav_Buffer_Range'First = 1);

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


\pagebreak

## Downmixing a 5.1-channel wavefile to a stereo wavefile

In this example, we take a 5.1-channel wavefile and downmix it to a stereo
wavefile. This example is similar to the
[previous one](#downmixing-a-stereo-wavefile-to-a-mono-wavefile). However,
because of the multichannel aspects that we need to deal with, we're now using
definitions from the `RIFF.Wav.Formats.Standard_Channel_Configurations` child
package.

We now have to declare two `PCM_IO` packages: one for the input wavefile, and
one for the output wavefile. This approach is particularly useful to ensure
that we're using the correct channel positions when addressing the input and
output PCM buffers. The main issue is that channel positions between two
configurations might not always match. For example, the front left channel
(`F_L`) of a 5.1-channel configuration has the same index as the (front) left
channel of a stereo configuration. However, the top front left channel
(`T_F_L`) of a 5.1.2-channel configuration doesn't have the same index as the
top front left channel of a 7.1.4-channel configuration. When we declare two
`PCM_IO` packages and the corresponding PCM buffers, strong typing ensures that
the channel positions are always using the correct index.

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


\pagebreak

## Downmixing a 7.1.4-channel wavefile to a 5.1-channel wavefile

This example is very similar to the
[previous one](#downmixing-a-5.1-channel-wavefile-to-a-stereo-wavefile). In
this case, however, we're downmixing a input wavefile with a 7.1.4-channel
configuration and generating a 5.1-channel wavefile. Please refer to that
example for more details.

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


\pagebreak

# Direct Copying Wavefiles

## Direct copying a complete wavefile without PCM buffer conversion

Most examples from this document make uses of a PCM buffer to interface with
the data from the wavefile. This is usually the recommend approach because we
want to use a specific format in our application and be able to interface with
wavefiles that may contain PCM data in arbitrary formats. For example, our
application may use the 32-bit floating-point format for the PCM buffer, but
the wavefiles may contain data in 16-bit fixed-point or 64-bit floating-point
formats. The most straightforward solution is to first copy the data from the
wavefile into an intermediate buffer — which makes use of the same format as in
the wavefile — and then convert the data to the format used for the PCM buffer.

Sometimes, however, we may be able to avoid this conversion. This might be the
case when we're writing an application that doesn't need to cope with arbitrary
wavefile formats. For example, the implementation below always reads and writes
16-bit fixed-point wavefiles. In this case, no conversion is needed, so we can
avoid the intermediate buffer. Therefore, we don't need to declare a `PCM_IO`
package instance. Instead, we can declare a `Wav_IO` package instance.

Note that, when we use a `Wav_IO` package, we can only read or write data for
the specific format that we've used in the declaration of the `Wav_IO` package.
For example, if the `Wav_IO` package has been instantiated for the 16-bit
fixed-point format, it won't be able to read a wavefile containing 32-bit
floating-point PCM data. Therefore, in general, we should use `PCM_IO` packages
and only go for `Wav_IO` packages when it's really appropriate to do so.

~~~~~~~~~~ada
with Audio.Wavefiles;            use Audio.Wavefiles;
with Audio.Wavefiles.Data_Types; use Audio.Wavefiles.Data_Types;
with Audio.RIFF.Wav.Formats;     use Audio.RIFF.Wav.Formats;

with Audio.Wavefiles.Generic_Direct_Fixed_Wav_IO;

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

         package Wav_IO is new Audio.Wavefiles.Generic_Direct_Fixed_Wav_IO
           (Wav_Sample    => Wav_Fixed_16,
            Channel_Range => Wav_Buffer_Range,
            Wav_MC_Sample => Wav_Buffer_Fixed_16);
         use Wav_IO;

         pragma Assert (Wav_Buffer_Range'First = 1);

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


\pagebreak

## Direct copying a complete floating-point wavefile without PCM buffer conversion

This example is similar to the
[previous one](#direct-copying-a-complete-wavefile-without-pcm-buffer-conversion),
but for floating-point wavefiles. In this example, we're copying a 32-bit
floating-point wavefile and instantiating the `Generic_Direct_Float_Wav_IO`
package. Please refer to the previous example for more details.

~~~~~~~~~~ada
with Audio.Wavefiles;            use Audio.Wavefiles;
with Audio.Wavefiles.Data_Types; use Audio.Wavefiles.Data_Types;
with Audio.RIFF.Wav.Formats;     use Audio.RIFF.Wav.Formats;

with Audio.Wavefiles.Generic_Direct_Float_Wav_IO;

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

         package Wav_IO is new Audio.Wavefiles.Generic_Direct_Float_Wav_IO
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


\pagebreak

## Converting a 8-bit wavefile to a 16-bit wavefile

While most wavefile formats — such as 16-bit or 32-bit PCM wavefiles — use
signed values for the PCM data, 8-bit wavefiles are an odd exception to the
rule: they use unsigned values for the PCM data. Therefore, in order to read
and process data from a 8-bit wavefile, we need to first adapt the PCM data to
the *standard* range.

In this example, we use the direct `Wav_IO` packages to read data from a 8-bit
wavefile. Then, we adjust the range to match the signed range, and write the
result to a 16-bit wavefile. This process is implemented in the
`Convert_Wav_MC_Sample` block of the `Convert_8_Bit_To_16_Bit_Wavefile`
procedure. Here, we use an offset (the `Offset` constant) to adapt the values
from the input buffer (`Wav_Buf_In`) and store them in the output buffer
(`Wav_Buf_Out`).

~~~~~~~~~~ada
with Ada.Text_IO;                use Ada.Text_IO;

with Audio.Wavefiles;            use Audio.Wavefiles;
with Audio.Wavefiles.Data_Types; use Audio.Wavefiles.Data_Types;
with Audio.RIFF.Wav.Formats;     use Audio.RIFF.Wav.Formats;

with Audio.Wavefiles.Generic_Direct_Fixed_Wav_IO;

procedure Convert_8_Bit_To_16_Bit_Wavefile is

   package Wav_IO_8 is new Audio.Wavefiles.Generic_Direct_Fixed_Wav_IO
     (Wav_Sample    => Wav_Unsigned_Fixed_8,
      Channel_Range => Wav_Buffer_Range,
      Wav_MC_Sample => Wav_Buffer_Unsigned_Fixed_8);
   use Wav_IO_8;

   package Wav_IO_16 is new Audio.Wavefiles.Generic_Direct_Fixed_Wav_IO
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


\pagebreak

# Reading Wavefiles to Memory

## Reading a complete wavefile into memory (channel-interleaved data)

In previous examples, we were declaring small PCM buffers to store just a
single (multichannel) PCM value in the memory. In some cases, however, it can
be quite handy to store a complete wavefile in the memory. The `Read_To_Memory`
block of this example contains an implementation that does this.

In the `Read_To_Memory` block, we declare a PCM buffer (`PCM_Data`) that is big
enough to store all values of the input wavefile. Also, we use a
channel-interleaved approach, so that each element of `PCM_Data` has an array
containing the PCM  samples for each audio channel. We implement this with two
types:

- `Bounded_Wav_Buffer_Float_32`, which is a subtype of the
  `Wav_Buffer_Float_32` array type restricted to the number of channels of the
  input wavefile.

- `PCM_Container`, which is an array type with elements of the
  `Bounded_Wav_Buffer_Float_32` subtype mentioned. We use the
  `Long_Long_Integer` type for the range to ensure that it's sufficient for
  longer wavefiles to be processed.

We then declare a `PCM_Data` array of `PCM_Container` type ranging from the
first to the last sample of the input wavefile. We then simply read the samples
in a loop using the `Get` function.

Note that in this example, we're allocating the PCM buffer on the stack.
Because our input wavefile is known to be small, this will work fine. However,
for very long wavefiles, this approach could be problematic. In that case, we
should allocate the PCM buffer on the heap instead.

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

      subtype WF_In_Channels is Wav_Buffer_Range range
        Wav_Buffer_Range'First .. WF_In.Number_Of_Channels
                                  + Wav_Buffer_Range'First - 1;

      subtype Bounded_Wav_Buffer_Float_32 is
        Wav_Buffer_Float_32 (WF_In_Channels);

      package PCM_IO is new Audio.Wavefiles.Generic_Float_PCM_IO
        (PCM_Sample    => Wav_Float_32,
         Channel_Range => Wav_Buffer_Range,
         PCM_MC_Sample => Wav_Buffer_Float_32);
      use PCM_IO;

      type PCM_Container is array (Long_Long_Integer range <>) of
        Bounded_Wav_Buffer_Float_32;

      PCM_Data : PCM_Container (WF_In.First_Sample .. WF_In.Last_Sample);

   begin
      for Sample_Count in PCM_Data'Range loop
         pragma Assert (not WF_In.End_Of_File);
         PCM_Data (Sample_Count) := Get (WF_In);
      end loop;

      --  At this point, we have all PCM data from the wavefile stored in
      --  PCM_Data. Let's display a couple of samples:

      Display_Some_Samples : declare

         procedure Display_Sample (MC_Sample    : Bounded_Wav_Buffer_Float_32;
                                   Sample_Count : Long_Long_Integer);

         procedure Display_Sample (MC_Sample    : Bounded_Wav_Buffer_Float_32;
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

         Max_Samples_To_Display : constant := 10;
         Last_Sample_To_Display : constant Long_Long_Integer
           := Long_Long_Integer'Min (Max_Samples_To_Display
                                       + PCM_Data'First - 1,
                                     PCM_Data'Last);
      begin
         for Sample_Count in PCM_Data'First .. Last_Sample_To_Display loop
            Display_Sample (PCM_Data (Sample_Count), Sample_Count);
         end loop;
      end Display_Some_Samples;

   end Read_To_Memory;

   WF_In.Close;
end Read_To_Memory_Channel_Interleaved;
~~~~~~~~~~


\pagebreak

## Reading a complete wavefile into memory (data per channel)

This example is a variation of the
[previous example](#reading-a-complete-wavefile-into-memory-channel-interleaved-data)
where we were reading a complete wavefile into memory and storing the PCM
samples in a channel-interleaved way. In this case, however, instead of using
channel-interleaving, we have a big PCM buffer for each channel of the input
wavefile. We implement this with the following types:

- `Channel_PCM_Data`, which is an array type of PCM samples (where each PCM
  sample has the `Wav_Float_32` type).

- `Bounded_Channel_PCM_Data`, which is a subtype of the
  `Channel_PCM_Data` array type restricted to the number of samples of the
  input wavefile.

- `PCM_Container`, which is an array type with elements of the
  `Bounded_Channel_PCM_Data` subtype mentioned.

We then declare a `PCM_Data` array of `PCM_Container` type ranging from the
first to the last channel of the input wavefile.

Similar to the previous example, the core of this application consists of
reading the samples in a loop using the `Get` function. However, because the
subprograms of the `PCM_IO` package can only handle channel-interleaved PCM
data, we also need an additional small buffer (`Wav_Buf`) to store each
multichannel PCM sample returned by the `Get` function before *de-interleaving*
it into the `PCM_Data` buffer. This is implemented in the `Read_Sample` block.

~~~~~~~~~~ada
with Ada.Text_IO;

with Audio.Wavefiles;                      use Audio.Wavefiles;
with Audio.Wavefiles.Data_Types;           use Audio.Wavefiles.Data_Types;
with Audio.Wavefiles.Generic_Float_PCM_IO;

procedure Read_To_Memory_Per_Channel is

   type Channel_PCM_Data is array (Long_Long_Integer range <>) of
     Wav_Float_32;

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

      subtype WF_In_Channels is Wav_Buffer_Range range
        Wav_Buffer_Range'First .. WF_In.Number_Of_Channels
                                  + Wav_Buffer_Range'First - 1;

      PCM_Data : PCM_Container (WF_In_Channels);

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

      Display_Some_Samples : declare

         procedure Display_Samples
           (Samples                : Channel_PCM_Data;
            Channel_Index          : Positive;
            Last_Sample_To_Display : Long_Long_Integer);

         procedure Display_Samples
           (Samples                : Channel_PCM_Data;
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
               Put_Line ("    Sample # "
                         & Long_Long_Integer'Image (Sample_Count)
                         & ": "
                         & Wav_Float_32'Image (Samples (Sample_Count)));
            end loop;
         end Display_Samples;

         Max_Samples_To_Display : constant := 10;
         Last_Sample_To_Display : constant Long_Long_Integer
           := Long_Long_Integer'Min (Max_Samples_To_Display
                                       + Bounded_Channel_PCM_Data'First - 1,
                                     Bounded_Channel_PCM_Data'Last);
      begin
         for Channel_Count in PCM_Data'Range loop
            Display_Samples
              (Samples                => PCM_Data (Channel_Count),
               Channel_Index          => Channel_Count,
               Last_Sample_To_Display => Last_Sample_To_Display);
         end loop;
      end Display_Some_Samples;

   end Read_To_Memory;

   WF_In.Close;
end Read_To_Memory_Per_Channel;
~~~~~~~~~~


\pagebreak

# Display Wavefile Information

## Displaying the channel configuration of a wavefile

In this example, we use the `Guessed_Channel_Configuration` function to guess
the channel configuration of a wavefile based on the number of channels.
Although this function delivers the correct result in many cases, it might be
inappropriate for some corner cases. This is due to the fact that multiple
channel configurations may have the same number of channels. For example, both
5.1-channel and 6.0-channel configurations have the same number of channels:
6 channels. In this particular case, `Guessed_Channel_Configuration` selects
the 5.1-channel configuration because it is more commonly used than the
6.0-channel configuration.

We start with the implementation of an auxiliary procedure that displays the
channel configuration name (`Display_Channel_Config_Name`).

~~~~~~~~~~ada
with Audio.RIFF.Wav.Formats; use Audio.RIFF.Wav.Formats;

procedure Display_Channel_Config_Name (Channel_Config : Channel_Configuration);

with Ada.Text_IO;            use Ada.Text_IO;

with Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;
use  Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;

procedure Display_Channel_Config_Name
  (Channel_Config : Channel_Configuration) is
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
end Display_Channel_Config_Name;
~~~~~~~~~~

In the `Display_Channel_Config` procedure, we open a wavefile and call the
`Guessed_Channel_Configuration` function with the number of channels of the
wavefile. The function returns a channel configuration (`Channel_Config`),
which we display with the call to `Display_Channel_Config_Name`.

~~~~~~~~~~ada
with Audio.Wavefiles;        use Audio.Wavefiles;
with Audio.RIFF.Wav.Formats; use Audio.RIFF.Wav.Formats;

with Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;
use  Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;

with Display_Channel_Config_Name;

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
         Display_Channel_Config_Name (Channel_Config);
      end;
   end if;

   WF.Close;
end Display_Channel_Config;
~~~~~~~~~~


\pagebreak

# RIFF Chunks

## Displaying the RIFF chunks of a wavefile

We can use the `Wavefiles` package to identify the RIFF chunks of a wavefile
and extract data from them. In this example, we call the `Get_RIFF_Info`
procedure to get the RIFF information and store it in the `RIFF_Info` object.
We then call `Display_Info` procedure to display detailed information about
each RIFF chunk found in the wavefile.

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


\pagebreak

## Extracting the iXML chunk from a wavefile

In this example, we extract the iXML chunk of an input wavefile — if it can be
found in the wavefile, of course.

First, let's start by implementing the auxiliary procedure `Write_Data_As_Text`
that receives an arbitrary byte array and writes it as text to an output file.
Note that we use an overlay to interpret the bytes as characters.

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
~~~~~~~~~~

Then, we implement the `Extract_XML_Chunk` procedure, which extracts the XML
chunk from the wavefile and calls `Write_Data_As_Text` to write the XML data
to the output file.

We start by opening the input wavefile and creating an output XML file. We then
call the `Get_RIFF_Info` procedure to retrieve the RIFF information from the
wavefile, which we store in the `RIFF_Info` object. We then call the
`Find_First_Chunk` procedure and pass as arguments:

- the chunks from the RIFF information object (`RIFF_Info.Chunks`) and

- the chunk type we're looking for (`Wav_Chunk_IXML` in this case).

The `Find_First_Chunk` procedure has an output parameter called `Found`. The
`Success` component of `Found` indicates whether a chunk has been found in the
wavefile. If it was, then basic information about the chunk is available in the
`Chunk_Element` component.

We can then retrieve the actual chunk data from the wavefile by calling the
`Chunk_Element_Data` function and passing the `Chunk_Element` object. This
function returns the complete chunk data as a byte array. This is the byte
array that we pass to the `Write_Data_As_Text` procedure to write the output
XML file.

Note that we can use the method described above to extract any chunk from a
wavefile, including the PCM data chunk. In all cases, however, we'll only get
raw data (as bytes), which we can then interpret or process separately.

~~~~~~~~~~ada
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
         Found : Wav_Chunk_Element_Found;
      begin
         Find_First_Chunk (Chunks    => RIFF_Info.Chunks,
                           Chunk_Tag => Wav_Chunk_IXML,
                           Found     => Found);

         if Found.Success then
            Write_Data_As_Text (Xml_File,
                                Chunk_Element_Data (WF, Found.Chunk_Element));
         end if;
      end;
   end if;

   WF.Close;
   Close (Xml_File);
end Extract_XML_Chunk;
~~~~~~~~~~
