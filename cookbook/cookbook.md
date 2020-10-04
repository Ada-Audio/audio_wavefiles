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
