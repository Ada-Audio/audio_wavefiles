package Audio.Wavefiles.Data_Types is

   D_8      : constant := 1.0 / 2.0 ** (8  - 1);
   D_16     : constant := 1.0 / 2.0 ** (16 - 1);
   D_24     : constant := 1.0 / 2.0 ** (24 - 1);
   D_32     : constant := 1.0 / 2.0 ** (32 - 1);
   D_64     : constant := 1.0 / 2.0 ** (64 - 1);

   type Wav_Fixed_8 is delta D_8 range -1.0 .. 1.0 - D_8
     with Size => 8,  Stream_Size => 8;
   type Wav_Fixed_16 is delta D_16 range -1.0 .. 1.0 - D_16
     with Size => 16, Stream_Size => 16;
   type Wav_Fixed_24 is delta D_24 range -1.0 .. 1.0 - D_24
     with Size => 24, Stream_Size => 24;
   type Wav_Fixed_32 is delta D_32 range -1.0 .. 1.0 - D_32
     with Size => 32, Stream_Size => 32;
   type Wav_Fixed_64 is delta D_64 range -1.0 .. 1.0 - D_64
     with Size => 64, Stream_Size => 64;

   type Wav_Float_32 is digits 6
     with Size => 32, Stream_Size => 32;
   type Wav_Float_64 is digits 15
     with Size => 64, Stream_Size => 64;
   type Wav_Float_128 is digits 18
     with Size => 128;

   type Wav_Buffer_Fixed_8   is array (Positive range <>) of Wav_Fixed_8;
   type Wav_Buffer_Fixed_16  is array (Positive range <>) of Wav_Fixed_16;
   type Wav_Buffer_Fixed_24  is array (Positive range <>) of Wav_Fixed_24;
   type Wav_Buffer_Fixed_32  is array (Positive range <>) of Wav_Fixed_32;
   type Wav_Buffer_Fixed_64  is array (Positive range <>) of Wav_Fixed_64;

   type Wav_Buffer_Float_32  is array (Positive range <>) of Wav_Float_32;
   type Wav_Buffer_Float_64  is array (Positive range <>) of Wav_Float_64;
   type Wav_Buffer_Float_128 is array (Positive range <>) of Wav_Float_128;

end Audio.Wavefiles.Data_Types;
