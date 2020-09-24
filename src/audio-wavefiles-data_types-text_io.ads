with Ada.Text_IO;

package Audio.Wavefiles.Data_Types.Text_IO is

   package Wav_Fixed_8_Text_IO is new
     Ada.Text_IO.Fixed_IO (Wav_Fixed_8);

   package Wav_Fixed_16_Text_IO is new
     Ada.Text_IO.Fixed_IO (Wav_Fixed_16);

   package Wav_Fixed_24_Text_IO is new
     Ada.Text_IO.Fixed_IO (Wav_Fixed_24);

   package Wav_Fixed_32_Text_IO is new
     Ada.Text_IO.Fixed_IO (Wav_Fixed_32);

   package Wav_Fixed_64_Text_IO is new
     Ada.Text_IO.Fixed_IO (Wav_Fixed_64);

   package Wav_Float_32_Text_IO is new
     Ada.Text_IO.Float_IO (Wav_Float_32);

   package Wav_Float_64_Text_IO is new
     Ada.Text_IO.Float_IO (Wav_Float_64);

   package Wav_Float_128_Text_IO is new
     Ada.Text_IO.Float_IO (Wav_Float_128);

end Audio.Wavefiles.Data_Types.Text_IO;
