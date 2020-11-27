package body Audio.RIFF.Wav.Formats.Standard_Channel_Configurations is

   function Guessed_Channel_Configuration
     (Number_Of_Channels : Positive) return Channel_Configuration is
   begin
      case Number_Of_Channels is
         when  1 => return Channel_Config_1_0;
         when  2 => return Channel_Config_2_0;
         when  3 => return Channel_Config_3_0;
         when  4 => return Channel_Config_4_0;
         when  5 => return Channel_Config_5_0;
         when  6 => return Channel_Config_5_1;
         when  7 => return Channel_Config_7_0;
         when  8 => return Channel_Config_7_1;
         when  9 => return Channel_Config_7_1_BC;
         when 10 => return Channel_Config_5_1_4;
            --      return Channel_Config_7_1_2;
         when 11 => return Channel_Config_7_0_4;
         when 12 => return Channel_Config_7_1_4;
         when others => return Channel_Config_Empty;
      end case;
   end Guessed_Channel_Configuration;

end Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;
