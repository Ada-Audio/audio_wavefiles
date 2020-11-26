package Audio.RIFF.Wav.Formats.Standard_Channel_Configurations is

   Channel_Config_2_0 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right    => True,
      others                                          => False);

   Channel_Config_5_1 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right     |
      Speaker_Front_Center   | Speaker_Low_Frequency   |
      Speaker_Back_Left      | Speaker_Back_Right     => True,
      others                                          => False);

   Channel_Config_7_1 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right     |
      Speaker_Front_Center   | Speaker_Low_Frequency   |
      Speaker_Back_Left      | Speaker_Back_Right      |
      Speaker_Side_Left      | Speaker_Side_Right     => True,
      others                                          => False);

   Channel_Config_5_1_2 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right      |
      Speaker_Front_Center   | Speaker_Low_Frequency    |
      Speaker_Back_Left      | Speaker_Back_Right       |
      Speaker_Top_Front_Left | Speaker_Top_Front_Right => True,
      others                                           => False);

   Channel_Config_5_1_4 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right      |
      Speaker_Front_Center   | Speaker_Low_Frequency    |
      Speaker_Back_Left      | Speaker_Back_Right       |
      Speaker_Top_Front_Left | Speaker_Top_Front_Right  |
      Speaker_Top_Back_Left  | Speaker_Top_Back_Right  => True,
      others                                           => False);

   Channel_Config_7_1_2 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right      |
      Speaker_Front_Center   | Speaker_Low_Frequency    |
      Speaker_Back_Left      | Speaker_Back_Right       |
      Speaker_Side_Left      | Speaker_Side_Right       |
      Speaker_Top_Front_Left | Speaker_Top_Front_Right => True,
      others                                           => False);

   Channel_Config_7_1_4 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right      |
      Speaker_Front_Center   | Speaker_Low_Frequency    |
      Speaker_Back_Left      | Speaker_Back_Right       |
      Speaker_Side_Left      | Speaker_Side_Right       |
      Speaker_Top_Front_Left | Speaker_Top_Front_Right  |
      Speaker_Top_Back_Left  | Speaker_Top_Back_Right  => True,
      others                                           => False);

end Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;
