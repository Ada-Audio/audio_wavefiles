package Audio.RIFF.Wav.Formats.Standard_Channel_Configurations is

   Channel_Config_Empty : constant Channel_Configuration :=
     (others                                          => False);

   Channel_Config_1_0 : constant Channel_Configuration :=
     (Speaker_Front_Left                              => True,
      others                                          => False);

   Channel_Config_2_0 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right    => True,
      others                                          => False);

   Channel_Config_3_0 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right     |
      Speaker_Front_Center                            => True,
      others                                          => False);

   Channel_Config_4_0 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right     |
      Speaker_Back_Left      | Speaker_Back_Right     => True,
      others                                          => False);

   Channel_Config_5_0 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right     |
      Speaker_Front_Center   |
      Speaker_Back_Left      | Speaker_Back_Right     => True,
      others                                          => False);

   Channel_Config_5_1 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right     |
      Speaker_Front_Center   | Speaker_Low_Frequency   |
      Speaker_Back_Left      | Speaker_Back_Right     => True,
      others                                          => False);

   Channel_Config_7_0 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right     |
      Speaker_Front_Center   |
      Speaker_Back_Left      | Speaker_Back_Right      |
      Speaker_Side_Left      | Speaker_Side_Right     => True,
      others                                          => False);

   Channel_Config_7_1 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right     |
      Speaker_Front_Center   | Speaker_Low_Frequency   |
      Speaker_Back_Left      | Speaker_Back_Right      |
      Speaker_Side_Left      | Speaker_Side_Right     => True,
      others                                          => False);

   Channel_Config_7_1_BC : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right     |
      Speaker_Front_Center   | Speaker_Low_Frequency   |
      Speaker_Back_Left      | Speaker_Back_Right      |
      Speaker_Side_Left      | Speaker_Side_Right      |
      Speaker_Back_Center                             => True,
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

   Channel_Config_7_0_4 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right      |
      Speaker_Front_Center   |
      Speaker_Back_Left      | Speaker_Back_Right       |
      Speaker_Side_Left      | Speaker_Side_Right       |
      Speaker_Top_Front_Left | Speaker_Top_Front_Right  |
      Speaker_Top_Back_Left  | Speaker_Top_Back_Right  => True,
      others                                           => False);

   Channel_Config_7_1_4 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right      |
      Speaker_Front_Center   | Speaker_Low_Frequency    |
      Speaker_Back_Left      | Speaker_Back_Right       |
      Speaker_Side_Left      | Speaker_Side_Right       |
      Speaker_Top_Front_Left | Speaker_Top_Front_Right  |
      Speaker_Top_Back_Left  | Speaker_Top_Back_Right  => True,
      others                                           => False);

   function Guess_Channel_Configuration
     (Number_Of_Channels : Positive) return Channel_Configuration
     with Post =>
       (if Guess_Channel_Configuration'Result /= Channel_Config_Empty then
          Is_Consistent
            (Guess_Channel_Configuration'Result,
             Number_Of_Channels));

end Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;
