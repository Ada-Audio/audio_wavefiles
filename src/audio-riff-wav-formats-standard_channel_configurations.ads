------------------------------------------------------------------------------
--                                                                          --
--                         AUDIO / RIFF / WAV                               --
--                                                                          --
--              Standard channel configurations for wavefiles               --
--                                                                          --
--  The MIT License (MIT)                                                   --
--                                                                          --
--  Copyright (c) 2020 Gustavo A. Hoffmann                                  --
--                                                                          --
--  Permission is hereby granted, free of charge, to any person obtaining   --
--  a copy of this software and associated documentation files (the         --
--  "Software"), to deal in the Software without restriction, including     --
--  without limitation the rights to use, copy, modify, merge, publish,     --
--  distribute, sublicense, and / or sell copies of the Software, and to    --
--  permit persons to whom the Software is furnished to do so, subject to   --
--  the following conditions:                                               --
--                                                                          --
--  The above copyright notice and this permission notice shall be          --
--  included in all copies or substantial portions of the Software.         --
--                                                                          --
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         --
--  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      --
--  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  --
--  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY    --
--  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,    --
--  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE       --
--  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                  --
------------------------------------------------------------------------------

package Audio.RIFF.Wav.Formats.Standard_Channel_Configurations is

   Channel_Config_Empty : constant Channel_Configuration :=
     (others                                          => False);

   Channel_Config_1_0 : constant Channel_Configuration :=
     (Speaker_Front_Left                              => True,
      others                                          => False);

   type Channel_Position_1_0 is
     (Front_Left);

   for Channel_Position_1_0 use
     (Front_Left        =>  1);

   function F_L return Channel_Position_1_0 renames Front_Left;

   Channel_Config_2_0 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right    => True,
      others                                          => False);

   type Channel_Position_2_0 is
     (Front_Left,              Front_Right);

   function F_L return Channel_Position_2_0 renames Front_Left;
   function F_R return Channel_Position_2_0 renames Front_Right;

   Channel_Config_3_0 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right     |
      Speaker_Front_Center                            => True,
      others                                          => False);

   type Channel_Position_3_0 is
     (Front_Left,              Front_Right,
      Front_Center);

   function F_L return Channel_Position_3_0 renames Front_Left;
   function F_R return Channel_Position_3_0 renames Front_Right;
   function F_C return Channel_Position_3_0 renames Front_Center;

   Channel_Config_4_0 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right     |
      Speaker_Back_Left      | Speaker_Back_Right     => True,
      others                                          => False);

   type Channel_Position_4_0 is
     (Front_Left,              Front_Right,
      Back_Left,               Back_Right);

   function F_L return Channel_Position_4_0 renames Front_Left;
   function F_R return Channel_Position_4_0 renames Front_Right;
   function B_L return Channel_Position_4_0 renames Back_Left;
   function B_R return Channel_Position_4_0 renames Back_Right;

   Channel_Config_5_0 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right     |
      Speaker_Front_Center   |
      Speaker_Back_Left      | Speaker_Back_Right     => True,
      others                                          => False);

   type Channel_Position_5_0 is
     (Front_Left,              Front_Right,
      Front_Center,
      Back_Left,               Back_Right);

   function F_L return Channel_Position_5_0 renames Front_Left;
   function F_R return Channel_Position_5_0 renames Front_Right;
   function F_C return Channel_Position_5_0 renames Front_Center;
   function B_L return Channel_Position_5_0 renames Back_Left;
   function B_R return Channel_Position_5_0 renames Back_Right;

   Channel_Config_5_1 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right     |
      Speaker_Front_Center   | Speaker_Low_Frequency   |
      Speaker_Back_Left      | Speaker_Back_Right     => True,
      others                                          => False);

   type Channel_Position_5_1 is
     (Front_Left,              Front_Right,
      Front_Center,            Low_Frequency,
      Back_Left,               Back_Right);

   function F_L return Channel_Position_5_1 renames Front_Left;
   function F_R return Channel_Position_5_1 renames Front_Right;
   function F_C return Channel_Position_5_1 renames Front_Center;
   function LFE return Channel_Position_5_1 renames Low_Frequency;
   function B_L return Channel_Position_5_1 renames Back_Left;
   function B_R return Channel_Position_5_1 renames Back_Right;

   Channel_Config_7_0 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right     |
      Speaker_Front_Center   |
      Speaker_Back_Left      | Speaker_Back_Right      |
      Speaker_Side_Left      | Speaker_Side_Right     => True,
      others                                          => False);

   type Channel_Position_7_0 is
     (Front_Left,              Front_Right,
      Front_Center,
      Back_Left,               Back_Right,
      Side_Left,               Side_Right);

   function F_L return Channel_Position_7_0 renames Front_Left;
   function F_R return Channel_Position_7_0 renames Front_Right;
   function F_C return Channel_Position_7_0 renames Front_Center;
   function B_L return Channel_Position_7_0 renames Back_Left;
   function B_R return Channel_Position_7_0 renames Back_Right;
   function S_L return Channel_Position_7_0 renames Side_Left;
   function S_R return Channel_Position_7_0 renames Side_Right;

   Channel_Config_7_1 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right     |
      Speaker_Front_Center   | Speaker_Low_Frequency   |
      Speaker_Back_Left      | Speaker_Back_Right      |
      Speaker_Side_Left      | Speaker_Side_Right     => True,
      others                                          => False);

   type Channel_Position_7_1 is
     (Front_Left,              Front_Right,
      Front_Center,            Low_Frequency,
      Back_Left,               Back_Right,
      Side_Left,               Side_Right);

   function F_L return Channel_Position_7_1 renames Front_Left;
   function F_R return Channel_Position_7_1 renames Front_Right;
   function F_C return Channel_Position_7_1 renames Front_Center;
   function LFE return Channel_Position_7_1 renames Low_Frequency;
   function B_L return Channel_Position_7_1 renames Back_Left;
   function B_R return Channel_Position_7_1 renames Back_Right;
   function S_L return Channel_Position_7_1 renames Side_Left;
   function S_R return Channel_Position_7_1 renames Side_Right;

   Channel_Config_7_1_BC : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right     |
      Speaker_Front_Center   | Speaker_Low_Frequency   |
      Speaker_Back_Left      | Speaker_Back_Right      |
      Speaker_Side_Left      | Speaker_Side_Right      |
      Speaker_Back_Center                             => True,
      others                                          => False);

   type Channel_Position_7_1_BC is
     (Front_Left,              Front_Right,
      Front_Center,            Low_Frequency,
      Back_Left,               Back_Right,
      Side_Left,               Side_Right,
      Back_Center);

   function F_L return Channel_Position_7_1_BC renames Front_Left;
   function F_R return Channel_Position_7_1_BC renames Front_Right;
   function F_C return Channel_Position_7_1_BC renames Front_Center;
   function LFE return Channel_Position_7_1_BC renames Low_Frequency;
   function B_L return Channel_Position_7_1_BC renames Back_Left;
   function B_R return Channel_Position_7_1_BC renames Back_Right;
   function S_L return Channel_Position_7_1_BC renames Side_Left;
   function S_R return Channel_Position_7_1_BC renames Side_Right;
   function B_C return Channel_Position_7_1_BC renames Back_Center;

   Channel_Config_5_1_2 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right      |
      Speaker_Front_Center   | Speaker_Low_Frequency    |
      Speaker_Back_Left      | Speaker_Back_Right       |
      Speaker_Top_Front_Left | Speaker_Top_Front_Right => True,
      others                                           => False);

   type Channel_Position_5_1_2 is
     (Front_Left,              Front_Right,
      Front_Center,            Low_Frequency,
      Back_Left,               Back_Right,
      Top_Front_Left,          Top_Front_Right);

   function F_L   return Channel_Position_5_1_2 renames Front_Left;
   function F_R   return Channel_Position_5_1_2 renames Front_Right;
   function F_C   return Channel_Position_5_1_2 renames Front_Center;
   function LFE   return Channel_Position_5_1_2 renames Low_Frequency;
   function B_L   return Channel_Position_5_1_2 renames Back_Left;
   function B_R   return Channel_Position_5_1_2 renames Back_Right;
   function T_F_L return Channel_Position_5_1_2 renames Top_Front_Left;
   function T_F_R return Channel_Position_5_1_2 renames Top_Front_Right;

   Channel_Config_5_1_4 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right      |
      Speaker_Front_Center   | Speaker_Low_Frequency    |
      Speaker_Back_Left      | Speaker_Back_Right       |
      Speaker_Top_Front_Left | Speaker_Top_Front_Right  |
      Speaker_Top_Back_Left  | Speaker_Top_Back_Right  => True,
      others                                           => False);

   type Channel_Position_5_1_4 is
     (Front_Left,              Front_Right,
      Front_Center,            Low_Frequency,
      Back_Left,               Back_Right,
      Top_Front_Left,          Top_Front_Right,
      Top_Back_Left,           Top_Back_Right);

   function F_L   return Channel_Position_5_1_4 renames Front_Left;
   function F_R   return Channel_Position_5_1_4 renames Front_Right;
   function F_C   return Channel_Position_5_1_4 renames Front_Center;
   function LFE   return Channel_Position_5_1_4 renames Low_Frequency;
   function B_L   return Channel_Position_5_1_4 renames Back_Left;
   function B_R   return Channel_Position_5_1_4 renames Back_Right;
   function T_F_L return Channel_Position_5_1_4 renames Top_Front_Left;
   function T_F_R return Channel_Position_5_1_4 renames Top_Front_Right;
   function T_B_L return Channel_Position_5_1_4 renames Top_Back_Left;
   function T_B_R return Channel_Position_5_1_4 renames Top_Back_Right;

   Channel_Config_7_1_2 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right      |
      Speaker_Front_Center   | Speaker_Low_Frequency    |
      Speaker_Back_Left      | Speaker_Back_Right       |
      Speaker_Side_Left      | Speaker_Side_Right       |
      Speaker_Top_Front_Left | Speaker_Top_Front_Right => True,
      others                                           => False);

   type Channel_Position_7_1_2 is
     (Front_Left,              Front_Right,
      Front_Center,            Low_Frequency,
      Back_Left,               Back_Right,
      Side_Left,               Side_Right,
      Top_Front_Left,          Top_Front_Right);

   function F_L   return Channel_Position_7_1_2 renames Front_Left;
   function F_R   return Channel_Position_7_1_2 renames Front_Right;
   function F_C   return Channel_Position_7_1_2 renames Front_Center;
   function LFE   return Channel_Position_7_1_2 renames Low_Frequency;
   function B_L   return Channel_Position_7_1_2 renames Back_Left;
   function B_R   return Channel_Position_7_1_2 renames Back_Right;
   function S_L   return Channel_Position_7_1_2 renames Side_Left;
   function S_R   return Channel_Position_7_1_2 renames Side_Right;
   function T_F_L return Channel_Position_7_1_2 renames Top_Front_Left;
   function T_F_R return Channel_Position_7_1_2 renames Top_Front_Right;

   Channel_Config_7_0_4 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right      |
      Speaker_Front_Center   |
      Speaker_Back_Left      | Speaker_Back_Right       |
      Speaker_Side_Left      | Speaker_Side_Right       |
      Speaker_Top_Front_Left | Speaker_Top_Front_Right  |
      Speaker_Top_Back_Left  | Speaker_Top_Back_Right  => True,
      others                                           => False);

   type Channel_Position_7_0_4 is
     (Front_Left,              Front_Right,
      Front_Center,
      Back_Left,               Back_Right,
      Side_Left,               Side_Right,
      Top_Front_Left,          Top_Front_Right,
      Top_Back_Left,           Top_Back_Right);

   function F_L   return Channel_Position_7_0_4 renames Front_Left;
   function F_R   return Channel_Position_7_0_4 renames Front_Right;
   function F_C   return Channel_Position_7_0_4 renames Front_Center;
   function B_L   return Channel_Position_7_0_4 renames Back_Left;
   function B_R   return Channel_Position_7_0_4 renames Back_Right;
   function S_L   return Channel_Position_7_0_4 renames Side_Left;
   function S_R   return Channel_Position_7_0_4 renames Side_Right;
   function T_F_L return Channel_Position_7_0_4 renames Top_Front_Left;
   function T_F_R return Channel_Position_7_0_4 renames Top_Front_Right;
   function T_B_L return Channel_Position_7_0_4 renames Top_Back_Left;
   function T_B_R return Channel_Position_7_0_4 renames Top_Back_Right;

   Channel_Config_7_1_4 : constant Channel_Configuration :=
     (Speaker_Front_Left     | Speaker_Front_Right      |
      Speaker_Front_Center   | Speaker_Low_Frequency    |
      Speaker_Back_Left      | Speaker_Back_Right       |
      Speaker_Side_Left      | Speaker_Side_Right       |
      Speaker_Top_Front_Left | Speaker_Top_Front_Right  |
      Speaker_Top_Back_Left  | Speaker_Top_Back_Right  => True,
      others                                           => False);

   type Channel_Position_7_1_4 is
     (Front_Left,              Front_Right,
      Front_Center,            Low_Frequency,
      Back_Left,               Back_Right,
      Side_Left,               Side_Right,
      Top_Front_Left,          Top_Front_Right,
      Top_Back_Left,           Top_Back_Right);

   function F_L   return Channel_Position_7_1_4 renames Front_Left;
   function F_R   return Channel_Position_7_1_4 renames Front_Right;
   function F_C   return Channel_Position_7_1_4 renames Front_Center;
   function LFE   return Channel_Position_7_1_4 renames Low_Frequency;
   function B_L   return Channel_Position_7_1_4 renames Back_Left;
   function B_R   return Channel_Position_7_1_4 renames Back_Right;
   function S_L   return Channel_Position_7_1_4 renames Side_Left;
   function S_R   return Channel_Position_7_1_4 renames Side_Right;
   function T_F_L return Channel_Position_7_1_4 renames Top_Front_Left;
   function T_F_R return Channel_Position_7_1_4 renames Top_Front_Right;
   function T_B_L return Channel_Position_7_1_4 renames Top_Back_Left;
   function T_B_R return Channel_Position_7_1_4 renames Top_Back_Right;

   function Guessed_Channel_Configuration
     (Number_Of_Channels : Positive) return Channel_Configuration
     with Post =>
       (if Guessed_Channel_Configuration'Result /= Channel_Config_Empty then
          Is_Consistent
            (Guessed_Channel_Configuration'Result,
             Number_Of_Channels));

end Audio.RIFF.Wav.Formats.Standard_Channel_Configurations;
