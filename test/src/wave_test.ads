package Wave_Test is

   procedure Display_Info_File
     (File_In : String);

   procedure Copy_File
     (File_In         : String;
      File_Out        : String);

   procedure Compare_Files
     (File_Ref    : String;
      File_DUT    : String);

   procedure Diff_Files
     (File_Ref       : String;
      File_DUT       : String;
      File_Diff      : String);

   procedure Mix_Files
     (File_Ref        : String;
      File_DUT        : String;
      File_Mix        : String);

end Wave_Test;
