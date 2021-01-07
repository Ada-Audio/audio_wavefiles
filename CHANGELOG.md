# CHANGELOG

This is the changelog of the Ada Wavefile Library, a Wavefile Reader & Writer
Package implemented in Ada 2012.

## 2.0.0, released on 2021-01-07

- Major refactoring of all packages of the Library.

### General

- Converted project file for GPRbuild to library project.

- The `Wavefiles` package is now a child of the `Audio` package.

- Added `Wavefiles.Report` package for reporting information to the user.

### Wavefile Features

- Aligned I/O packages with `Text_IO` package from the standard Ada library:
    - Both `Open` and `Create` procedures are now available.
    - I/O packages now make use of the `File_Mode` from the
      `Ada.Streams.Stream_IO` package.
    - Added support for appending wavefiles.

- Improved API of the `Audio.Wavefiles` package.
    - Added functions:
        - `End_Of_File`;
        - `Is_Open`;
        - `Name` (to the retrieve the file name);
        - `Sample_Rate` (to retrieve the sampling rate of a wavefile);
        - `Number_Of_Channels` (of a wavefile);
        - `Format_Of_Wavefile` (to retrieve the complete RIFF format);
        - `Is_Supported_Format` (to assess whether the Library supports the
          specified RIFF format; useful for wavefile writing).
    - Added procedures:
        - `Set_Format_Of_Wavefile` (useful for wavefile writing).

- Changed `Wavefile` data type to tagged record.
    - Updated code to use dot notation.

- Included package for common data types.
    - See `Audio.Wavefiles.Data_Types` package.

- Replaced exception-based error handling by error and warning *codes*.
    - Added enumerations for error and warning codes.
        - See `Wavefile_Error_Code` and `Wavefile_Warning_Code` types and
          `Errors` and `Warnings` functions.
    - Changed procedures to track errors instead of raising exceptions.

- Improved consistency checks.
    - Added check for consistency of channel mask.

- Improved support for sample counting, positioning, and timing information.
    - Added support for retrieving time information in seconds.
    - Added support for retrieving sample-accurate information with following
      functions:
        - `Total_Sample_Count`;
        - `First_Sample`, `Current_Sample`, and `Last_Sample`.
    - Added support for setting the current position in the wavefile with
      following procedures:
        - `Set_Sample_Position`;
        - `Set_Sample_Time`.
    - Improved reliability of data types used for sample counting.
        - The Library now uses `Long_Long_Integer` for sample information.
    - Added support for flexible (internal) sample count ranges starting at
      index zero or one.

- Added support for RIFF chunk parsing.
    - Added support for identifying all known chunk formats.
        - Known chunks are listed in `Audio.RIFF.Wav.Formats`.
    - Added support for reading multiple chunks.
        - Added `Get_RIFF_Info` to retrieve information about all RIFF chunks
          from a wavefile.
        - Added `Get_First_Chunk` procedure to identify location of a chunk
          in the wavefile based on a chunk tag.
        - Added `Chunk_Element_Data` function to read data of a chunk.
            - The returned data is stored in an array of bytes (`Byte_Array`
              type).
    - Added support for listing chunks found in a wavefile.
        - See `Display_Info` procedure for `RIFF_Information` type.

- Added support for globally unique identifiers (GUIDs).
    - Added support for identifying all known GUIDs.
        - Known GUIDs are listed in `Audio.RIFF.Wav.GUIDs`.

- Improved support for handling of RIFF wave format.
    - Split `RIFF` package into `Audio.RIFF`, `Audio.RIFF.Wav` and
      `Audio.RIFF.Wav.Formats`.
    - Added enumerations for some format information, including:
        - bit depth;
        - sampling rate;
        - wave format tag.
    - Added functions for sampling rate handling.
        - Added `To_Float` and `To_Positive` functions to convert sampling-rate
          enumerations.
    - Improved support for channel information.
        - Replaced `Channel_Mask_Type` by `Channel_Configuration`.
        - Added `Audio.RIFF.Wav.Formats.Channel_Configurations` package, which
          includes:
            - standard channel configurations, and
            - standard channel positions.
        - Added function for guessing the channel configuration based on number
          of channels.

### PCM Buffer Features

- Removed support for operations on PCM buffers.
    - Removed support for common operations (`=`, `*`, `+`, `-`) and custom
      operations.
    - This feature might be re-introduced in the future using better design.

- Improved consistency of the terminology used in the API.
    - Clarified distinction between wavefile interfacing and PCM buffer:
        - The prefix `Wav` is used for features that refer to data from
          wavefiles, while
        - the prefix `PCM` is used for features that refer to the PCM buffer.
    - This includes terms such as `PCM_Sample` and `Wav_Sample` for the data
      type used for samples stored in PCM buffers and in wavefiles.

- Added support for reading and writing of wavefiles in floating-point format.
    - The Library now uses formal floating-point and fixed-point data types as
      parameters of the generic I/O packages.

- Improved maintainability of the Library by adding support for generated
  source-code files for fixed-point and floating-point targets.

- Improved conversion between PCM and wavefile data types.
    - Fixed issue affecting small negative values in the PCM data.
    - Fixed issue affecting 24-bit wavefiles.

- Added support for arbitrary channel ranges.
    - Added formal `Channel_Range` parameter to the generic `PCM_IO` / `Wav_IO`
      packages.

- Added procedural version of `Get` (for reading samples from a wavefile).

#### Testing

- Added test for generated source-code files to prevent *local* changes.

- Added test of data accuracy — including data I/O and data type conversion —
  by checking extreme values on following formats and bit depths:
    - 16/24/32/64-bit fixed-point data;
    - 32/64 floating-point data.

- Added tests for all source-code examples of the cookbook.
    - Added check of wavefile information (using `wavinfo` tool).
    - Added comparison check for standard output / error.
    - Added comparison check for wavefiles.
    - Reference logfiles and wavefiles are stored in the `cookbook/ref`
      directory.

- Introduced simple benchmarking environment for performance checks.
    - NOTE: reference benchmarks and benchmark history are not available.

- Added support for continuous integration (CI) testing using GitHub actions
  and Travis-CI.
    - Added all tests mentioned above to the CI scripts.
    - Extended compiler version and platform coverage by testing following
      configurations using GitHub Actions:
        - GNAT FSF 7, 8, 9, and 10 (on Ubuntu 18.04 and 20.04);
        - GNAT Community 2020 on Linux, Windows and macOS.

### Documentation

- Introduced cookbook with source-code examples for various use-cases,
  including the following use-cases:
    - Opening and closing of a wavefile for reading or writing.
    - Reading data from a wavefile.
    - Reading of a complete wavefile to the memory.
    - Writing data to a wavefile.
    - Writing sine tones to stereo, 5.1-channel and 7.1.4-channel wavefiles.
    - Appending a wavefile.
    - Copying a complete wavefile sample by sample using floating-point or
      fixed-point PCM buffers.
    - Copying parts of a wavefile (i.e. creating a loop over an audio clip).
    - Converting a 16-bit (fixed-point/integer) wavefile to a 32-bit
      floating-point wavefile.
    - Down-mixing of wavefiles for following use-cases:
        - stereo to mono wavefile;
        - 5.1-channel to stereo wavefile;
        - 7.1.4-channel to 5.1-channel wavefile.
    - Direct reading of data from a wavefile without PCM buffer conversion (for
      16-bit wavefile).
    - Direct writing of data to a wavefile without PCM buffer conversion (for
      32-bit floating-point wavefile).
    - Direct conversion of an 8-bit PCM wavefile to a 16-bit PCM wavefile.

-------------------------------------------------------------------------------

## 1.0.0, released on 2016-02-28

- Initial release.

### Wavefile Features

Reading and writing of wavefiles supporting following features:

- Stereo and multichannel audio
- Audio samples with following bit depths:
    - 16-bit PCM
    - 24-bit PCM
    - 32-bit PCM
- Wave-Format-Extensible (WAVE_FORMAT_EXTENSIBLE)
- Conversion between PCM buffer data type and wavefile data type.
     - Adaptations for different precisions are performed automatically.

### PCM Buffer Features

- Built-in handling of PCM buffers
    - The Wavefiles Package is a generic package that can be instantiated for
      different formats of PCM buffers.
    - When instantiating the generic package, the user must specify:
        - The maximum number of samples that the buffer can contain
        - The numerical data type for storing the PCM samples
    - The numerical data type of the PCM samples can be:
        - A floating-point type of arbitrary precision
        - A fixed-point type of arbitrary precision
    - When declaring an instance of the PCM buffer, the user must specify:
        - The number of channels

- Support for operations on PCM buffers:
    - "=", "*", "+", "-"
    - Custom operations (using function Wavefiles.PCM_Buffers.Perform)

### Known limitations

#### Wavefile Features

Following features are not currently supported:

- Reading and writing of 8-bit PCM wavefiles
- Reading and writing of wavefiles in floating-point format

#### PCM Buffer Features

- Data type conversion to be investigated and improved.
    - Conversion of small negative values might be improved.

#### Documentation

- Extensive documentation and tutorials are missing.
    - Please refer to the test folder for an example on how to use the Package.

#### Testing

- Unit test for the Package is missing.
    - Just a test module for manual checks is currently available.

- Testing of wavefiles in various formats is missing.
- Testing of erroneous wavefiles is missing.
