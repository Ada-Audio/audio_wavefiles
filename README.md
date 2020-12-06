Wavefiles: Ada Wavefile Reader & Writer Package
===============================================
Version 1.0.0

![GNAT 7 on Ubuntu 18.04](https://github.com/Ada-Audio/wavefiles/workflows/GNAT%207%20on%20Ubuntu%2018.04/badge.svg)
![GNAT 8 on Ubuntu 20.04](https://github.com/Ada-Audio/wavefiles/workflows/GNAT%208%20on%20Ubuntu%2020.04/badge.svg)
![GNAT 9 on Ubuntu 20.04](https://github.com/Ada-Audio/wavefiles/workflows/GNAT%209%20on%20Ubuntu%2020.04/badge.svg)
![GNAT 10 on Ubuntu 20.04](https://github.com/Ada-Audio/wavefiles/workflows/GNAT%2010%20on%20Ubuntu%2020.04/badge.svg)
![GNAT Community 2020 on Ubuntu 20.04](https://github.com/Ada-Audio/wavefiles/workflows/GNAT%20Community%202020%20on%20Ubuntu%2020.04/badge.svg)
![GNAT Community 2020 on Windows Server 2019](https://github.com/Ada-Audio/wavefiles/workflows/GNAT%20Community%202020%20on%20Windows%20Server%202019/badge.svg)
![GNAT Community 2020 on macOS 10.15](https://github.com/Ada-Audio/wavefiles/workflows/GNAT%20Community%202020%20on%20macOS%2010.15/badge.svg)

1. Introduction
---------------

This Package contains a Wavefile Reader & Writer written in Ada 2012.

2. License & Copyright
----------------------

This Package is available "as is" under MIT License. Unless stated otherwise,
the copyright is held by Gustavo A. Hoffmann.

2. Supported Platforms
----------------------

This Package has been tested on the following compilers / platforms:

- GNAT GPL 2015 for Linux

The Package is expected to work on Windows and Mac platforms.

3. Features
-----------

### Wavefile Features

Reading and writing of wavefiles supporting following features:

- Stereo and multichannel audio
- Audio samples with following bit depths:
    - 16-bit PCM
    - 24-bit PCM
    - 32-bit PCM
- Wave-Format-Extensible (WAVEFORMATEXTENSIBLE)
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

4. Known Issues and Limitations
-------------------------------

### Wavefile Features

Following features are not currently supported:

- Reading and writing of 8-bit PCM wavefiles
- Reading and writing of wavefiles in floating-point format

### PCM Buffer Features

- Data type conversion to be investigated and improved.
    - Conversion of small negative values might be improved.

### Documentation

- Extensive documentation and tutorials are missing.
    - Please refer to the test folder for an example on how to use the Package.

### Testing

- Unit test for the Package is missing.
    - Just a test module for manual checks is currently available.

- Testing of wavefiles in various formats is missing.
- Testing of erroneous wavefiles is missing.
