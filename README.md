Ada Library for Wavefile I/O
============================

**Version 2.0.0**

![GNAT 7 on Ubuntu 18.04](https://github.com/Ada-Audio/wavefiles/workflows/GNAT%207%20on%20Ubuntu%2018.04/badge.svg)
![GNAT 8 on Ubuntu 20.04](https://github.com/Ada-Audio/wavefiles/workflows/GNAT%208%20on%20Ubuntu%2020.04/badge.svg)
![GNAT 9 on Ubuntu 20.04](https://github.com/Ada-Audio/wavefiles/workflows/GNAT%209%20on%20Ubuntu%2020.04/badge.svg)
![GNAT 10 on Ubuntu 20.04](https://github.com/Ada-Audio/wavefiles/workflows/GNAT%2010%20on%20Ubuntu%2020.04/badge.svg)
![GNAT Community 2020 on Ubuntu 20.04](https://github.com/Ada-Audio/wavefiles/workflows/GNAT%20Community%202020%20on%20Ubuntu%2020.04/badge.svg)
![GNAT Community 2020 on Windows Server 2019](https://github.com/Ada-Audio/wavefiles/workflows/GNAT%20Community%202020%20on%20Windows%20Server%202019/badge.svg)
![GNAT Community 2020 on macOS 10.15](https://github.com/Ada-Audio/wavefiles/workflows/GNAT%20Community%202020%20on%20macOS%2010.15/badge.svg)

Introduction
------------

This Package contains a Wavefile Reader & Writer written in Ada 2012. It
supports reading and writing of wavefiles, including the following features:

- Mono, stereo and multichannel audio.
- Audio samples with following bit depths:
    - 16/24/32/64-bit PCM
    - 32/64-bit floating-point PCM
- Wave-Format-Extensible format (WAVEFORMATEXTENSIBLE)
- Conversion between PCM buffer data type and wavefile data type.
     - Adaptations for different precisions are performed automatically.

See CHANGELOG.md file for a comprehensive list of features.

License & Copyright
-------------------

This Package is available "as is" under MIT License. Unless stated otherwise,
the copyright is held by Gustavo A. Hoffmann.


Supported Platforms
-------------------

This Package has been tested with following compilers and platforms:

- GNAT FSF 7, 8, 9 and 10 for Linux;
- GNAT Community 2020 for Linux, Windows and macOS.

Using the Library
-----------------

For a list of source-code examples for various use-cases, please refer to the
[cookbook](cookbook/cookbook.md) file.
