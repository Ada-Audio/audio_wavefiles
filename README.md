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

This library also includes support for PCM buffers in floating-point and
fixed-point formats, as well as the automatic conversion between the data types
used for the PCM buffer and the wavefile, which might have different formats
(floating-point vs. fixed-point) or precisions (e.g., 16 bits vs. 64 bits).

In addition, following features are available:

- Support for parsing known GUIDs
- Support for parsing RIFF chunks
- Support for sample positioning and timing information

See the [CHANGELOG](CHANGELOG.md) file for a comprehensive list of features.


License & Copyright
-------------------

As indicated in the [LICENSE](LICENSE) file, this Package is available "as is"
under MIT License. Please refer to that file for all licensing conditions.
Unless stated otherwise, the copyright is held by Gustavo A. Hoffmann.


Supported Platforms
-------------------

This Package has been tested with following compilers and platforms:

- GNAT FSF 7, 8, 9 and 10 for Linux;
- GNAT Community 2020 for Linux, Windows and macOS.


Setting Up the Library
----------------------

You can use GPRbuild to build the library:

```
gprbuild -P wavefiles_lib.gpr
```

However, since parts of the Wavefiles package are generic and therefore need to
be instantiated, it's advisable to include this library in your project by
adding the following line:

```
with "wavefiles_lib.gpr";
```


Using the Library
-----------------

To use the library, you have to add a reference to the `Wavefiles` package to
your source-code file:

```
with Audio.Wavefiles; use Audio.Wavefiles;
```

Then, you can open and close a wavefile:

```
   WF            : Wavefile;
begin
   WF.Open (In_File, "test.wav");

   WF.Close;
```

Also, you should instantiate at least one of the PCM I/O packages. To do that,
you could reference, for example, the generic PCM I/O package for
floating-point types:

```
with Audio.Wavefiles.Generic_Float_PCM_IO;
use  Audio.Wavefiles.Generic_Float_PCM_IO;
```

You can then instantiate this package by writing, for example:

```
   type Float_Array is array (Positive range <>) of Float;

   package PCM_IO is new Audio.Wavefiles.Generic_Float_PCM_IO
     (PCM_Sample    => Float,
      Channel_Range => Positive,
      PCM_MC_Sample => Float_Array);
   use PCM_IO;
```

You can now read data from the wavefile:

```
      loop
         declare
            PCM_Buf : constant Float_Array := Get (WF);
         begin
            exit when WF.End_Of_File;
         end;
      end loop;
```

For a list of source-code examples for various use-cases — starting from the
simplest ones to more complicated use-cases —, please refer to the
[cookbook](cookbook/cookbook.md) file.
