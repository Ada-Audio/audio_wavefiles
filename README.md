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

This Library contains a Wavefile Reader & Writer written in Ada 2012. It
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

To contribute to this project, please refer to the guidelines described in the
[CONTRIBUTING](CONTRIBUTING.md) file.


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

### Using ALIRE

You can retrieve this Library as a crate from
[ALIRE](https://alire.ada.dev) (the Ada LIbrary REpository):

```sh
alr get audio_wavefiles
```

This Library depends on the `audio_base` crate. If you use the
command-line above, all dependencies are automatically retrieved. However,
you could retrieve that crate from [ALIRE](https://alire.ada.dev) as well
using:

```sh
alr get audio_base
```

Then, you can build the Library (as a standalone library) with
[ALIRE](https://alire.ada.dev) using the following command:

```sh
cd audio_wavefiles*

alr build
```

Usually, however, you would like to use the Library in your project. You
can do that by adding the dependency to your project by running this
command from the root directory of your project:

```sh
alr with audio_wavefiles
```

Finally, you can build your project using [ALIRE](https://alire.ada.dev):

```sh
alr build
```

### Cloning the source-code

Alternatively, you can clone the source-code of the Library and its
dependencies using these commands from the root directory of your project:

```sh
mkdir deps

(cd deps && git clone https://github.com/Ada-Audio/audio_base )
(cd deps && git clone https://github.com/Ada-Audio/audio_wavefiles )
```

Then, you have to include the Library in your GPRbuild project by adding
the following line:

```
with "audio_wavefiles.gpr";
```

You can use GPRbuild to build your project with the Library. Don't forget
to set the path to the GPRbuild projects using the environment variable
`GPR_PROJECT_PATH`:


```sh
# Set path to audio_base and audio_wavefiles
export GPR_PROJECT_PATH="$(cd deps/audio_base && pwd):$(cd deps/audio_wavefiles && pwd)"

gprbuild
```


Using the Library
-----------------

To use the library, you have to add a reference to the `Wavefiles` package to
your source-code file:

```ada
with Audio.Wavefiles; use Audio.Wavefiles;
```

Then, you can open and close a wavefile:

```ada
   WF            : Wavefile;
begin
   WF.Open (In_File, "test.wav");

   WF.Close;
```

Also, you should instantiate at least one of the PCM I/O packages. To do that,
you could reference, for example, the generic PCM I/O package for
floating-point types:

```ada
with Audio.Wavefiles.Generic_Float_PCM_IO;
use  Audio.Wavefiles.Generic_Float_PCM_IO;
```

You can then instantiate this package by writing, for example:

```ada
   type Float_Array is array (Positive range <>) of Float;

   package PCM_IO is new Audio.Wavefiles.Generic_Float_PCM_IO
     (PCM_Sample    => Float,
      Channel_Range => Positive,
      PCM_MC_Sample => Float_Array);
   use PCM_IO;
```

You can now read data from the wavefile:

```ada
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
