Ada Wavefile Library
====================

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
- Wave-Format-Extensible format (WAVE_FORMAT_EXTENSIBLE)

This library also includes support for PCM buffers in floating-point and
fixed-point formats, as well as the automatic conversion between the data types
used for the PCM buffer and the wavefile, which might have different formats
(floating-point or fixed-point) or precisions (e.g., 16 bits or 64 bits).

In addition, following features are available:

- Support for parsing known GUIDs
- Support for parsing RIFF chunks
- Support for sample positioning and timing information

See the [CHANGELOG](CHANGELOG.md) file for a comprehensive list of features.

To contribute to this project, please refer to the guidelines described in the
[CONTRIBUTING](CONTRIBUTING.md) file.


License & Copyright
-------------------

As indicated in the [LICENSE](LICENSE) file, this Library is available "as is"
under MIT License. Please refer to that file for all licensing conditions.
Unless stated otherwise, the copyright is held by Gustavo A. Hoffmann.


Supported Platforms
-------------------

This Library has been tested with following compilers and platforms:

- GNAT FSF 7, 8, 9 and 10 for Linux;
- GNAT Community 2020 for Linux, Windows and macOS.


Setting Up the Library
----------------------

### Cloning the source-code

You can clone the source-code of the Library and its dependencies using these
commands from the root directory of your project:

```sh
mkdir deps

(cd deps && git clone https://github.com/Ada-Audio/audio_base )
(cd deps && git clone https://github.com/Ada-Audio/audio_wavefiles )
```

Note that you can use the `--branch` option to retrieve a specific version —
for example: `--branch 2.0.0`.

Another method is to use git submodules:

```sh
git submodule add https://github.com/Ada-Audio/audio_base ./deps/audio_base
git submodule add https://github.com/Ada-Audio/audio_wavefiles ./deps/audio_wavefiles
```

### Integration with your project

This section describes how to manually integrate the Library into your GPRbuild
project. If you're using ALIRE, please refer to the next section.

You can include the Library in your GPRbuild project by adding the following
line:

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

### Using ALIRE

If you're using [ALIRE](https://alire.ada.dev) (the Ada LIbrary REpository),
this section shows how you can integrate the Library to your project using the
ALIRE environment. These are the prerequisites:

1. You have cloned the source-code of the Library and its dependencies using
   one of the methods described above.

2. You have initialized your project for ALIRE (using `alr init --bin` or
   similar).

You can now integrate the Library and its dependencies to the ALIRE
environment using these commands:

```sh
alr with audio_base      --use $(cd deps/audio_base      && pwd)
alr with audio_wavefiles --use $(cd deps/audio_wavefiles && pwd)
```

For the remaining of this section, we'll assume that your ALIRE project is
called `wavefile_test`. Just replace this name with the actual name of your
project wherever it's appropriate.

Now, let's say you have the following main application in
`./src/wavefile_test.adb`:

```ada
with Ada.Text_IO;     use Ada.Text_IO;
with Audio.Wavefiles; use Audio.Wavefiles;

procedure Wavefile_Test is
   WF : Wavefile;
begin
   WF.Create (Out_File, "test.wav");

   if WF.Is_Open then
      Put_Line ("Created output wavefile.");

      WF.Close;
   end if;
end Wavefile_Test;
```

You can build the project using ALIRE:

```
alr build
```

And run it with this command:

```
./alire/build/bin/wavefile_test
```


Using the Library
-----------------

To use the Library, you have to add a reference to the `Wavefiles` package to
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

You can then instantiate this package for reading, for example:

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
[cookbook](cookbook/README.md).


Roadmap
-------

These features are on the list for future versions of this Library:

- Support for [Broadcast Wave Format](https://en.wikipedia.org/wiki/Broadcast_Wave_Format)
- Support for [RF64](https://en.wikipedia.org/wiki/RF64)
- Support for [Audio Definition Model](https://www.itu.int/rec/R-REC-BS.2076)
- Support for [Serial Representation of the Audio Definition Model](https://www.itu.int/rec/R-REC-BS.2125)
