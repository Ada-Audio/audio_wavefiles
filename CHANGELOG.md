# CHANGELOG

This is the changelog of the Wavefiles Package, a Wavefile Reader & Writer
Package implemented in Ada 2012.


-------------------------------------------------------------------------------

## 1.0.0, released on 2016-02-28

* Initial release.

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
