if ( Test-Path -Path '.\deps\audio_base' -PathType Container )
{
    "Using available component!"
}
else
{
    New-Item -Path ".\deps" -ItemType Directory

    git clone --branch release/1.0 https://github.com/Ada-Audio/audio_base ".\deps\audio_base"
}
