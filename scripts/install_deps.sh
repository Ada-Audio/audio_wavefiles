#!/bin/bash

set -e

if [ -d deps/audio_base ]
then
    echo "Using existing audio_base component"
else
    mkdir -p deps
    (cd deps && git clone --branch release/1.0 https://github.com/Ada-Audio/audio_base )
fi

export AUDIO_BASE_PATH="$(cd deps/audio_base && pwd)"
export GPR_PROJECT_PATH="$AUDIO_BASE_PATH"
