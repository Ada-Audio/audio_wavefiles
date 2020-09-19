#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

function generate_src_file {
    SRC=$1
    NUM_TYPE=$2

    gnatprep -DNUM_TYPE=${NUM_TYPE} $SRC $(echo $SRC | sed -e 's#\.prep##' | sed -e "s#NUM_TYPE#${NUM_TYPE}#")
}

function generate_src_files {
    SRC=$1
    generate_src_file $SRC float
    generate_src_file $SRC fixed
}

generate_src_files $DIR/../src.prep/audio-wavefiles-generic_NUM_TYPE_types.ads
generate_src_files $DIR/../src.prep/audio-wavefiles-generic_NUM_TYPE_types.adb
generate_src_files $DIR/../src.prep/audio-wavefiles-generic_NUM_TYPE_io.ads
generate_src_files $DIR/../src.prep/audio-wavefiles-generic_NUM_TYPE_io.adb

generate_src_files $DIR/../test/src.prep/generic_NUM_TYPE_pcm_buffer_ops.adb
generate_src_files $DIR/../test/src.prep/generic_NUM_TYPE_pcm_buffer_ops.ads
generate_src_files $DIR/../test/src.prep/generic_NUM_TYPE_wave_test.adb
generate_src_files $DIR/../test/src.prep/generic_NUM_TYPE_wave_test.ads
