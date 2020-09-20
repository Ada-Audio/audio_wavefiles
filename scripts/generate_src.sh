#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

TEST_EXIT_CODE=0

function generate_src_file {
    SRC=$1
    NUM_TYPE=$2
    TARGET_SRC=$(echo $SRC | sed -e 's#\.prep##' \
                 | sed -e "s#NUM_TYPE#${NUM_TYPE}#")
    TEST_SRC=$(  echo $SRC | sed -e 's#\.prep##' \
                 | sed -e "s#NUM_TYPE#${NUM_TYPE}_TEST#")

    if [ "$TEST" == "1" ]
    then
        OUTPUT_SRC=$TEST_SRC
    else
        OUTPUT_SRC=$TARGET_SRC
    fi

    gnatprep -DNUM_TYPE=${NUM_TYPE} $SRC $OUTPUT_SRC

    if [ "$TEST" == "1" ]
    then
        diff $TARGET_SRC $TEST_SRC
        if [ "$?" -ne "0" ]
        then
            echo "ERROR: differences detected in file $TARGET_SRC"
            TEST_EXIT_CODE=1
        else
            echo "PASS: $TARGET_SRC"
        fi

        rm -f $TEST_SRC
    fi
}

function generate_src_files {
    SRC=$1
    generate_src_file $SRC float
    generate_src_file $SRC fixed
}

generate_src_files $DIR/../src.prep/audio-wavefiles-generic_NUM_TYPE_pcm_conversions.ads
generate_src_files $DIR/../src.prep/audio-wavefiles-generic_NUM_TYPE_pcm_conversions.adb
generate_src_files $DIR/../src.prep/audio-wavefiles-generic_NUM_TYPE_wav_io.ads
generate_src_files $DIR/../src.prep/audio-wavefiles-generic_NUM_TYPE_wav_io.adb
generate_src_files $DIR/../src.prep/audio-wavefiles-generic_NUM_TYPE_pcm_io.ads
generate_src_files $DIR/../src.prep/audio-wavefiles-generic_NUM_TYPE_pcm_io.adb

generate_src_files $DIR/../test/src.prep/generic_NUM_TYPE_pcm_buffer_ops.adb
generate_src_files $DIR/../test/src.prep/generic_NUM_TYPE_pcm_buffer_ops.ads
generate_src_files $DIR/../test/src.prep/generic_NUM_TYPE_wave_test.adb
generate_src_files $DIR/../test/src.prep/generic_NUM_TYPE_wave_test.ads

exit $TEST_EXIT_CODE
