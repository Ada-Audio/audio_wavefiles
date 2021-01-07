#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

TEST_EXIT_CODE=0

function generate_src_file {
    SRC=$1
    NUM_TYPE=$2
    NUM_TYPE_2=$3
    TARGET_SRC=$(echo $SRC | sed -e 's#\.prep##' \
                 | sed -e "s#NUM_TYPE_2#${NUM_TYPE_2}#" \
                 | sed -e "s#NUM_TYPE#${NUM_TYPE}#")
    TEST_SRC=$(  echo $SRC | sed -e 's#\.prep##' \
                 | sed -e "s#NUM_TYPE_2#${NUM_TYPE_2}#" \
                 | sed -e "s#NUM_TYPE#${NUM_TYPE}_TEST#")

    if [ "$TEST" == "1" ]
    then
        OUTPUT_SRC=$TEST_SRC
    else
        OUTPUT_SRC=$TARGET_SRC
    fi

    if [ "$NUM_TYPE_2" != "" ]
    then
        gnatprep -DNUM_TYPE=${NUM_TYPE} -DNUM_TYPE_2=${NUM_TYPE_2} $SRC $OUTPUT_SRC
    else
        gnatprep -DNUM_TYPE=${NUM_TYPE} $SRC $OUTPUT_SRC
    fi

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

function generate_src_2_files {
    SRC=$1
    for num_type_1 in float fixed
    do
        generate_src_file $SRC $num_type_1
    done
}

function generate_src_4_files {
    SRC=$1
    for num_type_1 in float fixed
    do
        for num_type_2 in float fixed
        do
            generate_src_file $SRC $num_type_1 $num_type_2
        done
    done
}

generate_src_4_files $DIR/../src.prep/audio-wavefiles-generic_NUM_TYPE_wav_NUM_TYPE_2_pcm_io.ads
generate_src_4_files $DIR/../src.prep/audio-wavefiles-generic_NUM_TYPE_wav_NUM_TYPE_2_pcm_io.adb

generate_src_2_files $DIR/../src.prep/audio-wavefiles-generic_NUM_TYPE_wav_io.ads
generate_src_2_files $DIR/../src.prep/audio-wavefiles-generic_NUM_TYPE_wav_io.adb

generate_src_2_files $DIR/../src.prep/audio-wavefiles-generic_NUM_TYPE_pcm_io.ads
generate_src_2_files $DIR/../src.prep/audio-wavefiles-generic_NUM_TYPE_pcm_io.adb

generate_src_2_files $DIR/../src.prep/audio-wavefiles-generic_direct_NUM_TYPE_wav_io.ads
generate_src_2_files $DIR/../src.prep/audio-wavefiles-generic_direct_NUM_TYPE_wav_io.adb

generate_src_2_files $DIR/../test/wavefiles_test/src.prep/generic_NUM_TYPE_pcm_buffer_ops.adb
generate_src_2_files $DIR/../test/wavefiles_test/src.prep/generic_NUM_TYPE_pcm_buffer_ops.ads
generate_src_2_files $DIR/../test/wavefiles_test/src.prep/generic_NUM_TYPE_wave_test.adb
generate_src_2_files $DIR/../test/wavefiles_test/src.prep/generic_NUM_TYPE_wave_test.ads

generate_src_2_files $DIR/../test/quick_wav_data_check/src.prep/quick_wav_data_checks-NUM_TYPE_checks.ads
generate_src_2_files $DIR/../test/quick_wav_data_check/src.prep/quick_wav_data_checks-NUM_TYPE_checks.adb

exit $TEST_EXIT_CODE
