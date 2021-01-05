#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

####################
# GLOBAL VARIABLES #
####################

TEST_EXIT_CODE=0
CAT_FILE_LIST=""

#######################
# AUXILIARY FUNCTIONS #
#######################

function cookbook_check
{
    TEST_RESULT=$?
    TEST_CASE=$1
    TEST_CASE_DETAILS=$2
    CAT_FILE=$3

    if [ "$TEST_RESULT" -ne "0" ]
    then
        echo "ERROR: $TEST_CASE $TEST_CASE_DETAILS"
        TEST_EXIT_CODE=1
        if [ "$CAT_FILE" != "" ]
        then
            CAT_FILE_LIST="$CAT_FILE_LIST $CAT_FILE"
        fi
    else
        echo "PASS: $TEST_CASE $TEST_CASE_DETAILS"
    fi
}

function simple_testcase
{
    TESTCASE=$1
    LOGFILE=${TESTCASE,,}.log
    ./bin/${TESTCASE,,} \
        >& ./log/$LOGFILE
    cookbook_check "$TESTCASE" "(run check)"

    diff ./ref/$LOGFILE ./log/$LOGFILE >& ./log/diff_$LOGFILE
    cookbook_check "$TESTCASE" "(logfile check)" "./log/diff_$LOGFILE"
}

function check_wavinfo
{
    TESTCASE=$1
    WAVFILE=$2

    LOGFILE=${TESTCASE,,}_wavinfo.log
    wavinfo ./out/$WAVFILE >& ./log/$LOGFILE
    cookbook_check "$TESTCASE" "(wavinfo)"

    diff ./ref/$LOGFILE ./log/$LOGFILE >& ./log/diff_$LOGFILE
    cookbook_check "$TESTCASE" "(wavinfo logfile check)" "./log/diff_$LOGFILE"
}

function check_wavefile
{
    TESTCASE=$1
    WAVFILE=$2

    LOGFILE=${TESTCASE,,}.log

    diff ./ref/$WAVFILE ./out/$WAVFILE >& ./log/diff_$LOGFILE
    cookbook_check "$TESTCASE" "(wavfile check)" ""
}

function cleanup_data
{
    rm log/*
    rm out/*
    rm src/*
    rm cookbook.ada
}

################
# PREPARATIONS #
################

# Change to cookbook directory
cd $DIR/../cookbook

# Create .ada file based on Markdown file
sed -n '/^~~~~~~~~~~/,/^~~~~~~~~~~/ p' < cookbook.md | sed '/^~~~~~~~~~~/ d' > cookbook.ada
cookbook_check "PREPARATIONS" "(sed)" ""

# Create source-code files based on cookbook file
gnatchop -wr cookbook.ada ./src >& ./log/gnatchop.log
cookbook_check "PREPARATIONS" "(gnatchop)" "./log/gnatchop.log"

# Set GPR environment variable
AUDIO_WAVEFILES_PATH=$(cd .. && pwd)
if [ -z "$GPR_PROJECT_PATH" ]
then
    export GPR_PROJECT_PATH=${AUDIO_WAVEFILES_PATH}
else
    export GPR_PROJECT_PATH="${GPR_PROJECT_PATH}:${AUDIO_WAVEFILES_PATH}"
fi
echo "GPR_PROJECT_PATH = $GPR_PROJECT_PATH" >& ./log/gprbuild_env.log

# Build application for each source-code file
gprbuild ./cookbook.gpr  >& ./log/gprbuild.log
cookbook_check "PREPARATIONS" "(gprbuild)" "./log/gprbuild.log"

##############
# RUN & TEST #
##############

simple_testcase Open_Close_Wavefile_For_Reading

simple_testcase Open_Close_Wavefile_For_Writing
check_wavinfo   Open_Close_Wavefile_For_Writing     "test.wav"

simple_testcase Display_Errors_For_Wavefiles

simple_testcase List_Errors_For_Wavefiles

simple_testcase Display_RIFF_Chunks

simple_testcase Read_Display_Wavefile_Data

simple_testcase Write_Mono_Silence_Wavefile
check_wavinfo   Write_Mono_Silence_Wavefile         "1ch_silence.wav"
check_wavefile  Write_Mono_Silence_Wavefile         "1ch_silence.wav"

simple_testcase Write_Stereo_Sine_Wavefile
check_wavinfo   Write_Stereo_Sine_Wavefile          "2ch_sine.wav"
check_wavefile  Write_Stereo_Sine_Wavefile          "2ch_sine.wav"

simple_testcase Write_5_1_Channel_Sine_Wavefile
check_wavinfo   Write_5_1_Channel_Sine_Wavefile     "5_1ch_sine.wav"
check_wavefile  Write_5_1_Channel_Sine_Wavefile     "5_1ch_sine.wav"

simple_testcase Write_7_1_4_Channel_Sine_Wavefile
check_wavinfo   Write_7_1_4_Channel_Sine_Wavefile   "7_1_4ch_sine.wav"
check_wavefile  Write_7_1_4_Channel_Sine_Wavefile   "7_1_4ch_sine.wav"

simple_testcase Display_Channel_Config

simple_testcase Append_Wavefile
check_wavinfo   Append_Wavefile                         "2ch_sine_append.wav"
check_wavefile  Append_Wavefile                         "2ch_sine_append.wav"

simple_testcase Copy_Wavefile
check_wavinfo   Copy_Wavefile                           "2ch_sine.wav"
check_wavefile  Copy_Wavefile                           "2ch_sine.wav"

simple_testcase Copy_Wavefile_Using_Fixed_Point_Buffer
check_wavinfo   Copy_Wavefile_Using_Fixed_Point_Buffer  "2ch_sine.wav"
check_wavefile  Copy_Wavefile_Using_Fixed_Point_Buffer  "2ch_sine.wav"

simple_testcase Copy_Parts_Of_Wavefile
check_wavinfo   Copy_Parts_Of_Wavefile                  "looped_clip.wav"
check_wavefile  Copy_Parts_Of_Wavefile                  "looped_clip.wav"

simple_testcase Convert_Fixed_To_Float_Wavefile
check_wavinfo   Convert_Fixed_To_Float_Wavefile         "2ch_float_sine.wav"
check_wavefile  Convert_Fixed_To_Float_Wavefile         "2ch_float_sine.wav"

simple_testcase Downmix_Stereo_To_Mono_Wavefile
check_wavinfo   Downmix_Stereo_To_Mono_Wavefile         "1ch_dmx_sine.wav"
check_wavefile  Downmix_Stereo_To_Mono_Wavefile         "1ch_dmx_sine.wav"

simple_testcase Downmix_5_1_To_2_0_Wavefile
check_wavinfo   Downmix_5_1_To_2_0_Wavefile             "2_0ch_dmx_sine.wav"
check_wavefile  Downmix_5_1_To_2_0_Wavefile             "2_0ch_dmx_sine.wav"

simple_testcase Downmix_7_1_4_To_5_1_Wavefile
check_wavinfo   Downmix_7_1_4_To_5_1_Wavefile           "5_1ch_dmx_sine.wav"
check_wavefile  Downmix_7_1_4_To_5_1_Wavefile           "5_1ch_dmx_sine.wav"

simple_testcase Direct_Copy_Wavefile
check_wavinfo   Direct_Copy_Wavefile                    "2ch_sine.wav"
check_wavefile  Direct_Copy_Wavefile                    "2ch_sine.wav"

simple_testcase Direct_Copy_Float_Wavefile
check_wavinfo   Direct_Copy_Float_Wavefile              "2ch_float_sine.wav"
check_wavefile  Direct_Copy_Float_Wavefile              "2ch_float_sine.wav"

simple_testcase Convert_8_Bit_To_16_Bit_Wavefile

simple_testcase Read_To_Memory_Channel_Interleaved

simple_testcase Read_To_Memory_Per_Channel

simple_testcase Extract_XML_Chunk
check_wavefile  Extract_XML_Chunk                       "2020-08-09.xml"

################
# FINALIZATION #
################

if [ "$CAT_FILE_LIST" != "" ]
then
    for cat_file in $CAT_FILE_LIST
    do
        echo "------------------------------------------"
        echo $cat_file
        echo "------------------------------------------"
        cat $cat_file
    done
fi

# cleanup_data

exit $TEST_EXIT_CODE
