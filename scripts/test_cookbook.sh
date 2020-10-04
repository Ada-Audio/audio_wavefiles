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
        >& $LOGFILE
    cookbook_check "$TESTCASE" "(run check)"

    diff ./ref/$LOGFILE $LOGFILE >& diff_$LOGFILE
    cookbook_check "$TESTCASE" "(logfile check)" "diff_$LOGFILE"
}

function check_wavinfo
{
    TESTCASE=$1
    WAVFILE=$2

    LOGFILE=${TESTCASE,,}_wavinfo.log
    wavinfo ./out/$WAVFILE >& $LOGFILE
    cookbook_check "$TESTCASE" "(wavinfo)"

    diff ./ref/$LOGFILE $LOGFILE >& diff_$LOGFILE
    cookbook_check "$TESTCASE" "(wavinfo logfile check)" "diff_$LOGFILE"
}

function check_wavefile
{
    TESTCASE=$1
    WAVFILE=$2

    LOGFILE=${TESTCASE,,}.log

    diff ./ref/$WAVFILE ./out/$WAVFILE >& diff_$LOGFILE
    cookbook_check "$TESTCASE" "(wavfile check)" ""
}

################
# PREPARATIONS #
################

# Change to cookbook directory
cd $DIR/../cookbook

# Create .ada file based on Markdown file
sed -n '/^```/,/^```/ p' < cookbook.md | sed '/^```/ d' > cookbook.ada
cookbook_check "PREPARATIONS" "(sed)" ""

# Create source-code files based on cookbook file
gnatchop -wr cookbook.ada ./src >& gnatchop.log
cookbook_check "PREPARATIONS" "(gnatchop)" "gnatchop.log"

# Build application for each source-code file
gprbuild ./cookbook.gpr  >& gprbuild.log
cookbook_check "PREPARATIONS" "(gprbuild)" "gprbuild.log"

##############
# RUN & TEST #
##############

simple_testcase Open_Close_Wavefile_For_Reading

simple_testcase Open_Close_Wavefile_For_Writing
check_wavinfo   Open_Close_Wavefile_For_Writing "test.wav"

simple_testcase Write_Silence_Mono_Wavefile
check_wavinfo   Write_Silence_Mono_Wavefile     "1ch_silence.wav"
check_wavefile  Write_Silence_Mono_Wavefile     "1ch_silence.wav"

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

rm *.log
rm out/*

exit $TEST_EXIT_CODE
