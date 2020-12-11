#!/bin/bash

set -e

PATH=$GNAT_PATH/bin:$PATH

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd $DIR && cd ..

echo ""
echo "======================================================================="
echo "Install dependencies"
echo "======================================================================="

. ./scripts/install_deps.sh

echo ""
echo "======================================================================="
echo "Verify generated source-code files"
echo "======================================================================="

TEST=1 ./scripts/generate_src.sh

echo ""
echo "======================================================================="
echo "Build test for Wavefile Library"
echo "======================================================================="

gprbuild -P ./test/wavefiles_test/wavefiles_test.gpr

echo ""
echo "======================================================================="
echo "Build & run Quick Wav-Data Check"
echo "======================================================================="

gprbuild -P ./test/quick_wav_data_check/quick_wav_data_check.gpr && \
    ./test/quick_wav_data_check/bin/quick_wav_data_check "$(date +"%Y%m%d-%H%M%S")_"

echo ""
echo "======================================================================="
echo "Build & check Cookbook"
echo "======================================================================="

./scripts/test_cookbook.sh

echo ""
echo "======================================================================="
echo "Build & run Simple Benchmarking"
echo "======================================================================="

gprclean -r -P ./test/simple_benchmarking/simple_benchmarking.gpr   && \
    gprbuild -P ./test/simple_benchmarking/simple_benchmarking.gpr     \
    -XWAVEFILES_BUILD_MODE=RELEASE                                  && \
    ./scripts/simple_benchmarking.sh
