#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

( CPU_MHZ=`lscpu | grep "CPU MHz" | sed -e 's#CPU MHz:##'` && ./test/simple_benchmarking/obj/simple_benchmarking $CPU_MHZ )
