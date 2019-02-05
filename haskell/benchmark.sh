#!/usr/bin/env bash
readonly INT_SIZE=4000

set -x

make benchmark_seq

./Main $INT_SIZE factorials_seq +RTS -H500M

make benchmark_par

./Main $INT_SIZE factorials_par +RTS -H500M -ls -N2

threadscope Main.eventlog
