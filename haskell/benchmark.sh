#!/usr/bin/env bash
readonly INT_SIZE=30

set -x

make benchmark_par

./Main $INT_SIZE fibonaccis +RTS -H1G -A100M -N1

./Main $INT_SIZE fibonaccis +RTS -H1G -A100M -N4
