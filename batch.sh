#!/usr/bin/env bash

. ./config.sh

echo "I'm about to run ALL benchmarks. This can take several hours."
echo
echo "I will use these parameters:"
echo "    Maximum number of threads: $MAX_THREADS"
echo "    Repetitions: $REPETITIONS"
echo "    Fibonacci width: $FIB_WIDTH"
echo "    Fibonacci depth: $FIB_DEPTH"
echo "    k-means n: $KMEANS_N"
echo "    k-means k: $KMEANS_K"
echo "    Factorial width: $FAC_WIDTH"
echo "    Factorial depth: $FAC_DEPTH"
echo ""
echo "To change these parameters, please edit config.sh."
echo 

sleep 5

echo
echo "Running Haskell benchmarks ..."
cd haskell
./batch-fib.sh
./batch-kmeans.sh
./batch-fac.sh
cd -

echo
echo "Running Erlang benchmarks ..."
cd erlang
./batch-fib.sh
./batch-kmeans.sh
./batch-fac.sh
cd -

echo
echo "Running Scala benchmarks ..."
cd scala
./batch.sh fib
./batch.sh kmeans
./batch.sh fac
cd -

echo
echo "Running Manticore benchmarks ..."
cd manticore
./batch-fib.sh
./batch-fac.sh
cd -

echo
echo "Done."

