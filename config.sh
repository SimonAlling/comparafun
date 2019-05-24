#!/usr/bin/env bash

# Benchmarks will be run on 1, 2, ..., MAX_THREADS.
# Value should generally not exceed number of hardware threads.
MAX_THREADS=24

# For languages where no benchmarking library is used, each benchmark will be
# run REPETITIONS times.
REPETITIONS=9

# Width is number of items to map fib over.
# Depth is each item in the list.
FIB_WIDTH=1000
FIB_DEPTH=30

# n is number of points.
# k is number of desired clusters.
KMEANS_N=20000
KMEANS_K=200

