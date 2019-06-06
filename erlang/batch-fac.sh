#!/usr/bin/env bash

. ../config.sh

erlc *.erl

readonly now=$(date +"%Y-%m-%d_%H-%M")
readonly LOG_FILE="benchmark-fac-($now)-scaling.log"
readonly collect="tee -a $LOG_FILE"

echo "Width: $FAC_WIDTH" | $collect
echo "Depth: $FAC_DEPTH" | $collect
echo "Repetitions: $REPETITIONS" | $collect

for ((i=1; i<=MAX_THREADS; i++)); do
	erl +S "$i:$i" -noinput -run main facBenchmark $FAC_WIDTH $FAC_DEPTH $REPETITIONS | $collect
done
