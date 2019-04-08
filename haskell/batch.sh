#!/usr/bin/env bash

readonly now=$(date +"%Y-%m-%d_%H-%M")
readonly LOG_FILE="benchmark-$now.log"
readonly cmd="stack exec -- comparafun-kmeans batch kmeans +RTS -H1G -A100M"
readonly collect="tee -a $LOG_FILE"

stack build

echo "$cmd" | $collect

echo "**** 4 HECs ****"
$cmd -N4 | $collect

echo "**** 10 HECs ****"
$cmd -N10 | $collect

echo "**** 20 HECs ****"
$cmd -N20 | $collect

echo "-------- DONE" | $collect
