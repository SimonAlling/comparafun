#!/usr/bin/env bash

set -x

readonly now=$(date +"%Y-%m-%d_%H-%M")
readonly LOG_FILE="benchmark-$now.log"
readonly cmd="stack exec -- comparafun-kmeans batch kmeans +RTS -H1G -A100M"
readonly collect="tee $LOG_FILE"

stack build

echo "**** 2 HECs ****"
$cmd -N2 | $collect

echo "**** 4 HECs ****"
$cmd -N4 | $collect

echo "**** 8 HECs ****"
$cmd -N8 | $collect

echo "Done!"
