#!/usr/bin/env bash

. ../config.sh

readonly now=$(date +"%Y-%m-%d_%H-%M")

stack build

readonly SEED=3

i=1
cmd="stack exec -- comparafun kmeans $KMEANS_N $KMEANS_K $SEED seq +RTS -H1G -A100M -N1 -RTS --output kmeans-$i.html"
LOG_FILE="benchmark-kmeans-($now)-scaling-$i.log"
collect="tee -a $LOG_FILE"
echo "$cmd" | $collect
$cmd | $collect

for ((i=2; i<=MAX_THREADS; i++)); do
        cmd="stack exec -- comparafun kmeans $KMEANS_N $KMEANS_K $SEED $i +RTS -H1G -A100M -N$i -RTS --output kmeans-$i.html"
        LOG_FILE="benchmark-kmeans-($now)-scaling-$i.log"
        collect="tee -a $LOG_FILE"
        echo "$cmd" | $collect
        $cmd | $collect
done

readonly endTime=$(date +"%H:%M on %Y-%m-%d")
echo "Finished at $endTime"
