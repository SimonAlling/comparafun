#!/usr/bin/env bash

readonly now=$(date +"%Y-%m-%d_%H-%M")

stack build

readonly N=10000
readonly K=100
readonly SEED=3

i=1
cmd="stack exec -- comparafun kmeans $N $K $SEED seq +RTS -H1G -A100M -N1 -RTS --output kmeans-$i.html"
LOG_FILE="benchmark-kmeans-($now)-scaling-$i.log"
collect="tee -a $LOG_FILE"
echo "$cmd" | $collect
$cmd | $collect

for i in {2..20}
do
        cmd="stack exec -- comparafun kmeans $N $K $SEED $i +RTS -H1G -A100M -N$i -RTS --output kmeans-$i.html"
        LOG_FILE="benchmark-kmeans-($now)-scaling-$i.log"
        collect="tee -a $LOG_FILE"
        echo "$cmd" | $collect
        $cmd | $collect
done

readonly endTime=$(date +"%H:%M on %Y-%m-%d")
echo "Finished at $endTime"
