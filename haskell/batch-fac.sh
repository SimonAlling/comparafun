#!/usr/bin/env bash

. ../config.sh

readonly now=$(date +"%Y-%m-%d_%H-%M")

stack build

i=1
cmd="stack exec -- comparafun fac $FAC_DEPTH $FAC_WIDTH seq +RTS -H1G -A100M -N$i -RTS --output fac-par-$i.html"
LOG_FILE="benchmark-fac-($now)-$i.log"
collect="tee -a $LOG_FILE"
echo "$cmd" | $collect
$cmd | $collect

for ((i=2; i<=MAX_THREADS; i++)); do
	cmd="stack exec -- comparafun fac $FAC_DEPTH $FAC_WIDTH $i +RTS -H1G -A100M -N$i -RTS --output fac-par-$i.html"
	LOG_FILE="benchmark-fac-($now)-$i.log"
	collect="tee -a $LOG_FILE"
	echo "$cmd" | $collect
	$cmd | $collect
done

readonly endTime=$(date +"%H:%M on %Y-%m-%d")
echo "Finished at $endTime"
