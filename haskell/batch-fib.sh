#!/usr/bin/env bash

readonly now=$(date +"%Y-%m-%d_%H-%M")

stack build

for i in {1..24}
do
	cmd="stack exec -- comparafun fib 30 1000 0 +RTS -H1G -A100M -N$i -RTS --output fib-parList-$i.html"
	LOG_FILE="benchmark-fib-parList-($now)-$i.log"
	collect="tee -a $LOG_FILE"
	echo "$cmd" | $collect
	$cmd | $collect
done

for i in {1..24}
do
	cmd="stack exec -- comparafun fib 30 1000 $i +RTS -H1G -A100M -N$i -RTS --output fib-parListChunk-$i.html"
	LOG_FILE="benchmark-fib-parListChunk-($now)-$i.log"
	collect="tee -a $LOG_FILE"
	echo "$cmd" | $collect
	$cmd | $collect
done


readonly endTime=$(date +"%H:%M on %Y-%m-%d")
echo "Finished at $endTime"
