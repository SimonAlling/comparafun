***REMOVED***

readonly now=$(date +"%Y-%m-%d_%H-%M")

stack build

for i in {1..20}
do
	cmd="stack exec -- comparafun fib batch +RTS -H1G -A100M -N$i -RTS --output fib-$i.html"
	LOG_FILE="benchmark-fib-($now)-scaling-$i.log"
	collect="tee -a $LOG_FILE"
	echo "$cmd" | $collect
	$cmd | $collect
done

readonly endTime=$(date +"%H:%M on %Y-%m-%d")
echo "Finished at $endTime"
