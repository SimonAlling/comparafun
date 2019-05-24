***REMOVED***

. ../config.sh

erlc *.erl

readonly now=$(date +"%Y-%m-%d_%H-%M")
readonly LOG_FILE="benchmark-fib-($now)-scaling.log"
readonly collect="tee -a $LOG_FILE"

echo "Width: $FIB_WIDTH" | $collect
echo "Depth: $FIB_DEPTH" | $collect
echo "Repetitions: $REPETITIONS" | $collect

for ((i=1; i<=MAX_THREADS; i++)); do
	erl +S "$i:$i" -noinput -run main fibBenchmark $FIB_WIDTH $FIB_DEPTH $REPETITIONS | $collect
done
