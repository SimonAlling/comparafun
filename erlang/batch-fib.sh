***REMOVED***

. ../threads.sh

erlc *.erl

readonly now=$(date +"%Y-%m-%d_%H-%M")
readonly LOG_FILE="benchmark-fib-($now)-scaling.log"
readonly collect="tee -a $LOG_FILE"

readonly WIDTH=1000
readonly DEPTH=30
readonly REPETITIONS=9

echo "Width: $WIDTH" | $collect
echo "Depth: $DEPTH" | $collect
echo "Repetitions: $REPETITIONS" | $collect

for ((i=1; i<=MAX_THREADS; i++)); do
	erl +S "$i:$i" -noinput -run main fibBenchmark $WIDTH $DEPTH $REPETITIONS | $collect
done
