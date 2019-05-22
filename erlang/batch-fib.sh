***REMOVED***

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

for i in {1..24}; do
	erl +S "$i:$i" -noinput -run main fibBenchmark $WIDTH $DEPTH $REPETITIONS | $collect
done
