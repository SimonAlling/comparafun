***REMOVED***

. ../threads.sh

erlc *.erl

readonly now=$(date +"%Y-%m-%d_%H-%M")
readonly LOG_FILE="benchmark-kmeans-($now)-scaling.log"
readonly collect="tee -a $LOG_FILE"

readonly N=20000
readonly K=200
readonly REPETITIONS=9

echo "n: $N" | $collect
echo "k: $K" | $collect
echo "Repetitions: $REPETITIONS" | $collect

for ((i=1; i<=MAX_THREADS; i++)) do
	erl +S "$i:$i" -noinput -run main kmeansBenchmark $N $K $REPETITIONS | $collect
done
