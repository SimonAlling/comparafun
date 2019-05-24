***REMOVED***

. ../config.sh

erlc *.erl

readonly now=$(date +"%Y-%m-%d_%H-%M")
readonly LOG_FILE="benchmark-kmeans-($now)-scaling.log"
readonly collect="tee -a $LOG_FILE"

cd ../haskell
./testdata.sh
cd -

echo "n: $KMEANS_N" | $collect
echo "k: $KMEANS_K" | $collect
echo "Repetitions: $REPETITIONS" | $collect

for ((i=1; i<=MAX_THREADS; i++)) do
	erl +S "$i:$i" -noinput -run main kmeansBenchmark $KMEANS_N $KMEANS_K $REPETITIONS | $collect
done
