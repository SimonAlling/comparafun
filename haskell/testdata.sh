***REMOVED***

. ../config.sh

readonly SEED=3

stack build

stack exec -- comparafun kmeans testdata $KMEANS_N $KMEANS_K $SEED

echo "Copying test data to ../scala ..."
cp kmeans-correctness-*.testdata ../scala
echo "Copying test data to ../erlang ..."
cp kmeans-erlang.testdata ../erlang

