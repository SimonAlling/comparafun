***REMOVED***

***REMOVED***

readonly N=$1
readonly K=$2
readonly SEED=$3

stack exec -- comparafun kmeans correctness $N $K $SEED

mv *.haskell *.testdata ../scala/benchmarks

cd ../scala/benchmarks

sbt "runMain KMeansCorrectness $N $K $SEED"

diff "kmeans-correctness-$N-$K-$SEED.expected" "kmeans-correctness-$N-$K-$SEED.actual"