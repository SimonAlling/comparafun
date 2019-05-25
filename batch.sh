***REMOVED***

. ./config.sh

echo "I'm about to run ALL benchmarks. This can take several hours."
echo
echo "I will use a maximum of $MAX_THREADS threads."
echo "To change this, please edit config.sh."
echo 

sleep 2

echo
echo "Running Haskell benchmarks ..."
cd haskell
./batch-fib.sh
./batch-kmeans.sh
./batch-fac.sh
cd -

echo
echo "Running Erlang benchmarks ..."
cd erlang
./batch-fib.sh
./batch-kmeans.sh
cd -

echo
echo "Running Scala benchmarks ..."
cd scala
./batch.sh fib
./batch.sh kmeans
./batch.sh fac
cd -

echo
echo "Running Manticore benchmarks ..."
cd manticore
./batch.sh
cd -

echo
echo "Done."

