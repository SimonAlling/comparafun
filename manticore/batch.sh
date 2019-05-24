***REMOVED***

readonly now=$(date +"%Y-%m-%d_%H-%M")
readonly LOG_FILE_SEQ="fib-$now-seq.log"
readonly LOG_FILE_PAR="fib-$now-par.log"
readonly collect_seq="tee -a $LOG_FILE_SEQ"
readonly collect_par="tee -a $LOG_FILE_PAR"

readonly WIDTH=1000
readonly DEPTH=30
readonly REPETITIONS=9

echo "Compiling ..."
pmlc -o fib fib.pml

if [ $? -gt 0 ]; then
	exit $?
fi

echo "Done."

echo "Benchmarking sequential ..."
for ((i=1; i<=REPETITIONS; i++)); do
	echo "Repetition $i/$REPETITIONS ..."
	{ time ./fib $WIDTH $DEPTH 1 ; } 2>&1 | head -2 | tail -1 | $collect_seq
done

echo "Benchmarking parallel ..."
for ((i=1; i<=REPETITIONS; i++)); do
	echo "Repetition $i/$REPETITIONS ..."
	{ time ./fib $WIDTH $DEPTH 2 ; } 2>&1 | head -2 | tail -1 | $collect_par
done
