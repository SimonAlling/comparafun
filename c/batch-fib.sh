***REMOVED***

. ../config.sh

readonly now=$(date +"%Y-%m-%d_%H-%M")
readonly LOG_FILE_SEQ="fib-$now-seq.log"

collect_seq() {
	head -2 | tail -1 | tee -a $LOG_FILE_SEQ
}

echo "Compiling ..."
gcc -o fib -O3 -std=c99 fib.c
if [ $? -gt 0 ]; then exit $?; fi
echo "Done."

echo "Benchmarking ..."
for ((i=1; i<=REPETITIONS; i++)); do
	echo "Repetition $i/$REPETITIONS ..."
	{ time ./fib $FIB_WIDTH $FIB_DEPTH ; } 2>&1 | collect_seq
done
sort -o $LOG_FILE_SEQ $LOG_FILE_SEQ

