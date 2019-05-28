***REMOVED***

. ../config.sh

readonly now=$(date +"%Y-%m-%d_%H-%M")
readonly LOG_FILE_SEQ="fac-$now-seq.log"
readonly LOG_FILE_PAR="fac-$now-par.log"

collect_seq() {
	head -2 | tail -1 | tee -a $LOG_FILE_SEQ
}

collect_par() {
	head -2 | tail -1 | tee -a $LOG_FILE_PAR
}

echo "Compiling ..."
pmlc -o fac fac.pml
if [ $? -gt 0 ]; then exit $?; fi
echo "Done."

echo "Benchmarking sequential ..."
for ((i=1; i<=REPETITIONS; i++)); do
	echo "Repetition $i/$REPETITIONS ..."
	{ time ./fac $FAC_WIDTH $FAC_DEPTH 1 ; } 2>&1 | collect_seq
done
sort -o $LOG_FILE_SEQ $LOG_FILE_SEQ

echo "Benchmarking parallel ..."
for ((i=1; i<=REPETITIONS; i++)); do
	echo "Repetition $i/$REPETITIONS ..."
	{ time ./fac $FAC_WIDTH $FAC_DEPTH 2 ; } 2>&1 | collect_par
done
sort -o $LOG_FILE_PAR $LOG_FILE_PAR
