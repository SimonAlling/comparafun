***REMOVED***

readonly now=$(date +"%Y-%m-%d_%H-%M")
readonly LOG_FILE="fib-$now.log"
readonly collect="tee -a $LOG_FILE"

readonly REPETITIONS=9

echo "Compiling ..."
pmlc fib.pml

if [ $? -gt 0 ]; then
	exit $?
fi

echo "Done."

for ((i=1; i<=REPETITIONS; i++)); do
	echo "Repetition $i/$REPETITIONS ..."
	{ time ./a.out ; } 2>&1 | head -2 | tail -1 | $collect
done
