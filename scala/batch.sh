***REMOVED***

. ../config.sh

readonly NAMES=("fib" "kmeans" "fac")

readonly name=$1

if [[ ! " ${NAMES[@]} " =~ " ${name} " ]]; then
	echo "Unknown benchmark. Choose one of these:"
	echo "${NAMES[*]}"
	exit 1
fi

readonly now=$(date +"%Y-%m-%d_%H-%M")
readonly LOG_FILE="$name-$now.log"

collect() {
	tee -a $LOG_FILE
}

if [[ "$name" == "kmeans" ]]; then
	cd ../haskell
	./testdata.sh
	cd -
fi

sbt "runMain $name""Benchmark" | collect
