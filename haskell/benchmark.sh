***REMOVED***
readonly MainFib="stack exec -- comparafun-fib"
readonly INT_SIZE=30
readonly MainKMeans="stack exec -- comparafun-kmeans"
readonly K=8
readonly RTS="-H1G -A100M"

***REMOVED***

stack build

function benchmarkFib() {
	$MainFib $INT_SIZE fibonaccis_par +RTS $RTS -N1
	$MainFib $INT_SIZE fibonaccis_par +RTS $RTS -N4
}

function benchmarkKMeans() {
	$MainKMeans $K kmeans +RTS $RTS -N1
	$MainKMeans $K kmeans +RTS $RTS -N4
}

function benchmarkAll() {
	benchmarkFib
	benchmarkKMeans
}

case $1 in
"fib")
	benchmarkFib ;;
"kmeans")
	benchmarkKMeans ;;
"")
	benchmarkAll ;;
esac
