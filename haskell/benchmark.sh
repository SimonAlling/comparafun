#!/usr/bin/env bash
readonly MainFib="stack exec -- comparafun-fib"
readonly INT_SIZE=30
readonly MainKMeans="stack exec -- comparafun-kmeans"
readonly K=8
readonly MainSort="stack exec -- comparafun-sort"
readonly N=500000
readonly RTS="-H1G -A100M"

set -x

stack build

function benchmarkFib() {
	$MainFib $INT_SIZE fibonaccis_par +RTS $RTS -N1
	$MainFib $INT_SIZE fibonaccis_par +RTS $RTS -N4
}

function benchmarkKMeans() {
	$MainKMeans $K kmeans +RTS $RTS -N1
	$MainKMeans $K kmeans +RTS $RTS -N4
}

function benchmarkSort() {
	$MainSort $N ghc_sort +RTS $RTS -N1
	$MainSort $N sort +RTS $RTS -N1
	$MainSort $N sort +RTS $RTS -N4
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
"sort")
	benchmarkSort ;;
"")
	benchmarkAll ;;
esac
