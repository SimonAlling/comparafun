***REMOVED***
readonly INT_SIZE=30
readonly Main="stack exec comparafun-exe"
readonly RTS="-H1G -A100M"

***REMOVED***

stack build

$Main $INT_SIZE fibonaccis_par +RTS $RTS -N1

$Main $INT_SIZE fibonaccis_par +RTS $RTS -N4
