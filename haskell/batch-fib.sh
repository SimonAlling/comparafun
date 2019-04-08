***REMOVED***

readonly now=$(date +"%Y-%m-%d_%H-%M")
readonly LOG_FILE="benchmark-fib-$now.log"
readonly cmd="stack exec -- comparafun fib batch +RTS -H1G -A100M"
readonly collect="tee -a $LOG_FILE"

stack build

echo "$cmd" | $collect

echo "**** 4 HECs ****"
$cmd -N4  -RTS --output fib-4.html | $collect

echo "**** 8 HECs ****"
$cmd -N8  -RTS --output fib-8.html | $collect

echo "**** 12 HECs ****"
$cmd -N12 -RTS --output fib-12.html | $collect

echo "**** 16 HECs ****"
$cmd -N16 -RTS --output fib-16.html | $collect

echo "**** 20 HECs ****"
$cmd -N20 -RTS --output fib-20.html | $collect

echo "-------- DONE" | $collect

readonly endTime=$(date +"%H:%M on %Y-%m-%d")
echo "Finished at $endTime" | $collect
