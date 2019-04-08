***REMOVED***

readonly now=$(date +"%Y-%m-%d_%H-%M")
readonly LOG_FILE="benchmark-kmeans-$now.log"
readonly cmd="stack exec -- comparafun kmeans batch +RTS -H1G -A100M"
readonly collect="tee -a $LOG_FILE"

stack build

echo "$cmd" | $collect

echo "**** 4 HECs ****"
$cmd -N4  -RTS --output kmeans-4.html  | $collect

echo "**** 10 HECs ****"
$cmd -N10 -RTS --output kmeans-10.html | $collect

echo "**** 20 HECs ****"
$cmd -N20 -RTS --output kmeans-20.html | $collect

echo "-------- DONE" | $collect

readonly endTime=$(date +"%H:%M on %Y-%m-%d")
echo "Finished at $endTime" | $collect
