***REMOVED***

readonly now=$(date +"%Y-%m-%d_%H-%M")
readonly LOG_FILE="benchmark-$now.log"
readonly cmd="stack exec -- comparafun-kmeans batch kmeans +RTS -H1G -A100M"
readonly collect="tee -a $LOG_FILE"

stack build

echo "**** 4 HECs ****"
$cmd -N4 | $collect

echo "**** 8 HECs ****"
$cmd -N8 | $collect

echo "**** 16 HECs ****"
$cmd -N16 | $collect

echo "Done!"
