***REMOVED***

# Run this file from an immediate subdirectory.

readonly EXTRACT="Extract"
readonly EXTRACT_HS="$EXTRACT.hs"
readonly EXTRACT_EXE="../$EXTRACT"

if [ ! -e $EXTRACT ]; then
	cd ..
	ghc -O2 "$EXTRACT_HS"
	cd -
fi

for i in $(ls -1v *.log); do
	cat "$i" | $EXTRACT_EXE
done
