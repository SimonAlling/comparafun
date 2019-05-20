#!/usr/bin/env bash

# Run this file from an immediate subdirectory.

readonly EXTRACT="Extract"
readonly EXTRACT_HS="$EXTRACT.hs"
readonly EXTRACT_EXE="../$EXTRACT"

if [ ! -e $EXTRACT ]; then
	cd ..
	ghc -O2 "$EXTRACT_HS"
	cd -
fi

cat $(ls -1v *.log) | $EXTRACT_EXE $1
