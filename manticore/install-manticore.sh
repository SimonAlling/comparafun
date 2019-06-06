#!/usr/bin/env bash

set -e

echo "This script is intended for installation of Manticore on NixOS."
echo "http://manticore.cs.uchicago.edu/install.html"
echo "Assumed in path:"
echo "  git"
echo "  autoheader"
echo "  autoconf"
echo "  make"
echo "  patchShebangs"

echo

#git clone https://github.com/ManticoreProject/manticore

cd manticore

#echo "Patching shebangs in scripts ..."

#patchShebangs .

echo "Commencing Manticore installation ..."

autoheader -Iconfig

autoconf -Iconfig

./configure

make build

make local-install

echo "Running regression tests ..."

cd src/regression-tests
bash-scripts/run-seq.bsh
bash-scripts/run-par.bsh

echo "Done!"

