#!/usr/bin/env bash

set -x

readonly remote="alsimon@remote12.chalmers.se:comparafun/manticore"

scp -r *.pml *.sh "$remote"
