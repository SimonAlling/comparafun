#!/usr/bin/env bash

set -x

readonly remote="alsimon@remote12.chalmers.se:comparafun"

scp -r *.nix *.sh "$remote"
