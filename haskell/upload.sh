#!/usr/bin/env bash

set -x

readonly remote="alsimon@remote12.chalmers.se:comparafun/haskell"

scp -r app src test *.sh *.cabal *.yaml *.hs "$remote"
