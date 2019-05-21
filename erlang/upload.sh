#!/usr/bin/env bash

set -x

readonly remote="alsimon@remote12.chalmers.se:comparafun/erlang"

scp -r *.sh *.erl *.testdata "$remote"
