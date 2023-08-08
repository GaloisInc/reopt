#! /usr/bin/env bash
set -euv

# Building first, so that if build is needed, the output does not bother us
cabal build reopt:exe:reopt

cabal run -v0 reopt:exe:reopt -- \
    --debug-dir ./deps/reopt-benchmark-binaries/centos7-dev/lib64 \
    --header ./scratch/shed.h \
    --export-llvm ./scratch/out.ll \
    ${*} 2>&1 | less -R
