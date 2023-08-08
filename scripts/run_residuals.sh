#! /usr/bin/env bash
set -euv

# Building first, so that if build is needed, the output does not bother us
cabal build reopt:exe:reopt-explore

cabal run -v0 reopt:exe:reopt-explore -- \
    --debug-dir ./deps/reopt-benchmark-binaries/centos7-dev/debug-lib64 \
    residuals \
    --output-for-spreadsheet \
    --header ./scratch/shed.h \
    ${*} 2>&1 | less -R
