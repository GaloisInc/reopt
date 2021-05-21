#!/bin/bash

set -e

if [[ -f "deps/reopt-benchmark-binaries/centos7-dev-bin.tar.lzma" ]] ; then
  unlzma deps/reopt-benchmark-binaries/centos7-dev-bin.tar.lzma
fi

mkdir -p centos7-dev
pushd centos7-dev
tar -xvf ../deps/reopt-benchmark-binaries/centos7-dev-bin.tar
tar -xvf centos7-dev-bin.tar
cabal run exe:reopt-explore -- ./bin --export-summary=centos7-bin-summary.txt
popd
