#!/bin/bash

set -e

tar_file="deps/reopt-benchmark-binaries/centos7-dev-bin.tar"
lzma_file="$tar_file.lzma"

if [[ "$lzma_file" -nt "$tar_file"  ]] ; then
  unlzma -k "$lzma_file"
fi

mkdir -p deps/reopt-benchmark-binaries/centos7-dev
pushd deps/reopt-benchmark-binaries/centos7-dev
echo "Unpacking"
tar -xf "../centos7-dev-bin.tar"
# This seems to be a tar-inside-tar
tar -xf "centos7-dev-bin.tar"
cabal run exe:reopt-explore -- ./bin --export-summary=centos7-bin-summary.txt
popd