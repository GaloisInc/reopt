#!/bin/bash

set -e

CENTOS7_DEV_DIR="centos7-dev"


unlzma_mv_untar () {
  echo -n "Unpacking and extracting contents of $1.lzma to $2..."
  if [[ "$1.lzma" -nt "$1"  ]] ; then
    unlzma -k "$1.lzma"
  fi
  mkdir -p $2
  if [[ -e "$1" ]] ; then
    mv $1 $2
  fi
  pushd $2 > /dev/null
  tar -xkf $1
  popd > /dev/null # $2
  echo " done!"
}

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
BENCHMARK_DIR="$SCRIPT_DIR/../deps/reopt-benchmark-binaries"
pushd $BENCHMARK_DIR > /dev/null


CENTOS7_DEV_BIN="centos7-dev-bin.tar"
CENTOS7_DEV_LIB64="centos7-dev-lib64.tar"
CENTOS7_DEV_DEBUG_LIB64="centos7-dev-debug-lib64.tar"

echo "Decompressing and unpacking centos7-dev files..."
unlzma_mv_untar $CENTOS7_DEV_BIN $CENTOS7_DEV_DIR
unlzma_mv_untar $CENTOS7_DEV_LIB64 $CENTOS7_DEV_DIR
unlzma_mv_untar $CENTOS7_DEV_DEBUG_LIB64 $CENTOS7_DEV_DIR


# echo "Exploring centos7-dev binaries..."
# pushd $CENTOS7_DEV_DIR
# cabal run exe:reopt-explore -- ./bin --export-summary=centos7-bin-summary.txt
# popd # $CENTOS7_DEV_DIR

popd > /dev/null # $BENCHMARK_DIR 
