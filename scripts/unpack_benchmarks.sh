#!/bin/bash

set -xe

CENTOS7_DEV_DIR="centos7-dev"


unlzma_mv_untar () {
  TAR_FILE=$1
  DEST=$2
  echo -n "Unpacking and extracting contents of $TAR_FILE.lzma to $DEST..."
  if [[ "$TAR_FILE.lzma" -nt "$TAR_FILE"  ]] ; then
    unlzma -k "$TAR_FILE.lzma"
  fi
  mkdir -p $DEST
  if [[ -e "$TAR_FILE" ]] ; then
    mv $TAR_FILE $DEST
    pushd $DEST > /dev/null
    if [[ "$OSTYPE" == "darwin"* ]]; then
      tar -xkf $TAR_FILE
    else
      tar --overwrite -xf $TAR_FILE
    fi
    popd > /dev/null # $DEST
  fi
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


popd > /dev/null # $BENCHMARK_DIR 
