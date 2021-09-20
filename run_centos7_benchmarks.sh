#!/bin/bash

set -Eeuox pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

BENCHMARK_DIR="$SCRIPT_DIR/benchmark-binaries"
CENTOS7_DEV_DIR="$BENCHMARK_DIR/centos7-dev/"


unlzma_mv_untar () {
  TAR_FILE=$1
  DIR=$2

  pushd $DIR
  echo -n "Unpacking and extracting contents of $TAR_FILE.lzma in $DIR..."
  if [[ "$TAR_FILE.lzma" -nt "$TAR_FILE"  ]] ; then
    unlzma -k "$TAR_FILE.lzma"
  fi
  if [[ "$OSTYPE" == "darwin"* ]]; then
    tar -xkf $TAR_FILE
  else
    tar --overwrite -xf $TAR_FILE
  fi
  echo " done!"
  popd # $DIR
}

pushd $BENCHMARK_DIR > /dev/null

echo "Decompressing and unpacking centos7-dev files..."
unlzma_mv_untar "bin.tar" "$CENTOS7_DEV_DIR/bin"
unlzma_mv_untar "lib64.tar" "$CENTOS7_DEV_DIR/lib64"
unlzma_mv_untar "debug-lib64.tar" "$CENTOS7_DEV_DIR/debug-lib64"


popd > /dev/null # $BENCHMARK_DIR

pushd $CENTOS7_DEV_DIR
reopt-explore bin --export-summary=centos7-bin-summary.txt \
  --lib-dir=lib64 \
  --debug-dir=debug-lib64
popd > /dev/null # $CENTOS7_DEV_DIR
