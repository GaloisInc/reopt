#!/bin/bash

set -xe

CENTOS7_DEV_DIR="centos7-dev"


unlzma_mv_untar () {
  TAR_FILE=$1
  DIR=$2

  pushd $DIR
  echo -n "Unpacking and extracting contents of $TAR_FILE.lzma in $DIR..."
  if [[ "$TAR_FILE.lzma" -nt "$TAR_FILE"  ]] ; then
    unlzma -k "$TAR_FILE.lzma"
  fi
  TARVER=`tar --version | head -n 1`
  if [[ "$TARVER" =~ "tar (GNU tar)" ]]; then
    tar -xf $TAR_FILE
  elif [[ "$TARVER" =~ "bsdtar" ]]; then
    tar -xkf $TAR_FILE
  else
    echo "Unknown version of tar, please report!"
    exit 1
  fi
  echo " done!"
  popd # $DIR
}

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
BENCHMARK_DIR="$SCRIPT_DIR/../deps/reopt-benchmark-binaries"
pushd $BENCHMARK_DIR > /dev/null


echo "Decompressing and unpacking centos7-dev files..."
unlzma_mv_untar "bin.tar" "centos7-dev/bin"
unlzma_mv_untar "lib64.tar" "centos7-dev/lib64"
unlzma_mv_untar "debug-lib64.tar" "centos7-dev/debug-lib64"


popd > /dev/null # $BENCHMARK_DIR
