#!/bin/bash

set -Euo pipefail

if [ -x $(which reopt) ] ; then
  REOPT=$(which reopt)
else
  REOPT="cabal run exe:reopt --"
fi

run_reopt () {
  echo "Running reopt on $1"
  $REOPT --recover $1 --lib-dir=lib64 --debug-dir=debug-lib64
}


DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

CENTOS7_DEV_DIR="$DIR/../deps/reopt-benchmark-binaries/centos7-dev"

/bin/bash $DIR/unpack_benchmarks.sh

if [[ $# -lt 1 ]]; then
  echo "No options or binaries specified!"
  echo ""
  echo "  usage: ./test_reopt.sh [all|small|bin...+]"
  echo "    where"
  echo "          all: runs reopt against all binaries in the centos7-dev/bin directory"
  echo "        small: runs reopt against a few binaries in the centos7-dev/bin directory"
  echo "      bin...+: runs reopt against the specified binaries in deps/reopt-benchmark-binaries/centos7-dev/bin"
elif [[ $1 == "small" ]]; then
  echo "Running small default set of reopt tests..."
  pushd $CENTOS7_DEV_DIR
  run_reopt bin/mkdir
  run_reopt bin/curl
  run_reopt bin/date
  popd > /dev/null # $CENTOS7_DEV_DIR
elif [[ $1 == "all" ]]; then
  echo "Running reopt on all centos7-dev binaries..."
  pushd $CENTOS7_DEV_DIR
  for exe in bin/*
  do
    if [[ -x "$exe" ]]; then
      run_reopt $exe
    fi
  done
else
  pushd $CENTOS7_DEV_DIR
  for exe in "$@"
  do
    if [[ -x "bin/$exe" ]]; then
      run_reopt bin/$exe
    else
      echo "Could not find binary $exe in bin"
      exit 1
    fi
  done
fi
