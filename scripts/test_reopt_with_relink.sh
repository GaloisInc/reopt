#!/bin/bash

set -Euo pipefail

OPT=$(which opt)
if [[ -x $OPT ]] ; then
  echo "opt was found in the PATH!"
else
  echo "opt was not found in the PATH!"
  exit 1
fi

if [[ -x $(which reopt) ]] ; then
  REOPT=$(which reopt)
else
  REOPT="cabal run exe:reopt --"
fi

NUM_BINARIES=0 # How many binaries were examined
NUM_BAD=0 # How many binaries were we unable to run pre-reopt
NUM_FAIL=0 # How many binaries behaved differently after reopt
NUM_PASS=0 # How many binaries

run_reopt () {
  echo "Running reopt on $1"
  NUM_BINARIES=$(($NUM_BINARIES+1))
  $1
  PRE_RESULT=$?
  # 1. If you want no output from reopt:
  # $REOPT $1 --lib-dir=lib64 --debug-dir=debug-lib64 --output=$1.reopt 1>/dev/null 2>/dev/null
  # 2. If you want to pipe into less:
  $REOPT $1 --lib-dir=lib64 --debug-dir=debug-lib64 --output=$1.reopt 2>&1
  if [[ $? -ne 0 ]]; then
    echo "Reopt terminated with a non-zero exit code on $1"
    NUM_BAD=$(($NUM_BAD+1))
  else
    $1.reopt
    POST_RESULT=$?
    if [[ $PRE_RESULT -eq $POST_RESULT ]]; then
      NUM_PASS=$(($NUM_PASS+1))
    else
      echo "reopt has caused $1 to behave differently after relinking"
      NUM_FAIL=$(($NUM_FAIL+1))
    fi
  fi
}

report_results() {
  echo "reopt was run on $NUM_BINARIES binaries."
  echo "$NUM_PASS binaries behaved identically after reopt ran."
  echo "$NUM_FAIL binaries behaved differently after reopt ran."
  if [[ $NUM_BAD -ne 0 ]]; then
    echo "$NUM_BAD binaries caused reopt to fail."
  fi
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
  report_results
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
