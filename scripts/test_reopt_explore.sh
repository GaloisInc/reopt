#!/bin/bash

set -Eeuo pipefail

if [[ -x $(which reopt-explore) ]] ; then
  REOPT_EXLPORE=$(which reopt-explore)
else
  REOPT_EXLPORE="cabal run exe:reopt-explore --"
fi

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

CENTOS7_DEV_DIR="$DIR/../deps/reopt-benchmark-binaries/centos7-dev/"


if [[ $# -lt 1 ]];
then
    echo "Need a command, one of 'all', 'small', or 'all-debug-info'."
    exit 1
fi

COMMAND="$1"
shift

/bin/bash $DIR/unpack_benchmarks.sh

case $COMMAND in
    all)
      pushd $CENTOS7_DEV_DIR
      $REOPT_EXLPORE --lib-dir=lib64 --debug-dir=debug-lib64 llvm \
		     --export-summary=centos7-bin-summary.txt \
		     --export-log=centos7-bin-casts.csv \
		     bin
      popd > /dev/null # $CENTOS7_DEV_DIR
      ;;
    # all-debug-info)
    #   pushd $CENTOS7_DEV_DIR
    #   $REOPT_EXLPORE --debug-info debug-lib64
    #   popd > /dev/null # $CENTOS7_DEV_DIR
    #   ;;
    small)
      pushd $CENTOS7_DEV_DIR
      rm -fr test-bins
      mkdir test-bins
      cp bin/mkdir test-bins/mkdir
      cp bin/curl test-bins/curl
      cp bin/date test-bins/date
      $REOPT_EXLPORE --lib-dir=lib64 --debug-dir=debug-lib64 llvm test-bins/mkdir # --export-summary=centos7-small-bin-mkdir-summary.txt
      $REOPT_EXLPORE --lib-dir=lib64 --debug-dir=debug-lib64 llvm test-bins # --export-summary=centos7-small-bin-summary.txt
      popd > /dev/null # $CENTOS7_DEV_DIR
      ;;
    *) echo "unknown command - use 'all', 'small', or 'all-debug-info'"; exit 1;;
esac
