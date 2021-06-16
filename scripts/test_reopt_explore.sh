#!/bin/bash

set -Eeuox pipefail

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
      cabal run exe:reopt-explore -- centos7-dev-bin --export-summary=centos7-bin-summary.txt \
        --lib-dir=centos7-dev-lib64 \
        --debug-dir=centos7-dev-debug-lib64
      popd > /dev/null # $CENTOS7_DEV_DIR
      ;;
    all-debug-info)
      pushd $CENTOS7_DEV_DIR
      cabal run exe:reopt-explore -- --debug-info centos7-dev-debug-lib64
      popd > /dev/null # $CENTOS7_DEV_DIR
      ;;
    small)
      pushd $CENTOS7_DEV_DIR
      rm -fr test-bins
      mkdir test-bins
      cp centos7-dev-bin/mkdir test-bins/mkdir
      cp centos7-dev-bin/curl test-bins/curl
      cp centos7-dev-bin/date test-bins/date
      cabal run exe:reopt-explore -- test-bins/mkdir --lib-dir=centos7-dev-lib64 --debug-dir=centos7-dev-debug-lib64
      cabal run exe:reopt-explore -- test-bins --lib-dir=centos7-dev-lib64 --debug-dir=centos7-dev-debug-lib64
      popd > /dev/null # $CENTOS7_DEV_DIR
      ;;
    *) echo "unknown command - use 'all', 'small', or 'all-debug-info'"; exit 1;;
esac
