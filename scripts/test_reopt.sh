#!/bin/bash

set -Eeuox pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

CENTOS7_DEV_DIR="$DIR/../deps/reopt-benchmark-binaries/centos7-dev/"

/bin/bash $DIR/unpack_benchmarks.sh

if [[ $# -lt 1 ]]; then
  echo "No binaries specified to test reopt against: running small default set of reopt tests..."
  pushd $CENTOS7_DEV_DIR
  cabal run exe:reopt -- -c centos7-dev-bin/mkdir --lib-dir=centos7-dev-lib64 --debug-dir=centos7-dev-debug-lib64
  cabal run exe:reopt -- -c centos7-dev-bin/curl --lib-dir=centos7-dev-lib64 --debug-dir=centos7-dev-debug-lib64
  cabal run exe:reopt -- -c centos7-dev-bin/date --lib-dir=centos7-dev-lib64 --debug-dir=centos7-dev-debug-lib64
  popd > /dev/null # $CENTOS7_DEV_DIR
else
  pushd $CENTOS7_DEV_DIR
  for arg in "$@"
  do
    if [[ -x "centos7-dev-bin/$arg" ]]; then
      cabal run exe:reopt -- -c centos7-dev-bin/$arg --lib-dir=centos7-dev-lib64 --debug-dir=centos7-dev-debug-lib64
    else
      echo "Could not find binary $arg in centos7-dev-bin"
      exit 1
    fi
  done
fi
