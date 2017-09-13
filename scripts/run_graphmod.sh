#!/bin/bash
set -e

# make sure graphmod and dot are in the PATH
if ! type -p graphmod; then
    echo "Error: cannot find Haskell 'graphmod' in PATH" 1>&2
    echo "It can be installed via cabal or stack." 1>&2
    exit 1
fi

if ! type -p dot; then
    echo "Error: cannot find 'dot' in PATH" 1>&2
    echo "It can be installed via 'brew install graphviz' or your system package manger." 1>&2
    exit 1
fi

dir="src"
name="reopt"
FILES="$(find src -name '*.hs') $(find reopt -name '*.hs')"
echo "Writing graphmod file to $name.svg"
graphmod -i src -i reopt -p --no-cluster $FILES -q | dot -Tsvg > reopt.svg

graphmod -i src -i reopt -p $FILES -q | dot -Tsvg > reopt_cluster.svg
