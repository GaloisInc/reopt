#!/bin/bash
set -e

# make sure graphmod and dot are in the PATH
if ! type -p graphmod || ! type -p dot; then
    echo "Error: cannot find 'graphmod' and/or 'dot' in PATH" 1>&2
    exit 1
fi

dir="src"
name="reopt"
FILES="$(find src -name '*.hs') $(find reopt -name '*.hs')"
echo "Writing graphmod file to $name.svg"
graphmod -i src -i reopt -p --no-cluster $FILES -q | dot -Tsvg > reopt.svg

graphmod -i src -i reopt -p $FILES -q | dot -Tsvg > reopt_cluster.svg
