#! /bin/bash

# Install reopt in a Cabal sandbox.

PRIVATE_GITHUB_REPOS=(elf flexdis86 parameterized-utils linux-ptrace posix-waitpid llvm-pretty fuzz64)
cd "$(dirname "${BASH_SOURCE[0]}")"
sandbox=$(pwd)/sandbox

# Check if 'stack' is in the path
if  type stack >/dev/null 2>&1; then
    echo "Found stack"
else
    echo >&2 "I require 'stack' but it's not installed. Aborting."
    echo >&2 "Stack available at: https://github.com/commercialhaskell/stack/wiki/Downloads"
    exit 1
fi

while getopts "ps" opt; do
  case $opt in
    p)
      dopull="true"
      ;;
    s)
      dosetup="true"
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
  esac
done

mkdir -p deps
for dep in "${PRIVATE_GITHUB_REPOS[@]}"; do
  if ! [ -e deps/"$dep" ]; then
    git clone git@github.com:GaloisInc/"$dep".git deps/"$dep"
  elif [ "$dopull" == true ]; then
    echo ">> Pulling repo $dep"
    (cd deps/"$dep" && git pull)
  fi
done

if [ "$dosetup" == true ]; then
  stack setup
fi

echo "Building"
stack build
