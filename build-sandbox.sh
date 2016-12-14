#! /bin/bash

# Install reopt in a Cabal sandbox.

PRIVATE_GITHUB_REPOS=(elf-edit flexdis86 parameterized-utils linux-ptrace posix-waitpid llvm-pretty fuzz64)
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

update_repo()
{
    local repo=$1
    local dir=$2
    if ! [ -e deps/"$dir" ]; then
        git clone $repo deps/"$dir"
    elif [ "$dopull" == true ]; then
        echo ">> Pulling repo $dir"
        (cd deps/"$dir" && git pull)
    fi
}

mkdir -p deps
for dep in "${PRIVATE_GITHUB_REPOS[@]}"; do
  update_repo "git@github.com:GaloisInc/$dep.git" "$dep"
done

update_repo "git@gitlab-ext.galois.com:macaw/macaw.git" "$dep"

if [ "$dosetup" == true ]; then
  stack setup
fi

echo "Building"
stack build
