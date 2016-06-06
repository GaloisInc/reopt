#! /bin/bash
set -e

# Clone and update dependencies

PRIVATE_GITHUB_REPOS=(elf-edit flexdis86 parameterized-utils linux-ptrace posix-waitpid llvm-pretty fuzz64)
cd "$(dirname "${BASH_SOURCE[0]}")"
sandbox=$(pwd)/sandbox

mkdir -p deps
for dep in "${PRIVATE_GITHUB_REPOS[@]}"; do
  if ! [ -e deps/"$dep" ]; then
    echo "Cloning $dep"
    git clone git@github.com:GaloisInc/"$dep".git deps/"$dep"
  else
    echo "Pulling $dep"
    (cd deps/"$dep" && git pull)
  fi
done
