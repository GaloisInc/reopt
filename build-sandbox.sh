#! /bin/bash -x

# Install reopt in a Cabal sandbox.

PRIVATE_GITHUB_REPOS=(elf flexdis86 parameterized-utils)
cd "$(dirname "${BASH_SOURCE[0]}")"
sandbox=$(pwd)/sandbox

while getopts "p" opt; do
  case $opt in
    p)
      dopull="true"
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
  esac
done

mkdir -p deps
if ! [ -e sandbox ]; then
  cabal sandbox init --sandbox="$sandbox"
fi
for dep in "${PRIVATE_GITHUB_REPOS[@]}"; do
  if ! [ -e deps/"$dep" ]; then  
    git clone git@github.com:GaloisInc/"$dep".git deps/"$dep"
    cabal sandbox add-source deps/"$dep"
    (cd deps/"$dep" && cabal sandbox --sandbox="$sandbox" init)
  elif [ "$dopull" == true ]; then
    (cd deps/"$dep" && git pull)
  fi
done

cabal install
