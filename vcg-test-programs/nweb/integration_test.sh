#!/bin/bash

reopt nweb23.exe --header=nweb.h --export-llvm=nweb23.ll --annotations=nweb23.ann
reopt-vcg nweb23.ann
if [[ 0 -eq $? ]]; then
  echo "reopt-vcg succeeded!"
  exit 0
elif [[ 1 -eq $? ]]; then
  # exit code of 1 means some verification conditions
  # failed to verify but no other errors were encountered
  echo "reopt-vcg succeeded in generating verification conditions, but some were not verified."
  exit 0
else
  echo "reopt-vcg encountered errors during verification condition generation and/or during verification."
  exit 1
fi
