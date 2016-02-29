#!/bin/bash

# Run 'reopt' on a binary and copy LLVM ASM for discovered functions
# into './llvm-latest'.

err () { echo "$@" > /dev/stderr; }

if [ $# != 1 ]; then
  err "usage: $0 BINARY_TO_REOPTIMIZE"
  err ""
  err "E.g. $0 examples/hello_world/hello_world_ubuntu_64_lts_12_04_musl."
  exit 2
fi

if ! which reopt &> /dev/null; then
  err "Can't find 'reopt'! Try running"
  err ""
  err "    stack exec -- $0 $@"
  err ""
  err "instead."
  exit 1
fi

export PATH=/usr/local/opt/llvm/bin:$PATH
TIME=`date +%Y-%m-%d-%H:%M:%S`

DIR=out/llvm-out-$TIME
LOG=$DIR/log.txt

mkdir -p $DIR
((reopt -D=urgent,recover,funargs -l $DIR $@ 2>&1) &&
 (cd $DIR; for i in *.ll; do ( llc -O2 $i > $i.out 2>&1 ; if [ $? -eq 0 ]; then rm $i.out; else echo $i; fi ) ; done)) \
  | tee $LOG

rm -f latest.txt llvm-latest
ln -s $LOG latest.txt
ln -s $DIR llvm-latest

