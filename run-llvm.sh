#!/bin/bash


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

