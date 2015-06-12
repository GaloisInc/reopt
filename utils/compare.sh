#! /bin/bash

tempobjdumps=`mktemp --tmpdir=.`
tempobjdumpc=`mktemp --tmpdir=.`
tempreopts=`mktemp --tmpdir=.`
tempreoptc=`mktemp --tmpdir=.`
echo $1
objdump -d -M intel $1 > $tempobjdumps
./cleaner.py $tempobjdumps > $tempobjdumpc
../.cabal-sandbox/bin/reopt -d $1 > $tempreopts
./cleaner.py $tempreopts > $tempreoptc
diff $tempobjdumpc $tempreoptc
