#! /bin/bash

tempobjdumps=`mktemp --tmpdir=.`
tempobjdumpc=`mktemp --tmpdir=.`
tempreopts=`mktemp --tmpdir=.`
tempreoptc=`mktemp --tmpdir=.`
tempdiff=`mktemp --tmpdir=.`
objdump -d -M intel $1 > $tempobjdumps
./cleaner.py $tempobjdumps > $tempobjdumpc
../.cabal-sandbox/bin/reopt -d $1 > $tempreopts
./cleaner.py $tempreopts > $tempreoptc
diff $tempobjdumpc $tempreoptc > $tempdiff
./diffcleaner.py $tempdiff

rm $tempobjdumps
rm $tempobjdumpc
rm $tempreopts
rm $tempreoptc
rm $tempdiff
