#!/bin/bash

set -e

outtmp=`mktemp testfile-XXXXXX`

if $( which gobjcopy > /dev/null 2>&1 );
then
    OC=`which gobjcopy`
else
    OC=`which objcopy`
fi

out=$1
shift

cat <<EOF > $outtmp.S
    .text
    .global _main
_main:
    $@
EOF

gcc -x assembler $outtmp.S -o $outtmp
$OC -O elf64-x86-64 $outtmp $out
rm -f $outtmp.S $outtmp

