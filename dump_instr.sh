#!/bin/bash

set -e

out=`mktemp testfile-XXXXXX`
out_obj=${out%.S}.o

cat <<EOF > $out
    .text
    $@
EOF

gcc -x assembler -c $out
objdump -d $out_obj | grep '0:'
objdump --disassembler-options=intel-mnemonic -d $out_obj | grep '0:'
rm -f $out $out_obj
