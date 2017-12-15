#!/bin/bash

set -e

out=`mktemp testfile-XXXXXX`
out_obj=${out%.S}.o

cat <<EOF > $out
    .text
    $@
EOF

gcc -x assembler -c $out
objdump --disassembler-options=intel-mnemonic -dw $out_obj | sed -n 's/^[[:space:]]*0:[[:space:]]*//p'
objdump --disassembler-options=intel-mnemonic -dw $out_obj | sed -n 's/^[[:space:]]*0:[[:space:]]*//p' | cut -f1 | xargs ConcreteDumpInstr
rm -f $out $out_obj
