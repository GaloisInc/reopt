#!/bin/bash

set -e

out=`mktemp testfile-XXXXXX`
out_obj=${out%.S}.o

cat <<EOF > $out
    .text
    $@
EOF

gcc -x assembler -c $out
objdump --disassembler-options=intel-mnemonic -dw $out_obj | sed -n 's/^[[:space:]]*[0-9a-f]*:[[:space:]]*//p'
objdump --disassembler-options=intel-mnemonic -dw $out_obj | sed -n 's/^[[:space:]]*[0-9a-f]*:[[:space:]]*//p' | cut -f1 | xargs LLVMDumpInstr
rm -f $out $out_obj
