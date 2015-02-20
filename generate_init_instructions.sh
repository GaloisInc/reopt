#!/bin/bash
objdump -w --disassembler-options=intel-mnemonic -d examples/nweb23_static_fresbsd \
    | cut -f 3 | sed -e 's/.*://' | cut -f1 -d ' ' | sort | uniq
