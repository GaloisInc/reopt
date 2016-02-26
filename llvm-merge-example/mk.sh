#!/bin/bash

set -e

syms=("w_str" "r_str" "root" "fopen" "fclose" "fputc" "fgetc" "fputs" "strcmp" "exit" "malloc" "feof" "realloc" "free")

function get_addr ()
{
    nm --defined-only original-tree | grep -E "\<$1\>" | cut -d' ' -f1
}

function make_original_cflags () {
    local cflags="-O3"
    for i in ${syms[*]}; do
        local name=$(echo ${i} | tr 'a-z' 'A-Z')
        cflags="$cflags -D$name=$i"
    done
    echo $cflags
}


original_cflags=$(make_original_cflags)
echo make CFLAGS="${original_cflags}"
make CFLAGS="${original_cflags}"
mv tree original-tree
mv tree.ll original-tree.ll


function make_cflags () {
    local cflags="-O3"
    for i in ${syms[*]}; do
        local v=$(get_addr $i)
        local name=$(echo ${i} | tr 'a-z' 'A-Z')
        cflags="$cflags -D$name='(*((typeof($i) *) (0x$v)))'"
    done
    echo $cflags
}

cflags=$(make_cflags)
echo make CFLAGS="${cflags}"
make CFLAGS="${cflags}"
