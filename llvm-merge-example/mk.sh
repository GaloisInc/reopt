#!/usr/local/bin/bash

set -e

syms=("w_str" "r_str" "root" "fopen" "fclose" "fputc" "fgetc" "fputs" "strcmp" "exit" "malloc" "feof" "realloc" "free")

function get_addr ()
{
    nm --defined-only host-tree | grep -E "\<$1\>" | cut -d' ' -f1
}

function make_host_cflags () {
    local cflags="-O3"
    for i in ${syms[*]}; do
        local name=$(echo ${i} | tr 'a-z' 'A-Z')
        cflags="$cflags -D$name=$i"
    done
    echo $cflags
}    


host_cflags=$(make_host_cflags)
make CFLAGS="${host_cflags}"
mv tree host-tree
mv tree.ll host-tree.ll


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
make CFLAGS="${cflags}"

