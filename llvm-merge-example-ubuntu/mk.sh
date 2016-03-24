#!/bin/bash

set -e

external_syms=("fopen" "fclose" "fputc" "fgetc" "fputs" "strcmp" "exit" "malloc" "feof" "realloc" "free")
internal_syms=("w_str" "r_str" "root" "print_tree" "tree_equal" "really_write_tree" "write_tree"
  "read_tree_payload" "really_read_tree" "read_tree")

syms=("${external_syms[@]}" "${internal_syms[@]}")

function upcase () {
  echo $1 | tr 'a-z' 'A-Z'
}

function make_original_cflags () {
    #local cflags="-O3"
    local cflags=""
    for i in ${syms[*]}; do
        local name=$(echo ${i} | tr 'a-z' 'A-Z')
        cflags="$cflags -D$name=$i"
    done
    echo $cflags
}

function make_original () {
  cflags=$(make_original_cflags)
  musl-gcc $cflags -static -o tmp/tree tree.c data.h
}

function reopt_original () {
  stack exec -- ../run-llvm.sh tmp/tree
}

# Point '<NAME>' to 'F<addr>' whenever 'llvm-latest/<name>_<addr>.s'
# can be found. The '.s' file will exist if the corresponding '.ll'
# file can be processed by 'llc', which usually works, except for some
# '.ll' files with type errors due to us not distinguishing between
# float and int types in our LLVM generation. This only comes up with
# the 'cvtsi2ss' translation, which hopefully only comes up in files
# we don't want to use anyway (i.e. 'malloc' and 'free'; but it also
# comes up in 'alloc_fwd', and i'm not sure what that is).
#
# The current implementation is too simplistic though:
#
# - it doesn't take transitive dependencies into account. They're
#   declared in the '.ll' files though, so we could learn them that
#   way.
#
# - in case the '.s' file is not found, we need to redirect the
# - 'F<addr>' back to the original '<name>', e.g. using 'asm_redirect'
# - below.
function make_frankenstein_cflags () {
  local cflags=""
  for i in ${syms[*]}; do
    local maybe_addr=$(make_frankenstein_sfiles | grep '^.*/'"$i"'_[0-9a-f]*\.s$' | \
            sed -nre 's|^.*_([0-9a-f]*)\.s$|\1|p')
    if [[ -n "$maybe_addr" ]]; then
      local v=F$maybe_addr
    else
      local v=$i
    fi
    local name=$(echo ${i} | tr 'a-z' 'A-Z')
    cflags="$cflags -D$name=$v"
  done
  echo "$cflags"
}

function make_frankenstein_sfiles () {
  for i in ${syms[*]}; do
    find llvm-latest/ -regextype posix-egrep -regex ".*/${i}_[0-9a-f]*.s"
  done
}

LIB_REOPT=../libreopt/build
lib_reopt_flags="-lreopt -L$LIB_REOPT"
function make_frankenstein_automatic () {
  local cflags=$(make_frankenstein_cflags)
  local sfiles=$(make_frankenstein_sfiles)
  #echo "sfiles='$sfiles'"
  set -x
  musl-gcc $cflags -static -o tmp/frankentree tree.c data.h $sfiles $lib_reopt_flags
}

# Create default defintions '-DNAME=name' for all syms, but skipping
# the syms passed as arguments.
function default_cflags () {
  local skip=("$@")
  local cflags=""
  for s in "${syms[@]}"; do
    local skipped=false
    for t in "${skip[@]}"; do
      if [[ "$s" = "$t" ]]; then
        skipped=true
        break
      fi
    done
    if [[ "$skipped" = false ]]; then
      cflags="$cflags -D$(upcase "$s")=$s" >&2
    fi
  done
  echo $cflags
}

# Redirect a function in ASM.
#
# E.g., 'asm_redirect F4022c0 strcmp' generates an ASM file which
# defines 'F4022c0' as a call to 'strcmp'. Returns the filename of
# containing generated ASM.
#
# Unfortunately, defining the redirection in assembly leads to
# "implicit declaration" warnings in the calling C code. But the only
# alternative I know is to give 'extern' decls, or do the redirection
# in 'C' directly, but in both cases I need to write down the types of
# the redirected functions, which is annoying. Could use the LLVM
# types to learn approximate C types, but that might cause other
# warnings.
function asm_redirect () {
  from=$1
  to=$2
  out=tmp/redirect_${from}_to_${to}.s
  cat <<EOF > $out
  .text
  .globl $from
$from:
  jmp $to
EOF
  echo $out
}

# Use reopted 'tree_equal', which transitively uses reopted 'strcmp'.
function make_frankenstein_manual_1 () {
  local cflags=$(default_cflags tree_equal)
  local sfiles=""
  cflags="$cflags -D_DEBUG -DTREE_EQUAL=F4002a0"
  # Here reopted 'tree_equal' depends on reopted 'strcmp'.
  local sfiles="llvm-latest/tree_equal_4002a0.s llvm-latest/strcmp_4022c0.s"
  set -x
  musl-gcc $cflags -static -o tmp/frankentree tree.c data.h $sfiles $lib_reopt_flags
}

# Use reopted 'tree_equal', but replace reopted 'strcmp' --
# i.e. 'F4022c0' -- with 'strcmp' from 'libc'.
function make_frankenstein_manual () {
  local cflags=$(default_cflags tree_equal)
  cflags="$cflags -D_DEBUG -DTREE_EQUAL=F4002a0"
  # Here reopted 'tree_equal' depends on reopted 'strcmp', but we
  # redirect reopted 'strcmp' back to libc 'strcmp'.
  local strcmp_out=$(asm_redirect F4022c0 strcmp)
  local sfiles="llvm-latest/tree_equal_4002a0.s $strcmp_out"
  set -x
  musl-gcc $cflags -static -o tmp/frankentree tree.c data.h $sfiles $lib_reopt_flags
}

function main () {
  #make_original
  #reopt_original
  #make_frankenstein_sfiles
  #make_frankenstein_cflags
  #make_frankenstein
  #make_frankenstein_manual_1
  make_frankenstein_manual
}
main

################################################################
# Scratch

function find_missing_s_files () {
# Find .s files for missing symbols when gcc'ing in make_frankenstein.
cat /tmp/missing | while read a; do echo -n "LOOKING FOR $a: "; f=$(find llvm-latest/ -name "*$a*.s"); if [[ -n "$f" ]]; then echo $f; else echo -n ".S NOT FOUND: "; find llvm-latest/ -name "*$a*.out" ; echo; fi; done
}
