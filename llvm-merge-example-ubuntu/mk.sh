#!/bin/bash

set -e

external_syms=("fopen" "fclose" "fputc" "fgetc" "fputs" "strcmp" "exit" "malloc" "feof" "realloc" "free")
internal_syms=("print_tree" "tree_equal" "really_write_tree" "write_tree"
  "read_tree_payload" "really_read_tree" "read_tree")

syms=("${external_syms[@]}" "${internal_syms[@]}")

die () {
  echo "$1" >&2
  exit 1
}

# Get address of symbol from 'original-tree' binary. These addresses
# correspond to the 'F<addr>' names in reopted '.s' and '.ll' files.
function get_addr ()
{
  local addr=$(nm --defined-only tmp/original-tree | grep -E "\<$1\>" | cut -d' ' -f1)
  if [[ -z "$addr" ]]; then
    die "get_addr: no addr for sym '$sym'!"
  fi
  echo $addr | sed -re 's/^0+//'
}

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
  musl-gcc $cflags -static -o tmp/original-tree tree.c
}

function reopt_original () {
  stack exec -- ../run-llvm.sh tmp/original-tree
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
#   'F<addr>' back to the original '<name>', e.g. using 'asm_redirect'
#   below.
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
#
# Oh, but apparently there is 'typeof' in Gnu C! E.g. in the original
# '../llvm-merge-example/mk.sh' there is
#
#    local v=$(get_addr $i)
#    local name=$(echo ${i} | tr 'a-z' 'A-Z')
#    cflags="$cflags -D$name='(*((typeof($i) *) (0x$v)))'"
#
# and we could create a header file declaring all the externs in a
# similar way.
function asm_redirect () {
  local sym=$1
  local addr=$(get_addr $1)
  if [[ -z "$addr" ]]; then 
    die "Could not find address for sym '$sym'!"
  fi

  local from=F$addr
  local to=$sym
  local out=tmp/redirect_${from}_to_${to}.s
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
function make_frankenstein_manual_2 () {
  local cflags=$(default_cflags tree_equal)
  cflags="$cflags -D_DEBUG -DTREE_EQUAL=F4002a0"
  # Here reopted 'tree_equal' depends on reopted 'strcmp', but we
  # redirect reopted 'strcmp' back to libc 'strcmp'.
  local strcmp_out=$(asm_redirect strcmp)
  local sfiles="llvm-latest/tree_equal_4002a0.s $strcmp_out"
  set -x
  musl-gcc $cflags -static -o tmp/frankentree tree.c data.h $sfiles $lib_reopt_flags
}

# Find the 'F<addr>' reopted function for argument symbol.
function reopted_function () {
  local sym=$1
  local addr=$(get_addr $sym)
  echo "F$addr"
}

# Find the reopted '.s' file for argument symbol.
function reopted_s_file () {
  local sym=$1
  local addr=$(get_addr $sym)
  local sfile=$(find llvm-latest/ -regextype posix-egrep -regex ".*/${sym}_${addr}.s")
  if [[ -z "$sfile" ]]; then
    {
      echo "Could not find '${sym}_${addr}.s'."
      local num_s=$(find llvm-latest/ -name "*${sym}*${addr}*.s" | wc -l)
      if [[ "$num_s" -ne 1 ]]; then
        echo "Found $num_s matches for '*${sym}*${addr}*.s'; wanted exactly 1!"
        exit 1
      else
        sfile=$(find llvm-latest/ -name "*${sym}*${addr}*.s")
        echo "Using approximate match '$sfile'."
      fi
    } >&2
  fi
  echo $sfile
}

# Use as many reopted functions as possible, but redirect back to libc
# where that fails.
function make_frankenstein_manual () {
  # Syms we expect to cause trouble for reopt.
  local defaulted_syms=(malloc free realloc main __libc_start_main _start_c __libc_exit_fini)
  # Reopted syms which are not in the list '$syms', which arise as
  # transitive dependencies of those syms.
  #
  # These could be discovered automatically by looking for 'F<addr>'
  # deps in reopted '.ll' files.
  local extra_reopted_syms=(fflush fflush_unlocked fwrite strchr strchrnul strlen __fmodeflags __syscall_ret __fdopen __errno_location __lockfile __unlist_locked_file __ofl_lock __ofl_unlock __unlockfile __overflow __uflow __funcs_on_exit __stdio_exit_needed _Exit deregister_tm_clones memset __fwritex __stdio_read __stdio_write __stdio_seek __stdio_close __ofl_add __wait __lock __unlock __towrite __toread close_file memcpy __aio_close)
  local cflags=""
  local sfiles=""

  # Redirect defaulted syms to themselves, and redirect reopted
  # versions of defaulted syms back to the originals.
  #
  # We need to redirect the reopted versions as well, since if 'f'
  # called 'g', and we use reopted 'f' but original 'g', then reopted
  # 'f' calls reopted 'g' which needs to replaced with original 'g'.
  for s in ${defaulted_syms[@]}; do
    cflags="$cflags -D$(upcase $s)=$s"
    sfiles="$sfiles $(asm_redirect $s)"
  done

  # Redirect non-defaulted syms to their reopted versions.
  for s in ${syms[@]}; do
    # Check if '$s' is defaulted. This element-of check only makes
    # sense when none of the elements have spaces in them.
    if ! [[ " ${defaulted_syms[@]} " =~ " $s " ]]; then
      cflags="$cflags -D$(upcase $s)=$(reopted_function $s)"
      sfiles="$sfiles $(reopted_s_file $s)"
    fi
  done

  # Inlude '.s' files for transitive reopt dependencies.
  for s in ${extra_reopted_syms[@]}; do
    sfiles="$sfiles $(reopted_s_file $s)"
  done    

  set -x
  musl-gcc $cflags -static -ggdb -D_DEBUG -o tmp/frankentree tree.c data.h $sfiles $lib_reopt_flags
}

function main () {
  mkdir -p tmp
  make_original
  reopt_original
  #make_frankenstein_sfiles
  #make_frankenstein_cflags
  #make_frankenstein
  #make_frankenstein_manual_1
  #make_frankenstein_manual_2
  make_frankenstein_manual
}
main

################################################################
# Scratch

function find_missing_s_files () {
# Find .s files for missing symbols when gcc'ing in make_frankenstein.
cat /tmp/missing | while read a; do echo -n "LOOKING FOR $a: "; f=$(find llvm-latest/ -name "*$a*.s"); if [[ -n "$f" ]]; then echo $f; else echo -n ".S NOT FOUND: "; find llvm-latest/ -name "*$a*.out" ; echo; fi; done
}

function show_which_s_files_need_which_reopted_functions () {
./mk.sh 2>&1 | while read l; do echo $l; echo $l | sed -nre 's/.*undefined reference to `F(.*)'"'"'/\1/p' | while read addr; do find llvm-latest/ -name "*$addr*.s"; done; done | less
}
