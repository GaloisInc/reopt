# This script applies reopt to the Ubuntu diet example, and generates
# intermediate files showing the different stages.
#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

set -ex
relink_output="reopt_diet"

echo "About to relink"
export PATH="$HOME/opt/binutils-linux/bin:$PATH"

# Path to GNU Assembler for Linux
# GAS_PATH=$HOME/opt/binutils-linux/bin/x86_64-unknown-linux-as
GAS_PATH=$(which as)

reopt=$(which reopt)

# Get list of functions that we will not translate.
reopt_exclude="--exclude=__unified_syscall --exclude=__libc_close --exclude=__libc_open --exclude=__libc_read --exclude=__libc_write --exclude=_exit --exclude=puts --exclude=__setup_tls --exclude=stackgap --exclude=puts --exclude=__libc_write --exclude=__exit"

#reopt_exclude="$reopt_exclude --exclude=memcmp --exclude=getenv --exclude=__stdio_outs --exclude=arch_prctl --exclude=strstr --exclude=__errno_location --exclude=__nop --exclude=__error_unified_syscall --exclude=__unified_syscall_16bit"

# Get reopt to just print out the basic blocks from discovery.
$reopt "$DIR/hello_world_ubuntu_64_lts_12_04_diet" \
      $reopt_exclude \
      -o "$DIR/$relink_output.blocks"

# Get reopt to just print out the functions that are discovered.
$reopt "$DIR/hello_world_ubuntu_64_lts_12_04_diet" \
      --debug=recover \
      $reopt_exclude \
      -o "$DIR/$relink_output.fns"

# Get reopt to just print out the LLVM that is discovered.
$reopt "$DIR/hello_world_ubuntu_64_lts_12_04_diet" \
      "--gas=$GAS_PATH" \
      --llvm-version=llvm38 \
      $reopt_exclude \
      -o "$DIR/$relink_output.ll"

# Get reopt to create the object file.
$reopt "$DIR/hello_world_ubuntu_64_lts_12_04_diet" \
      "--gas=$GAS_PATH" \
      --llvm-version=llvm38 \
      $reopt_exclude \
      -o "$DIR/$relink_output.o"

# Get reopt to perform the full reoptimization script.
$reopt "$DIR/hello_world_ubuntu_64_lts_12_04_diet" \
      "--gas=$GAS_PATH" \
      --llvm-version=llvm38 \
      $reopt_exclude \
      -o "$DIR/$relink_output"

echo "Written relinked output to $relink_output"
exit 0
