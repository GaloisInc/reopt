# This script applies reopt to the Ubuntu diet example, and generates
# intermediate files showing the different stages.
#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

set -ex
relink_output="reopt_diet"

echo "About to relink"
export PATH="$HOME/opt/binutils-linux/bin:$PATH"

# Path to GNU Assembler for Linux
GAS_PATH=$HOME/opt/binutils-linux/bin/x86_64-unknown-linux-as
# GAS_PATH=$(which as)

reopt=$(which reopt)

# Get reopt to just print out the basic blocks from discovery.
$reopt "$DIR/hello_world_ubuntu_64_lts_12_04_diet" \
      -o "$DIR/$relink_output.blocks"

# Get reopt to just print out the functions that are discovered.
$reopt "$DIR/hello_world_ubuntu_64_lts_12_04_diet" \
      --debug=recover \
      -o "$DIR/$relink_output.fns"

# Get reopt to just print out the LLVM that is discovered.
$reopt "$DIR/hello_world_ubuntu_64_lts_12_04_diet" \
      "--gas=$GAS_PATH" \
      --llvm-version=llvm38 \
      -o "$DIR/$relink_output.ll"

# Get reopt to create the object file.
$reopt "$DIR/hello_world_ubuntu_64_lts_12_04_diet" \
      "--gas=$GAS_PATH" \
      --llvm-version=llvm38 \
      -o "$DIR/$relink_output.o"

# Get reopt to perform the full reoptimization script.
$reopt "$DIR/hello_world_ubuntu_64_lts_12_04_diet" \
      "--gas=$GAS_PATH" \
      --llvm-version=llvm38 \
      -o "$DIR/$relink_output"

echo "Written relinked output to $relink_output"
exit 0
