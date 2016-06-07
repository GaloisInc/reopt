# This script applies reopt to the Ubuntu diet example, and generates
# intermediate files showing the different stages.
#!/bin/bash
set -e
relink_output="reopt_diet"

echo "About to relink"

# Path to GNU Assembler for Linux
GAS_PATH="$HOME/opt/binutils-linux/bin/x86_64-unknown-linux-gnu-as"

# Path to Reopt standard library
LIBREOPT_PATH="../../libreopt/build/x86_64-unknown-linux-gnu/libreopt.bc"

# Get list of functions that we will not translate.
reopt_notrans="--notrans=__unified_syscall --notrans=__libc_close --notrans=__libc_open --notrans=__libc_read --notrans=__libc_write --notrans=_exit --notrans=puts"

# Get reopt to just print out the basic blocks from discovery.
reopt hello_world_ubuntu_64_lts_12_04_diet \
      $reopt_notrans \
      -o "$relink_output.blocks"

# Get reopt to just print out the functions that are discovered.
reopt hello_world_ubuntu_64_lts_12_04_diet \
      --debug=recover \
      $reopt_notrans \
      -o "$relink_output.fns"

# Get reopt to just print out the LLVM that is discovered.
reopt hello_world_ubuntu_64_lts_12_04_diet \
      $reopt_notrans \
      -o "$relink_output.ll"

# Get reopt to create the object file.
reopt hello_world_ubuntu_64_lts_12_04_diet \
      "--gas=$GAS_PATH" \
      "--lib=$LIBREOPT_PATH" \
      $reopt_notrans \
      -o "$relink_output.o"

# Get reopt to perform the full reoptimization script.
reopt hello_world_ubuntu_64_lts_12_04_diet \
      "--gas=$GAS_PATH" \
      "--lib=$LIBREOPT_PATH" \
      $reopt_notrans \
      -o "$relink_output"


echo "Written relinked output to $relink_output"
exit 0
