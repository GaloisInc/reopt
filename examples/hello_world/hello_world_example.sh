# This script applies reopt to the Ubuntu diet example, and generates
# intermediate files showing the different stages.
#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

set -e
relink_output="reopt_diet"

function check_path {
  (builtin type -P "$1" &> /dev/null) || (echo "Error: Could not find '$1' in path." && exit 1)
}

check_path "reopt"
check_path "opt"
check_path "llc"
check_path "llvm-mc"


reopt=$(which reopt)

# Get reopt to just print out the basic blocks from discovery.
$reopt "$DIR/hello_world_ubuntu_64_lts_12_04_diet" \
      -o "$DIR/$relink_output.blocks"

# Get reopt to just print out the functions that are discovered.
$reopt "$DIR/hello_world_ubuntu_64_lts_12_04_diet" \
      -o "$DIR/$relink_output.fns"
#      --debug=recover \

# Get reopt to just print out the LLVM that is discovered.
$reopt "$DIR/hello_world_ubuntu_64_lts_12_04_diet" \
      -o "$DIR/$relink_output.ll"

# Get reopt to create the object file.
$reopt "$DIR/hello_world_ubuntu_64_lts_12_04_diet" \
      -o "$DIR/$relink_output.o"

# Get reopt to perform the full reoptimization script.
$reopt "$DIR/hello_world_ubuntu_64_lts_12_04_diet" \
      -o "$DIR/$relink_output"

echo "Written relinked output to $relink_output"
exit 0
