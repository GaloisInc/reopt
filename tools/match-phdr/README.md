# compare-dwarfdump

This contains a utility that extracts debug and frame information from an Elf file and check to see if we recognize the program
header table.  This is used to test the patterns the relinker is
expected to support.

## Usage

Run `match-phdr` with the names of any executable files or
directories to check.  Directories are checked recursively
automatically.  Symbolic links in directories are not followed.

If any inconsistencies are found, `match-phdr` will report the name of the file.