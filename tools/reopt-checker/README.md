# reopt-checker

This is a utility program that runs various checks to ensure
reopt's underlying algorithms understand the inputs.

## Usage

Run `reopt-checker` with the names of any executable files or
directories to check.  Directories are checked recursively
automatically.  Symbolic links in directories are not followed.

If any inconsistencies are found, the name of the file and brief
description are printed.
