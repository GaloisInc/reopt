
# LLVM merge example

This directory contains a small example to guide merging of a partially
extracted LLVM program and the original binary program.

In general, a partially extracted LLVM file will include references functions
and data in the original binary. It is anticipated that the original binary will
co-exist with the rewritten binary, thus ensuring that such references in the
rewritten binary are valid. In this case, the rewritten binary will _share_ the
data segment of the original binary, rather than duplicating it.

The example in this directory consists of a small C program which will write
out, read back in, and compare a statically allocated binary tree. This program
is compiled in two ways: firstly, it is statically linked to provide the
original binary; secondly, it is compiled such that references to external
functions and data are replaced by their corresponding addresses in the
statically linked binary.

This second compilation is intended as a starting point to give an idea of what
a rewritten LLVM file will look like, although it is likely that the results of
our rewriting phase will be much less structured --- the rewritten code will
probably have minimal type information, for example.

The files in this directory are as follows:

- `tree.c`: the input C program. External (library) functions are replaced by CPP
  macros which are replaced during compilation to form the two variants;
- `Makefile`: this will produce both the binary and LLVM programs, and is run by
  `mk.sh`;
- `mk.sh`: a script to compile both versions of the program, defining the CPP
  macros as appropriate;
- `original-tree`: the statically linked original form of the binary (compiled on
  FreeBSD with clang 3.5)
- `original-tree.ll`: the LLVM version of the above
- `tree.ll`: the LLVM model of the rewritten binary (all external references
  replaced by hard-coded pointers)

We'd like a tool that could link `tree.ll` into `original-tree`.  The binary
would be rewritten to call `tree.ll` in favor of the same code in `original-tree`.
