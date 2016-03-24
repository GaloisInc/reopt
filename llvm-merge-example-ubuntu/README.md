# LLVM merge example, Ubuntu version

This is based on `../llvm-merge-example`. See `README.md` there for
more ideas.

The files in this directory are as follows:

- `tree.c`: the input C program. Functions are replaced by CPP macros
  which are replaced during compilation to form the variants;

- `data.h`: defines the `tree` type and some strings;

- `data.c`: not used right now;

- `Makefile`: not used right now;

- `mk.sh`: script that builds various versions of the program,
  defining the CPP macros as appropriate.
