# LLVM merge example, Ubuntu version

These examples were developed and tested on Ubuntu 15.10.

This is based on `../llvm-merge-example`. See `README.md` there for
more ideas.

The main script `mk.sh` uses MUSL libc and its `musl-gcc` command.

Do

    ./mk.sh

to generate `tmp/original-tree` and `tmp/frankentree`. The former is a
static compilation of `tree.c` and the latter is a compilation of
`tree.c` together with a bunch of reopted versions of functions
discovered by running reopt on `tmp/original-tree`.

The files in this directory are as follows:

- `tree.c`: the input C program. Functions are replaced by CPP macros
  which are replaced during compilation to form the variants;

- `data.h`: defines the `tree` type and some strings;

- `data.c`: not used right now;

- `Makefile`: not used right now;

- `mk.sh`: script that builds various versions of the program,
  defining the CPP macros as appropriate. It uses `musl-gcc`, so you
  need to install `musl`.

- `example`: example output:

  - `llvm-latest` and `latest.txt`: copies of corresponding files from
    running `stack exec ../run-llvm.sh tmp/original-tree` as part of
    `./mk.sh`.

  - `tmp`: copy of `tmp` dir from running `./mk.sh`.

  - `tmp/frankentree`: copy of reassembled tree from running
    `./mk.sh`.
