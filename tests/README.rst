This directory contains some automated tests for reopt_ and macaw_.  The macaw tests currently live in this repository because this is where our x86_64 semantics are defined.  Separating those semantics out is difficult at this point.  The tests currently cover the code discovery implementation in macaw.  Specifically, they check:

1) That the correct number of functions are found,
2) Each function starts at the expected address,
3) Each function contains the correct number of basic blocks,
4) Each basic block starts at the expected address

The ``Makefile`` in the tests directory rebuilds the test files from source.  The resulting binaries are checked in to the repository so that Mac OS users can run the tests without having to have a Linux build toolchain available.  Additionally, having the binaries in the repository keeps the code layout stable.  Expected addresses are hard-coded into the expected results of the test suite; regenerating the binaries on a different system or with a different compiler could change the offsets in the test binaries, which would require the expected results to be tweaked.  This is unfortunate, but difficult to avoid in a robust way.

Note that the tests are bare-bones and do not link against libc.  They issue the exit system call directly to terminate.  This lets us get small and comprehensible test cases.

.. _reopt: https://github.com/GaloisInc/reopt
.. _macaw: https://gitlab-ext.galois.com/macaw/macaw
