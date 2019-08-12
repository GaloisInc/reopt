# reopt-vcg

This is a prototype of VC generator for LLVM program and Macaw program.

Folder `test-programs` contains some test programs that can be used
for testing to prove/disprove their equivalence.

# Building

This is a Haskell program built using GHC.  We include configuration
files so that it can be built using [stack](https://haskellstack.org).
Once stack is installed, you may need to install GHC.  This can be
done by running with running `stack setup`.  You can that install
`reopt-vcg` by running

    stack install reopt-vcg

When checking out this repo, you should first check out the dependency
submodules with the following:

    git submodule init
    git submodule update
