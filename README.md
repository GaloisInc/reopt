# reopt

Reopt is a prototype tool for recompiling compiled code to do enhanced
optimizations.  It works by mapping binaries into LLVM byte code,
using the LLVM optimization passes to optimize the LLVM, and then
combining the newly generated into the binary to generate a new
executable.

It currently supports only the x86_64 instruction set, and its main
LLVM generation capability is quite immature -- bugs are expected, and
you may want to only use Reopt to translate specific functions into
LLVM.  For example `reopt -o abc.ll abc.o` will translate the files
in the object file `abc.o` into LLVM.

More documentation will be provided once the tool is in a more mature
state.

# Installation

If you have `git` and Haskell `stack`, you should be able to install
with:

```
git submodule update --init
stack install reopt
```
