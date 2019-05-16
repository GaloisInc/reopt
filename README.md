# reopt

Reopt is a tool for recompiling compiled code to do enhanced
optimizations.  It works by mapping binaries into LLVM byte code,
using the LLVM optimization passes to optimize the LLVM, and then
combining the newly generated into the binary to generate a new
executable.

It currently supports only the x86_64 instruction set, but efforts
are underway to add support for additional executables.

# Usage

Scenarios:

Recompile an application

Pretty print an intermediate format:

Target Selection options:

** Whitelist
** Blacklist

Logging options:


Debugging:

** Objdump-style dump
** Discovered functions
** Recovered functions
** Generated LLVM
** Object file
** Relink existing object and code.


# Installation

If you have `git` and Haskell `stack`, you should be able to install with:

```
git submodule update --init
stack install reopt
```
