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




# Dependencies

In addition to Hackage dependencies, Reopt has the following dependencies:

- https://github.com/GaloisInc/dwarf A Haskell library for parsing Dwarf information
- https://github.com/GaloisInc/elf-edit (a fork of `elf` package on Hackage)
- https://github.com/GaloisInc/flexdis86
- https://github.com/GaloisInc/llvm-pretty
- https://github.com/GaloisInc/parameterized-utils
