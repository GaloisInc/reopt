# reopt

Reopt is a tool under development for decompiling and recompiling
code.  It works by mapping binaries into LLVM byte code, using the
LLVM optimization passes to optimize the LLVM, and then combining the
newly generated into the binary to generate a new executable.

Reopt supports the x86_64 instruction set, and is still under active
development -- bugs are expected.

We have recently updated Reopt to make it easier to use specific
capabilities within the tool.  The main capabilities are described
below.  Additional options can be viewed by running `reopt --help`.

 * **Disassembly.**  `reopt -d <binary>` provides a raw disassembler output view
   of the code in the binary.  This is similiar to `objdump`'s disassembly
   output.

 * **Control flow graph construction.** `reopt --cfg <binary>` displays the low
   level control flow graphs that Reopt has constructed for each discovered
   function within the binary.  This is a low-level IR that maintains
   machine code's explicit stack and register references, but lifts the
   machine code instructions into a more architectural neutral register
   transfer language.

 * **Function Recovery** `reopt --fns <binary>` displays the functions that Reopt
   has generated after performing stack and function argument analysis.  This
   is a higher-level IR in which explicit references to the stack have been
   replaced with allocations, and functions take arguments.

 * **LLVM Generation** `reopt --llvm <binary>` generates LLVM from the binary.  This
   is essentially a version of function recovery rendered in LLVM's format.

 * **Object Files** `reopt --object <binary>` generates an object file from the
   LLVM generated in the previous state.  This is essentially the same as
   generating the LLVM, and then running the LLVM compiler toolchain with
   the selected options.  The output file should be specified with the
   `-o` flag as object files cannot be written to standard out.

 * **Recompilation** `reopt <binary>` runs the full recompilation toolchain
   and produces a new binary equialent to the original.

The are additional options that can be viewed by running `reopt
--help`.  More documentation will be provided once the tool is in a
more mature state.

# Installation

If you have `git` and Haskell `stack`, you should be able to install
with:

```
git submodule update --init
stack install reopt
```
