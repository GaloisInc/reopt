# Try Reopt

Reopt is a general purpose decompilation and recompilation tool
for repurposing application logic.  It does this by analyzing machine
code to recover a more flexible program representation --
specifically the [LLVM assembly language](https://llvm.org/docs/LangRef.html).
Once in this format, one can then optimization tools to optimize the
LLVM, recompile the application into optimized or security hardened
object code, and use Reopt to merge the recompiled code back into the
original executable.

This repository branch is intended to help people try out Reopt.  Reopt
can be tried out through [Gitpod][gitpod].  You can
also view these locally using the [Reopt Docker image][dockerimage].  If you
have VSCode and Docker installed, then you can also use this as a
[devcontainer][devcontainer].

[gitpod]: https://gitpod.io#https://github.com/GaloisInc/reopt/tree/try-reopt
[dockerimage]: https://hub.docker.com/r/galoisbinaryanalysis/reopt
[devcontainer]: https://code.visualstudio.com/docs/remote/remote-overview


## Included examples

There are several small examples in the `examples` directory that include
make files that run `reopt` and `reopt-vcg` on the examples.  To run
each example, change into that directory and run `make`.

## Using Reopt

To try out Reopt directly, you can run it on system utilities such as
 `ls`.  To do an end-to-end recompilation, you can run reopt with the command.

```
$ reopt -o ls.exe $(which ls)
```

This execution will use the version of `ls` in your system path and produce
an executable `ls.exe` in the current directory.  When running `reopt`
will print out messages as it discovers functions within the application
and attempts to convert each discovered function into LLVM.

## Inspecting intermediate state

During recompilation, Reopt has to do a complex series of analysis steps
to lift the machine code into LLVM.  Each of these analysis steps is
incomplete and may fail either due to Reopt not recognizing features
in the binary or an error in our prerelease version of Reopt.  As such,
do not be alarmed when Reopt fails to translate functions.

If you'd like to inspect Reopt's intermediate state, there are several
command line flags to export intermediate results.  We describe the
main flags for exporting intermediate state below.
Additional options can be viewed by running `reopt --help`.

 * **Disassembly.**  `reopt -d <binary>` provides a raw disassembler output view
   of the code in the binary.  This is similiar to `objdump`'s disassembly
   output.

 * **Control flow graph construction.** `reopt --cfg <binary>` displays the low
   level control flow graphs that Reopt has constructed for each discovered
   function within the binary.  This is a low-level IR that maintains
   machine code's explicit stack and register references, but lifts the
   machine code instructions into a more architectural neutral register
   transfer language.

 * **Function Recovery** `reopt --fns <binary>` displays the functions that
   Reopt has generated after performing stack and function argument analysis.
   This is a higher-level IR in which explicit references to the stack have been
   replaced with allocations, and functions take arguments.

 * **LLVM Generation** `reopt --llvm <binary>` generates LLVM from the binary.
   This is essentially a version of function recovery rendered in LLVM's format.

 * **Object Files** `reopt --object <binary>` generates an object file from the
   LLVM generated in the previous state.  This is essentially the same as
   generating the LLVM, and then running the LLVM compiler toolchain with
   the selected options.  The output file should be specified with the
   `-o` flag as object files cannot be written to standard out.

## Function arguments

One common reason Reopt fails is because it cannot figure out the arguments
that a function can take.  We have four mechanisms for obtaining function
arguments: (1) User provided hints; (2) a small builtin database; (3) debug
information; and (4) a demand analysis that looks at what registers are used
to infer arguments.  These mechanisms are listed in priority order, although
we note that the builtin database is currently the only mechanism for supporting
functions that take a variable number of arguments like `printf`.

If you'd like to provide hints to Reopt, the recommended way is write a
C header file with the arguments, such as:

```
// decls.h


typedef long ssize_t;
typedef unsigned long size_t;

ssize_t read(int fd, void* buf, size_t count);
ssize_t write(int fd, const void* buf, size_t count);
```

You can then use this file to tell Reopt about the expected types for
`read` and `write` via the `--header` flag, e.g.,

```
reopt -o ls.exe --header decls.h $(which ls)
```