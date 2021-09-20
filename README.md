# reopt

Reopt is a general purpose decompilation and recompilation tool
for repurposing application logic.  It does this by analyzing machine
code to recover a more flexible program representation --
specifically the [LLVM assembly language](https://llvm.org/docs/LangRef.html).
Once in this format, one can then apply optimization tools to optimize the
LLVM, recompile the application into optimized or security hardened
object code, and use Reopt to merge the recompiled code back into the
original executable.

Reopt supports Linux x86_64 programs.  We are working towards a full
1.0 release, but the current pre-release version supports the end-to-end
recompilation toolchain.


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

The `benchmark-binaries` submodule contains CentOS 7 binaries, dynamic
dependencies, and debug information that were used to benchmark
`reopt` and `reopt-explore`. The script `run_centos7_benchmarks.sh` should
unpack said binaries and run `reopt-explore` on them.

## Using Reopt

Once `reopt` is installed on a Linux system and included in your path,
you can try running it on system utilities such as `ls`.  To do an
end-to-end recompilation, you can run reopt with the command.

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

 * **Disassembly.**  `reopt --disassemble <binary>` provides a raw
   disassembler output view of the code in the binary.  This is similiar to
   `objdump`'s disassembly output.

 * **Control flow graph construction.** `reopt --cfg <binary>` displays the low
   level control flow graphs that Reopt has constructed for each discovered
   function within the binary.  This is a low-level IR that maintains
   machine code's explicit stack and register references, but lifts the
   machine code instructions into a more architectural neutral register
   transfer language.

 * **Function Recovery** `reopt --export-fns <path> <binary>` writes the
   functions that Reopt has generated after performing stack and function
   argument analysis. This is a higher-level IR in which explicit references to
   the stack have been replaced with allocations, and functions take arguments.

 * **LLVM Generation** `reopt --export-llvm <path> <binary>` generates
   LLVM from the binary.  This is essentially a version of function
   recovery rendered in LLVM's format.  Providing the
   `--annotations <ann_file>` flag during LLVM generation will
   cause `reopt` to additionally emit JSON in `<ann_file>` describing
   verification conditions which (if valid) demonstrate functional equivalence
   between the generated LLVM and machine code. Running `reopt-vcg
   <ann_file>` will simulate the executation of the LLVM and machine code,
   block-by-block, leveraging an SMT solver (cvc4) to verify as many of
   the conditions as possible.

 * **Object Files** `reopt --export-object <path> <binary>` generates an object
   file from the LLVM generated in the previous state.
   This is essentially the same as generating the LLVM, and then running
   the LLVM compiler toolchain with the selected options.

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

## Using `OCCAM` for additional optimizations

`reopt` can leverage the [OCCAM](https://github.com/SRI-CSL/OCCAM) whole-program
partial evaluator for LLVM bitcode to further optimize binaries (assuming a user
has already installed and made available both `OCCAM` and its accompanying
interface `slash`).

This feature can be enabled by passing the `--occam-config=FILE` option to
`reopt`, where `FILE` is the `reopt`/`OCCAM` manifest. The manifest should
essentially a valid [OCCAM manifest
file](https://github.com/SRI-CSL/OCCAM/wiki/Manifest) (i.e., a file with JSON
entries) with the following (optional) additional field:

 + `slash_options`: a list of command line option flags for OCCAM's `slash` tool,

and *excluding* the following fields (`reopt` will populate these appropriately):

  + `binary`
  + `name`

The `main` field should specify the desired name of the bitcode file that will
be generated for `OCCAM` to process, and the OCCAM optimized result will share
the name with an added `.occam` suffix.

N.B., when passing flags to customize `OCCAM`/`slash` behavior, be aware that
`reopt` passes the `-c` and `-emit-llvm` flags via the
`ldflags` manifest entry so `OCCAM` skips recompiling and acts only as an LLVM
to LLVM translator.

## Using Reopt Explore

With `reopt-explore` installed we can gather statistics regarding `reopt`'s ability
to recover functions in an individual or collection of binaries.

To examine a single binary, simply call `reopt-explore` with the a path to the binary:

```
$ reopt-explore $(which ls)
...
/usr/bin/ls
  Initialization:
    Code segment: 112,004 bytes
    Initial entry points: 234
    Warnings: 0
  Discovery:
    Bytes discovered: 59,502 (53%)
    Succeeded: 216 (92%)
    Failed: 18 (8%)
      Unhandled instruction: 1 (0%)
      Unidentified control flow: 17 (7%)
  Argument Analysis:
    Succeeded: 123 (57%)
    Failed: 93 (43%)
    Header Warnings: 0
    DWARF Warnings: 0
    Code Warnings: 112
  Invariant Inference:
    Succeeded: 92 (75%)
    Failed: 31 (25%)
      Indirect call target: 1 (1%)
      Unresolved call target arguments: 30 (24%)
  Recovery:
    Succeeded: 81 (88%)
    Failed: 11 (12%)
      Unsupported function value: 8 (9%)
      Unimplemented LLVM backend feature: 3 (3%)
  LLVM generation status: Succeeded.
```

To recursively search a directory for binaries and examine each,
call `reopt-explore` with the path to the directory to search:

```
$ reopt-explore /usr/bin
...
reopt analyzed 394 binaries:
Generated LLVM bitcode for 394 out of 394 binaries.
Initialization:
  Code segment: 42,933,178 bytes
  Initial entry points: 79776
  Warnings: 0
Discovery:
  Bytes discovered: 23,025,164 (54%)
  Succeeded: 64,494 (81%)
  Failed: 15,500 (19%)
    Unhandled instruction: 425 (1%)
    Unidentified control flow: 15,075 (19%)
Argument Analysis:
  Succeeded: 40,429 (63%)
  Failed: 24,065 (37%)
  Header Warnings: 0
  DWARF Warnings: 0
  Code Warnings: 38,681
Invariant Inference:
  Succeeded: 30,221 (75%)
  Failed: 10,208 (25%)
    Symbolic call stack height: 1 (0%)
    Unresolved stack read: 13 (0%)
    Indirect call target: 526 (1%)
    Call target not function entry point: 41 (0%)
    Unresolved call target arguments: 9,614 (24%)
    Could not resolve varargs args: 13 (0%)
Recovery:
  Succeeded: 21,952 (73%)
  Failed: 8,269 (27%)
    Unsupported function value: 2,425 (8%)
    Unimplemented feature: 6 (0%)
    Unimplemented LLVM backend feature: 4,762 (16%)
    Stack offset escape: 83 (0%)
    Stack read overlapping offset: 1 (0%)
    Unresolved return value: 8 (0%)
    Missing variable value: 984 (3%)
```

## Improving recovery with debug information

`reopt` and `reopt-explore` will try to determine if any debug information
is available for dynamic dependencies by quiering `gdb` (if it is installed).

Users can also manually specify dependency and debug directories to search in
manually for both `reopt` and `reopt-explore` via the folowing flags:

```
--lib-dir=PATH              Additional location to search for dynamic
                            dependencies.
--debug-dir=PATH            Additional location to search for dynamic
                            dependencies' debug info.
```


## Benchmark Binaries

The `run_centos7_benchmarks.sh` script will unpack the CentOS7 binaries
in the `benchmark-binaries` subdirectory and run `reopt-explore` on them.
N.B., this can take a while, as there are several hundred binaries.
