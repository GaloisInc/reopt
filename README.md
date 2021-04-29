# reopt

Reopt is a general purpose decompilation and recompilation tool
for repurposing application logic.  It does this by analyzing machine
code to recover a more flexible program representation --
specifically the [LLVM assembly language](https://llvm.org/docs/LangRef.html).
Once in this format, one can then optimization tools to optimize the
LLVM, recompile the application into optimized or security hardened
object code, and use Reopt to merge the recompiled code back into the
original executable.

Reopt supports Linux x86_64 programs.  We are working towards a full
1.0 release, but the current pre-release version supports the end-to-end
recompilation toolchain.

## Getting Reopt

Although Reopt can build on other POSIX systems such as OSX, we recommend
building Reopt to run on Linux.  Reopt currently only supports Elf binaries
which are the default binary format for Linux.  It does not support OSX
Macho binaries, and so it is easier to find applications
to try Reopt on when running Linux.

### Gitpod

For most people, the easiest way to try out Reopt is to
[try it out on Gitpod](https://gitpod.io#https://github.com/GaloisInc/reopt/tree/try-reopt).
This requires an account on Gitpod, but gives you access to a VSCode IDE connected to a
Linux container with Reopt pre-installed.

### Github Releases

If you have Linux installed, you can download one of our recent releases from
the [Releases page](https://github.com/GaloisInc/reopt/releases).  We build
releases as static binaries on Centos 7, so they should work on a variety
of distributions.

### Docker

If you have Docker installed, you can install and run the Reopt pre-release
Docker image by running:

```
docker pull galoisbinaryanalysis/reopt
docker run --rm -it galoisbinaryanalysis/reopt
```

### Building from source

Building Reopt requires that one has installed the GHC Haskell
compiler and supporting tooling.  We currently build on GHC 8.10.4.
An easy way to get GHC is to install [ghcup](https://www.haskell.org/ghcup/),
and run `ghcup install ghc-8.10.4`.  We also maintain a
[Docker image](https://hub.docker.com/r/galoisbinaryanalysis/reopt-dev)
that has GHC and other dependencies preinstalled for building Reopt.

Once GHC is installed, the following steps may be useful for building Reopt:

```
git clone https://github.com/GaloisInc/reopt.git

cd reopt
# Fix submodule URLs (can skip if you have a Github account)
sed -i 's/git@github.com:/https:\/\/github.com\//' .gitmodules
git submodule update --init --remote
# Build Reopt
cabal install exe:reopt
```

Reopt will be installed at `$HOME/.cabal/bin/reopt`.

Reopt's verification condition generator (`reopt-vcg`) is included in the
aforementioned Github release and Docker image, however the source is currently
maintained in a [separate repository](https://github.com/galoisinc/reopt-vcg)
with it's own build instructions and requirements.

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
   Providing the `--annotations <ann_file>` flag during LLVM generation will
   cause `reopt` to additionally emit JSON in `<ann_file>` describing
   verification conditions which (if valid) demonstrate functional equivalence
   between the generated LLVM and machine code. Running `reopt-vcg
   <ann_file>` will simulate the executation of the LLVM and machine code,
   block-by-block, leveraging an SMT solver (cvc4) to verify as many of
   the conditions as possible.

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
