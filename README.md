# reopt

A tool for analyzing x86-64 binaries.

## Sandboxed Installation

Run `./build-sandbox.sh` to install `reopt` in a Cabal sandbox. On the
first run this downloads the dependencies automatically. In the future
use `./build-sandbox.sh` to rebuild without updating dependencies, or
`./build-sandbox.sh -p` to `git pull` the dependencies before
building.

## Dependencies

In addition to Hackage dependencies, Reopt has the following internally maintained dependencies:

- https://github.com/GaloisInc/elf (a fork of `elf` package on Hackage)
- https://github.com/GaloisInc/flexdis86
- https://github.com/GaloisInc/linux-ptrace (a fork of `linux-ptrace` package on Hackage)
- https://github.com/GaloisInc/llvm-pretty
- https://github.com/GaloisInc/mss (a multi-package repo which includes `crucible`; *use the `develop` branch, not the `master` branch*)
- https://github.com/GaloisInc/parameterized-utils
- https://github.com/GaloisInc/posix-waitpid (a fork of `posix-waitpid` package on Hackage)

The `./update-deps.sh` script can be used to initially clone these dependencies, or to update an existing clone. Note that the default branch for `mss` is `develop`, not `master`! The default branch is automatically checked out on clone by `./update-deps.sh`, but if you work in an `mss` feature branch, be sure to switch back to `develop`, not `master`, when you're done.

## Testing

The `reopt` executable can be tested by running it on
`examples/nweb23_static_fresbsd`. It should not crash ...

The `reopt_test` executable, which checks our implementation of the
X86_64 semantics against the hardware using `ptrace`, can be tested by
running it on `examples/thttpd`:

```
 reopt_test examples/thttpd 2>&1 | grep 'instruction II' | wc -l
```

To test changes in which an error would result in a changed memory or
register value, it can be useful to compare the number of "did not
match" warnings before and after your change:

```
 reopt_test examples/thttpd 2>&1 | grep 'did not match' | wc -l
```

## Executables

- `reopt`: the REOPTimizer
- `reopt_test`: a concrete instance of the Reopt x86 semantics
- `reopt_sym_test`: a minimal example of using Crucible (may have been obsoleted by `radss_compare`)
- `radss_compare`: for proving the equivalence between variants (CFAR/RADSS) using Crucible
