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
- https://github.com/GaloisInc/parameterized-utils
- https://github.com/GaloisInc/posix-waitpid (a fork of `posix-waitpid` package on Hackage)

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

## Testing with `run-tests.sh`

`reopt_test` can also be run with `run-tests.sh`. The script will generate
single instruction tests using the [fuzzer](https://github.com/GaloisInc/fuzz64)
and then execute them. Called with no arguments, the script will generate 1 test
of each iform and run `reopt_test` over all of them. There is also the `-s` flag
which takes the name of a test to run, for testing a single iform only.
