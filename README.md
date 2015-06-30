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
