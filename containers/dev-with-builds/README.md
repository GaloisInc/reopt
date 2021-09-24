# Reopt Dev Environment

This Docker image contains the build tools and artifacts for `reopt` and `reopt-vcg`
in `/home/vadd/reopt` and `/home/vadd/reopt-vcg` respectively.

## Reopt

`reopt` and `reopt-explore` are implemented in Haskell and can be built from the
`/home/vadd/reopt` directory using `cabal` via the command
`cabal build exe:reopt exe:reopt-explore` and installed
via `cabal install exe:reopt exe:reopt-explore`. After installation, the
binary locations can be found using `cabal exec which $EXE` (where `$EXE`
is `reopt` or `reopt-explore`).

## Reopt VCG

`reopt-vcg` is implemented using the in-development Lean 4 programming language
and a small amount of C++. The build script describes the precise snapshot of
Lean 4 with which `reopt-vcg` can be built (it has been pre-installed in this
container to avoid bit rot or other such issues which could arise over time). To
build `reopt-vcg`, from the `/home/vadd/reopt-vcg` directory run the `build.sh`
script. Once completed, the built executable will be found at
`/home/vadd/reopt-vcg/build/bin/reopt-vcg`.
