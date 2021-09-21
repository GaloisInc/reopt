# Reopt Docker Image

This docker image contains the following Reopt related binaries
in the user's PATH:

```
reopt
reopt-explore
reopt-vcg
```

Each binary has a CLI which can provide usage information
via the `--help` flag.

The `reopt-README.md` and `reopt-vcg-README.md` markdown files
contain some high level documentation the respective tools.


## Test Binaries

The `/root/deps/try-reopt` directory contains some basic information and a few
example binaries which `reopt` and `reopt-vcg` can be run on.

The `/root/deps/reopt-benchmark-binaries` directory contains a compressed corpus
of binaries and their dependencies from CentOS7 that can serve as example target
programs for `reopt`. (See the scripts described in a later section for
unpacking and exercising `reopt` on said binaries.)


## CentOS7 Binary Scripts

The `/root/scripts` directory contains scripts that can be used to exercise
Reopt's key functionality on CentOS7 binaries also included in this Docker image
(i.e., in `/root/deps/reopt-benchmark-binaries`).

The `test_reopt.sh` can be used to run reopt through function recovery on either
`all` the included CentOS7 binaries, a `small` subset of the included binaries,
or on particular subset of user specified binaries.

Similarly, the `test_reopt.sh` can be used to run reopt through function
recovery on either `all` the included CentOS7 binaries, a `small` subset of the
included binaries, or on particular subset of user specified binaries.

N.B., running the test scripts on `all` binaries can take a significant
amount of time and resources.
