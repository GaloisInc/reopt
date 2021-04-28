This directory contains the nweb web server.  It has been tested to
work with reopt.  The makefile expects to be in the source tree or for
reopt to be in the local path.  It also expects `cc` to be a valid C
compiler, and a Posix operating system.

You can build the original executablew and build the artifact
by running `make`.  This will build the original executable,
and run reopt to recompile and produce a new executable.  Reopt's
output is quite verbose and will report warnings, but these can
be ignored for now.

Once the reopt version is built, you can run nweb by providing it with a port number and a directory with some content, e.g.,

```
./nweb23.reopt.exe 1238 content
```
