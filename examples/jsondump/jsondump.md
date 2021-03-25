# jsondump

The `jsondump` executables are created from the library found here `https://github.com/zserge/jsmn` (commit 053d3cd29200edb1bfd181d917d140c16c1f8834).

```
$ musl-gcc -static -g  example/jsondump.c -o jsondump-musl
$ diet gcc -static -g  example/jsondump.c -o jsondump-diet-gcc
```
