# picoc

The `picoc` binaries were made using the picoc library [found here](https://gitlab.com/zsaleeba/picoc.git) (commit dc85a51e9211cfb644f0a85ea9546e15dc1141c3).

The makefile was modified slightly to make the builds static and omit a dependency on `readline`:

```
$ git diff
diff --git a/Makefile b/Makefile
index 34a7fa2..2b57c9a 100644
--- a/Makefile
+++ b/Makefile
@@ -1,6 +1,6 @@
-CC=gcc
-CFLAGS=-Wall -pedantic -g -DUNIX_HOST -DVER=\"2.1\"
-LIBS=-lm -lreadline
+CC=clang
+CFLAGS=-static -Wall -pedantic -g -DUNIX_HOST -DVER=\"2.1\"
+LIBS=-lm
 
 TARGET = picoc
 SRCS   = picoc.c table.c lex.c parse.c expression.c heap.c type.c \
diff --git a/platform.h b/platform.h
index 2d7c8eb..58dcdc6 100644
--- a/platform.h
+++ b/platform.h
@@ -49,7 +49,7 @@
 # ifndef NO_FP
 #  include <math.h>
 #  define PICOC_MATH_LIBRARY
-#  define USE_READLINE
+/* #  define USE_READLINE */
 #  undef BIG_ENDIAN
 #  if defined(__powerpc__) || defined(__hppa__) || defined(__sparc__)
 #   define BIG_ENDIAN
```

After the modification, running `make` will create a `picoc` executable.
