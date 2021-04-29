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

# diet picoc

Compiling with dietlibc took a few more tweaks:

```
diff --git a/Makefile b/Makefile
index 34a7fa2..16e524f 100644
--- a/Makefile
+++ b/Makefile
@@ -1,6 +1,6 @@
-CC=gcc
-CFLAGS=-Wall -pedantic -g -DUNIX_HOST -DVER=\"2.1\"
-LIBS=-lm -lreadline
+CC=diet c99
+CFLAGS=-static -Wall -pedantic -g -DUNIX_HOST -DVER=\"2.1\"
+LIBS=-lm
 
 TARGET = picoc
 SRCS   = picoc.c table.c lex.c parse.c expression.c heap.c type.c \
diff --git a/cstdlib/unistd.c b/cstdlib/unistd.c
index ecbd5e0..a7398ae 100644
--- a/cstdlib/unistd.c
+++ b/cstdlib/unistd.c
@@ -1,8 +1,10 @@
 /* stdlib.h library for large systems - small embedded systems use clibrary.c instead */
 #include <stdio.h>
 #include <unistd.h>
+#include <linux/limits.h>
 #include <limits.h>
 #include <fcntl.h>
+#include <getopt.h>
 #include "../interpreter.h"
 
 #ifndef BUILTIN_MINI_STDLIB
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
diff --git a/variable.c b/variable.c
index d2b6566..56316f7 100644
--- a/variable.c
+++ b/variable.c
@@ -2,6 +2,7 @@
  * variables */
  
 #include "interpreter.h"
+#include <stdint.h>
vagrant@ubuntu1804:~/examples/picoc$ git diff
diff --git a/Makefile b/Makefile
index 34a7fa2..16e524f 100644
--- a/Makefile
+++ b/Makefile
@@ -1,6 +1,6 @@
-CC=gcc
-CFLAGS=-Wall -pedantic -g -DUNIX_HOST -DVER=\"2.1\"
-LIBS=-lm -lreadline
+CC=diet c99
+CFLAGS=-static -Wall -pedantic -g -DUNIX_HOST -DVER=\"2.1\"
+LIBS=-lm
 
 TARGET = picoc
 SRCS   = picoc.c table.c lex.c parse.c expression.c heap.c type.c \
diff --git a/cstdlib/unistd.c b/cstdlib/unistd.c
index ecbd5e0..a7398ae 100644
--- a/cstdlib/unistd.c
+++ b/cstdlib/unistd.c
@@ -1,8 +1,10 @@
 /* stdlib.h library for large systems - small embedded systems use clibrary.c instead */
 #include <stdio.h>
 #include <unistd.h>
+#include <linux/limits.h>
 #include <limits.h>
 #include <fcntl.h>
+#include <getopt.h>
 #include "../interpreter.h"
 
 #ifndef BUILTIN_MINI_STDLIB
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
diff --git a/variable.c b/variable.c
index d2b6566..56316f7 100644
--- a/variable.c
+++ b/variable.c
@@ -2,6 +2,7 @@
  * variables */
  
 #include "interpreter.h"
+#include <stdint.h>
 
 /* maximum size of a value to temporarily copy while we create a variable */
 #define MAX_TMP_COPY_BUF 256

```
