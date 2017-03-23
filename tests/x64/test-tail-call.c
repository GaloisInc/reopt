/*

  This test case makes sure that macaw can find functions only
  reachable via tail call.

  callee1 issues a tail call to callee2.  Note that macaw identifies
  callee2 as a function, but also includes its body as a block in
  callee1 (so that block appears in two function definitions).  That
  seems fine.

 */
#include "util.h"

int g = -11;

void callee2(int x) {
  g += x;
}

void callee1(int x) {
  g += x * 2;
  callee2(x);
}

void _start() {
  callee1(g);

  EXIT();
}
