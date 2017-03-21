#include "util.h"

int g1;
int g2;
int g3;
int g4;

int f2() {
  return (int)&g2;
}

int f1(long l1, long l2, long l3) {
  long i1 = (long)&g1;
  i1 = l1 + i1 + l2 + l3;
  i1 += l3 * 2;
  i1 = i1 / (l2 - 100 + f2());
  return (int)i1;
}

void _start() {
  long i1 = (long)&g1;
  long i2 = (long)&g2;
  long i3 = (long)&g3;
  g1 = f1(i1, i2, i3);
  EXIT();
}

