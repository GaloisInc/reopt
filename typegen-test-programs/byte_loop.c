// byte_loop.c --- simple byte loop

#include "test.h"

#define N 1000
char bytes[N];

int NOINLINE
byte_loop(char *p, int n) {
  int count = 0;
  for (int i = 0; i < n; i++)
    count += *(p++);
  return count;
}

void NOINLINE
doit(void) {
  byte_loop(bytes, N);
}

int
main(void) {
  doit();
  return 0;    
}

  
