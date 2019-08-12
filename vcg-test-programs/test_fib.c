#include<inttypes.h>
#include<stdint.h>
#include<stdio.h>

uint64_t fib(uint64_t x) {
    if (x <= 1) {
	return x;
    } else {
	return fib(x-1)+fib(x-2);
    }
}

int main() {
  printf("fib(5): %" PRIx64 "\n", fib(5));
  return 0;
}
