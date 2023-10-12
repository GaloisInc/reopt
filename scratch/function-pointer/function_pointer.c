#include <stdlib.h>

#include "function_pointer.h"

// Trying to create a stripped object file, so we can attach a type to the
// external symbol `apply` but not the other ones, via the C header.
// int apply(int (*f)(int), int i) { return f(i); }

int callee(int i) {
    return i;
}
int higher_order(int (*f)()) { return apply(f, 13); }
int callit() {
    return higher_order(callee) + 2; // need +2 otherwise Macaw doesn't see RAX used...
}
int main() { return callee(0); }
