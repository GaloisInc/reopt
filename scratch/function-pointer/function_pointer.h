#ifndef _FP_
#define _FP_

// This line allows us to compile as a binary even if the symbol is missing.  We
// cannot compile as an object file in our case, because we want to work with a
// stripped version of the binary, but stripping also removes relocation tables,
// which essentially breaks the object file.
__attribute__((weak))
int apply(int (*f)(int), int);

int callee(int i);
int higher_order(int (*f)(int));
int callit();

#endif // _FP_
