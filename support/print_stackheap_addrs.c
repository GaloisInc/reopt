/**
 * This test program prints out stack and heap addresses.
 */
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <signal.h>
#include <pthread.h>

// A global that we can read in segfaultHandler.
intptr_t stack_top;
intptr_t stack_base;

void segfaultAction(int sig, siginfo_t * info, void * ucontext) {
    printf("segfaultAction %d\n", sig);
    //    printf("Segfault occured at %p\n", (void*) stack_base);
    exit(-1);
}

void checkedSigAction(int signum, const struct sigaction *act, struct sigaction *oldact) {
    if (sigaction(signum, act, oldact)) {
        printf("Registering segfault handler for %d failed\n", signum);
        exit(-1);
    }

}

void registerSegfaultAction() {
    struct sigaction act;
    act.sa_sigaction = segfaultAction;
    act.sa_flags = SA_SIGINFO;
    sigemptyset(&act.sa_mask);
    checkedSigAction(SIGSEGV, &act, NULL);
}

void segfaultHandler(int sig) {
    printf("segfaultHandler %d\n", sig);
    printf("Stack top:     %p\n", (void*) stack_top);
    printf("Segfault addr: %p\n", (void*) stack_base);
    printf("Distance:      %p\n", (void*) (stack_top - stack_base));
    exit(-1);
}

struct sigaction act;

void registerSegfaultHandler() {
    act.sa_handler = segfaultHandler;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);
         checkedSigAction(SIGSEGV, &act, NULL);
         /*
    for (int signum = 1; signum != 32l; ++signum) {
        if (signum == 9) continue;
        if (signum == 17) continue;
        checkedSigAction(signum, &act, NULL);
        }*/
}

/** This allocates a c99-style array with the given number of bytes
    and checks to make sure it can access the first byte in it.
 */
void test_c99_array(size_t sz) {
    char bottom;
    printf("bottom:         %p\n", &bottom);
    char a[sz];
    printf("c99 array addr: %p\n", a);

    printf("a[0] = %u\n", (unsigned) a[0]);
    a[0] = 10;
    printf("a[0] = %u\n", (unsigned) a[0]);
}



void test_alloca_array(size_t sz) {
    char* a = alloca(sz);
    printf("alloca addr:    %p\n", a);

    printf("a[0] = %u\n", (unsigned) a[0]);
    a[0] = 10;
    printf("a[0] = %u\n", (unsigned) a[0]);
}

/*
void printPthreadStackSize() {

    size_t stacksize;
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_getstacksize (&attr, &stacksize);
    printf("Default stack size = %p\n", (void*)stacksize);
    }*/

int main(int argc, char** argv) {
    // We allocate a 2 pages to ensure we have some room at the heap
    void* x = malloc(0x2000);
    void* stack_addr = &x;
    stack_top = (intptr_t) stack_addr;

    if (stack_addr < x) {
        printf("Stack is less than heap.\n");
        exit(-1);
    }
    ptrdiff_t d = stack_addr - x;

    // We reduce our allocation by a pages to give more room than needed for stack growth.
    const size_t bias = 0x1000;

    printf("Stack address:  %p\n", stack_addr);
    printf("Heap  address:  %p\n", x);
    printf("Heap via stack: %p\n", stack_addr - d);
    test_c99_array(d - bias);
    test_alloca_array(d - bias);

    //printPthreadStackSize();

    registerSegfaultHandler();
    //    *((char*) 0x4000) = 'a';


    const size_t page_size = 0x1000;

    stack_base = (intptr_t) stack_addr & ~(page_size - 1);
    long unsigned w = 0; // Counter to force demand of stack_base
    while (1) {
        w = w + *((long unsigned*) stack_base);
        alloca(page_size);
        printf("w %p\n", (void*) stack_base);
        stack_base -= page_size;
    }
    return (int) w;
}
