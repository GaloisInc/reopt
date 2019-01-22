typedef long uint64_t;

void mywrite(int fd, const char* s, int len);

int mystrlen(const char* s) {
    int r = 0;
    while (*s) {
	++s; ++r;
    }
    return r;
}

void myprint(int fd, const char* s) {
    mywrite(fd, s, mystrlen(s));
}


char myitoc(uint64_t x) {
    return x < 10 ? '0' + x : ('a' - 10) + x;
}

char spbuf[17];

const char* myitoa(uint64_t x) {
    for (int i = 0; i != 16; ++i) {
	uint64_t p = (x >> 4 * i) & 0xf;
	spbuf[15-i] = myitoc(p);
    }
    spbuf[16] = 0;
    return spbuf;

}

 typedef struct
 {
     int a_type;
     union {
         long a_val;
         void *a_ptr;
         void (*a_fnc)();
     } a_un;
 } auxv_t;

// Run with the stack pointer
void run(const uint64_t* sp) {

    myprint(0, "Hello world!\n");
    uint64_t argc = sp[0];
    const char** argv = (const char**) (sp + 1);
    uint64_t i;
    for (i = 0; i != argc; ++i) {
	myprint(0, argv[i]);
	myprint(0, "\n");
    }

    const char** init_envp = (const char**) (sp + 2 + argc);
    const char** envp = init_envp;
    while (*envp) {
	myprint(0, *envp);
	myprint(0, "\n");
	++envp;
    }

    const auxv_t* init_auxv = (const auxv_t*) (envp + 1);
    const auxv_t* auxv = init_auxv;
    while (auxv->a_type) {
	myprint(0, "AUXV\n");
	myprint(0, myitoa(auxv->a_type));
	myprint(0, "\n");
	switch (auxv->a_type) {
	case 0x3:
	    myprint(0, "PHDR: ");
	    myprint(0, myitoa(auxv->a_un.a_val));
	    myprint(0, "\n");
	    break;
	}
	++auxv;
    }


}
