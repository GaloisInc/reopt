MUSL_ROOT=$(HOME)/opt/musl
DIET_ROOT=$(HOME)/software/dietlibc-0.33
CLANG=clang-3.5
GCC=gcc-4.8

all : hello_world hello_world_musl

# Build default target
hello_world : hello_world.c Makefile
	$(CLANG) -static -o $@ $<

MUSL_CPP_FLAGS=-I$(MUSL_ROOT)/include
MUSL_LD_FLAGS=-L$(MUSL_ROOT)/lib
MUSL_LIBS=$(MUSL_ROOT)/lib/crt1.o $(MUSL_ROOT)/lib/crti.o $(MUSL_ROOT)/lib/crtn.o -lc

hello_world_musl.o : hello_world.c Makefile
	$(CLANG) -static -nostdinc $(MUSL_CPP_FLAGS) -c -o $@ $<

hello_world_musl : hello_world_musl.o Makefile
	$(CLANG) -static -nostdlib $(MUSL_LD_FLAGS) -o $@ $< $(MUSL_LIBS)

DIET_CPP_FLAGS=-I$(DIET_ROOT)/include
DIET_LIB=$(DIET_ROOT)/bin-x86_64
DIET_LD_FLAGS=-L$(DIET_LIB)
DIET_LIBS=$(DIET_LIB)/start.o $(DIET_LIB)/dietlibc.a

hello_world_diet.o : hello_world.c Makefile
	$(GCC) -static -nostdinc $(DIET_CPP_FLAGS) -c -o $@ $<

hello_world_diet : hello_world_diet.o Makefile
	ld -static -nostdlib -o $@ $< $(DIET_LIBS)

.PHONY : all