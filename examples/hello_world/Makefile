MUSL_ROOT=$(HOME)/opt/musl
CLANG="clang-3.5"

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
	$(CLANG) -static -nostdlib $(MUSL_CPP_FLAGS) $(MUSL_LD_FLAGS) -o $@ $< $(MUSL_LIBS)

.PHONY : all