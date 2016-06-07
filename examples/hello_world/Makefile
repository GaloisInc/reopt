MUSL_ROOT=$(HOME)/opt/musl
CLANG=clang-3.5
GCC=gcc-4.8

all : hello_world hello_world_musl
.PHONY : all

# Build default target
hello_world : hello_world.c Makefile
	$(CLANG) -static -o $@ $<

#####################################################################
# musl related related environment variables and targets

MUSL_CPP_FLAGS=-I$(MUSL_ROOT)/include
MUSL_LD_FLAGS=-L$(MUSL_ROOT)/lib
MUSL_LIBS=$(MUSL_ROOT)/lib/crt1.o $(MUSL_ROOT)/lib/crti.o $(MUSL_ROOT)/lib/crtn.o -lc

hello_world_musl.o : hello_world.c Makefile
	$(CLANG) -static -nostdinc $(MUSL_CPP_FLAGS) -c -o $@ $<

hello_world_musl : hello_world_musl.o Makefile
	$(CLANG) -static -nostdlib $(MUSL_LD_FLAGS) -o $@ $< $(MUSL_LIBS)


#####################################################################
# diet libc related environment variables and targets

DIET_ROOT=$(HOME)/software/dietlibc-0.33
DIET_CPP_FLAGS=-I$(DIET_ROOT)/include
DIET_LIB=$(DIET_ROOT)/bin-x86_64
DIET_LD_FLAGS=-L$(DIET_LIB)
DIET_LIBS=$(DIET_LIB)/start.o $(DIET_LIB)/dietlibc.a

hello_world_diet.o : hello_world.c Makefile
	$(GCC) -static -nostdinc $(DIET_CPP_FLAGS) -c -o $@ $<

goodbye_world_diet.o : goodbye_world.c Makefile
	$(GCC) -static -nostdinc $(DIET_CPP_FLAGS) -c -o $@ $<

hello_world_diet : hello_world_diet.o Makefile $(DIET_LIBS)
	ld -static -nostdlib -o $@ $< $(DIET_LIBS)

#####################################################################
# CLANG arm tests

ARM_ABI=armv7a-none-eabi

hello_world_arm.s : hello_world.c Makefile
	$(CLANG) -static -nostdlib -target $(ARM_ABI) -S -o $@ $<

hello_world_arm.o : hello_world.c Makefile
	$(CLANG) -static -nostdlib -target $(ARM_ABI) -c -o $@ $<
