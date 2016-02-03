MUSL_ROOT=$(HOME)/opt/musl

main : main.c Makefile
	clang-3.5 -static -o main main.c

main-musl : main.c Makefile
	clang-3.5 -static -nostdinc -nostdlib -I$(MUSL_ROOT)/include -L$(MUSL_ROOT)/lib -o main-musl main.c $(MUSL_ROOT)/lib/crt1.o $(MUSL_ROOT)/lib/crti.o $(MUSL_ROOT)/lib/crtn.o -lc
