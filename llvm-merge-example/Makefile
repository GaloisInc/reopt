
all: tree tree.ll


CLANG=clang-3.5
LLVM-DIS=llvm-dis35
LLC=llc
LLC_FLAGS=-mtriple=x86_64-unknown-freebsd

LD=x86_64-unknown-freebsd-ld

tree: tree.o data.o
	$(LD) -o tree tree.o data.o

data.bc: tree.c data.h
	$(CLANG) $(CFLAGS) -c -emit-llvm -static tree.c

tree.bc: tree.c data.h
	$(CLANG) $(CFLAGS) -c -emit-llvm -static tree.c

%.o : %.s
	$(AS) $<

%.s : %.bc
	$(LLC) $(LLC_FLAGS) $<

%.ll: %.bc
	$(LLVM-DIS) $<

clean:
	rm -f tree tree.ll tree.bc
