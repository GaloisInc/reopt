
all: tmp/tree tmp/tree.ll

CLANG=clang-3.5
LLVM-DIS=llvm-dis35
LLC=llc
# LLC_FLAGS=-mtriple=x86_64-unknown-freebsd

# LD=x86_64-unknown-freebsd-ld
LD=ld

tmp/tree: tmp/tree.o tmp/data.o | tmp
	$(LD) -o $@ $<

tmp/data.bc: tree.c data.h | tmp
	$(CLANG) $(CFLAGS) -o $@ -c -emit-llvm -static tree.c

tmp/tree.bc: tree.c data.h | tmp
	$(CLANG) $(CFLAGS) -o $@ -c -emit-llvm -static tree.c

tmp/%.o : tmp/%.s | tmp
	$(AS) -o $@ $<

tmp/%.s : tmp/%.bc | tmp
	$(LLC) $(LLC_FLAGS) -o $@ $<

tmp/%.ll: tmp/%.bc | tmp
	$(LLVM-DIS) -o $@ $<

clean:
	rm -f tmp

tmp:
	mkdir -p tmp
