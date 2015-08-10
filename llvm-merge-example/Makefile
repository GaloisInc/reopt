
all: tree tree.ll

tree: tree.c
	clang35 $(CFLAGS) -static -o tree tree.c

tree.ll: tree.c
	clang35 $(CFLAGS) -c -emit-llvm tree.c
	llvm-dis35 tree.bc
	rm tree.bc

clean:
	rm tree tree.ll
