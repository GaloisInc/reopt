
# Use variable names for programs for distros that want to change these
# (e.g. Ubuntu)
clang ?= clang
lld ?= ld.lld
cpp_flags ?=
#cpp_flags=-I/usr/include/x86_64-linux-gnu

reopt := $(shell which reopt)


all : test_add_diet_lld.exe test_fib_diet_lld.exe test_add_glibc_lld.exe test_add.bc test_fib.bc

# Generate a the blocks file.
test_add_diet_reopt.blocks : test_add_diet_lld.exe
	$(reopt) -o $@ --cfg --include add test_add_diet_lld.exe

# Generate an LLVM file using reopt.
test_add_diet_reopt.ll : test_add_diet_lld.exe test_add_protos.h $(reopt)
	$(reopt) -o $@ --llvm --header test_add_protos.h --include add test_add_diet_lld.exe

# Generate the .o file using reopt.
test_add_diet_reopt.o : test_add_diet_lld.exe
	$(reopt) -o $@ --include add test_add_diet_lld.exe

# Generate a the executable file.
test_add_diet_reopt.exe : test_add_diet_lld.exe
	$(reopt) -o $@ --include add test_add_diet_lld.exe

diet_libs=diet/start.o diet/libc.a

# This builds a test proram with diet libc and lld.
test_%_diet_lld.exe : test_%.o $(diet_libs)
	$(lld) -static -o $@ $+

# This builds a test proram with diet libc and lld.
test_%_diet_ld.exe : test_%.o $(diet_libs)
	ld -static -o $@ $+

# Static libraries needed to link against glibc.
#
# Note. lld does not care about order, but binutils ld does.  This should be tested
# with binutils ld after changes.
glibc_libs= glibc/crt1.o glibc/libc.a glibc/libgcc_eh.a glibc/libc.a glibc/libgcc.a glibc/placeholder_initfini.o

# This builds with glibc and lld.
test_%_glibc_lld.exe : test_%.o $(glibc_libs)
	$(lld) -static -o $@ $+

# This builds with glibc and ld.
test_%_glibc_ld.exe : test_%.o $(glibc_libc)
	ld -static -o $@ $+

# Create disassembly from exe
%.dis : %.exe
	llvm-objdump -d $< > $@

%.ll : %.bc
	llvm-dis -o $@ $<

%.bc : %.c
	$(clang) $(cpp_flags) --target=x86_64-pc-none-elf -emit-llvm -o $@ -c $<

%.o : %.c
	$(clang) --target=x86_64-pc-linux-elf -I/usr/include/x86_64-linux-gnu -o $@ -c $<

clean :
	rm -rf *.dis *.o *.ll

# Deletes executables even though they are checked into repo.
all-clean : clean
	rm -rf *.exe test_add.bc test_fib.bc

.PHONY : all-clean clean all

.PRECIOUS : %.o %.bc
