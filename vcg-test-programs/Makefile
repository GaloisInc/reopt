# Use variable names for programs for distros that want to change these
# (e.g. Ubuntu)
clang ?= clang
lld ?= ld.lld
llvm_as ?= llvm-as
cpp_flags ?=
#cpp_flags=-I/usr/include/x86_64-linux-gnu

reopt := $(shell which reopt)
reopt-vcg := $(shell which reopt-vcg)

# We create empty ".crt" files to indicate successful runs of
# reopt-vcg.


all : test_add_diet_reopt.crt \
      test_fib_diet_reopt.crt

%_manual.crt : %_manual.ann %.ll $(reopt-vcg)
	$(reopt-vcg) $<
	touch $@

%.crt : %.ann %.ll $(reopt-vcg)
	$(reopt-vcg) $<
	touch $@

# Generate LLVM file using reopt for add example
test_add_diet_reopt.ll test_add_diet_reopt.ann : test_add_diet_lld.exe test_add_protos.h $(reopt)
	$(reopt) -o test_add_diet_reopt.ll  --llvm --annotations test_add_diet_reopt.ann --header test_add_protos.h --include add --include main $<

# Generate LLVM file using reopt for add example
test_fib_diet_reopt.ll test_fib_diet_reopt.ann : test_fib_diet_lld.exe test_fib_protos.h $(reopt)
	$(reopt) -o test_fib_diet_reopt.ll  --llvm --annotations test_fib_diet_reopt.ann \
                 --header test_fib_protos.h --include fib --include main $<

%_reopt_pretty.ann : %_reopt.ann
	python -m json.tool $< > $@

#####################
# Debugging utilities

# Generate LLVM file using reopt for add example
test_add_diet_reopt.cfg : test_add_diet_lld.exe test_add_protos.h $(reopt)
	$(reopt) -o test_add_diet_reopt.cfg  -c --header test_add_protos.h --include add --include main $<

# Generate LLVM file using reopt for fib example
test_fib_diet_lld.cfg : test_fib_diet_lld.exe test_fib_protos.h $(reopt)
	$(reopt) -o $@ -c --include fib --include main $<

# Generate LLVM file using reopt for fib example
test_fib_diet_lld.fns : test_fib_diet_lld.exe test_fib_protos.h $(reopt)
	$(reopt) -o $@ --fns --header test_fib_protos.h --include fib --include main $<

# Generate the .o file using reopt.
test_add_diet_reopt.o : test_add_diet_lld.exe
	$(reopt) -o $@ --include add test_add_diet_lld.exe

diet_libs=diet/start.o diet/libc.a

# This builds a test program with diet libc and lld.
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
	rm -rf *.crt *.dis *.o *.ll
	rm -rf test_add_diet_reopt.ll test_add_diet_reopt.ann test_add_diet_reopt_pretty.ann
	rm -rf test_fib_diet_reopt.ll test_fib_diet_reopt.ann test_fib_diet_reopt_pretty.ann

# Deletes executables even though they are checked into repo.
all-clean : clean
	rm -rf *.exe test_add.bc test_fib.bc

.PHONY : all-clean clean all

.PRECIOUS : %.o %.bc
