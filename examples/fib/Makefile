# Use variable names for programs for distros that want to change these
# (e.g. Ubuntu)
clang ?= clang
lld ?= ld.lld
cpp_flags ?=
#cpp_flags=-I/usr/include/x86_64-linux-gnu

# We create empty ".crt" files to indicate successful runs of
# reopt-vcg.


all : fib_diet_lld.reopt.exe fib_diet_reopt.crt

%.crt : %.ann %.ll
	reopt-vcg $<
	touch $@

# Generate fibonacci binary
fib_diet_lld.reopt.exe : fib_diet_lld.exe fib_protos.h
	reopt -o $@ --header fib_protos.h --include fib --include main $<

# Generate LLVM file using reopt for fib example
fib_diet_reopt.ll fib_diet_reopt.ann : fib_diet_lld.exe fib_protos.h
	reopt --export-llvm fib_diet_reopt.ll --annotations fib_diet_reopt.ann \
        --header fib_protos.h --include fib --include main $<
	python -m json.tool fib_diet_reopt.ann > fib_diet_reopt.ann.pretty
	mv fib_diet_reopt.ann.pretty fib_diet_reopt.ann



diet_libs=diet/start.o diet/libc.a

# This builds a test program with diet libc and lld.
%_diet_lld.exe : %.o $(diet_libs)
	$(lld) -static -o $@ $+

# This builds a test proram with diet libc and lld.
%_diet_ld.exe : %.o $(diet_libs)
	ld -static -o $@ $+

%.ll : %.bc
	llvm-dis -o $@ $<

%.bc : %.c
	$(clang) $(cpp_flags) --target=x86_64-pc-none-elf -emit-llvm -o $@ -c $<

%.o : %.c
	$(clang) --target=x86_64-pc-linux-elf -I/usr/include/x86_64-linux-gnu -o $@ -c $<

clean :
	rm -rf *.crt *.o *.ll
	rm -rf fib_diet_reopt.ll fib_diet_reopt.ann
	rm -rf *.exe *.bc

.PHONY : clean all

.PRECIOUS : %.o %.bc
