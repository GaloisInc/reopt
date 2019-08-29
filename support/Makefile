all : llvm_return_test.512.as abi_test.512.ll abi_test.256.ll abi_test.as print_stackheap_addrs avx_intrin.512.o avx_intrin.512.ll

print_stackheap_addrs: print_stackheap_addrs.c
	clang -o $@ -fstack-protector-strong -fstack-protector-all $<


%.512.o : %.ll
	llc --mtriple=x86_64-pc-linux-elf -mattr=avx512f -filetype=obj -o $@ $<

%.512.as : %.ll
	llc --mtriple=x86_64-pc-linux-elf -mattr=avx512f -filetype=asm --x86-asm-syntax=intel -o $@ $<

%.256.as : %.ll
	llc --mtriple=x86_64-pc-linux-elf -mattr=avx2 --x86-asm-syntax=intel -o $@ $<

%.128.as : %.ll
	llc --mtriple=x86_64-pc-linux-elf --x86-asm-syntax=intel -o $@ $<

%.as : %.bc
	llc --mtriple=x86_64-pc-linux-elf --x86-asm-syntax=intel -o $@ $<

%.512.bc : %.c
	clang -c --target=x86_64-pc-linux-elf -emit-llvm -mavx512f -O3 -o $@ $<

%.256.bc : ^.c
	clang -c --target=x86_64-pc-linux-elf -emit-llvm -mavx2 -O3 -o $@ $<

%.512.o : %.512.c
	clang --target=x86_64-pc-linux-elf -mavx512f -c -o $@ $<

%.512.bc : %.512.c
	clang -c --target=x86_64-pc-linux-elf -mavx512f -emit-llvm -O3 -o $@ $<


%.ll : %.bc
	llvm-dis $<

.PHONY : all clean
.PRECIOUS : %.512.bc %.bc %.ll

clean :
	rm -rf *.as *.bc *.o abi_test.ll print_stackheap_addrs
