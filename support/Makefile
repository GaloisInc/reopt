all : llvm_return_test.as abi_test.ll abi_test.as print_stackheap_addrs

print_stackheap_addrs: print_stackheap_addrs.c
	clang -o $@ -fstack-protector-strong -fstack-protector-all $<

%.512.o : %.ll
	llc --mtriple=x86_64-pc-linux-elf -mattr=avx512f --x86-asm-syntax=intel -filetype=obj -o $@ $<

%.512.as : %.ll
	llc --mtriple=x86_64-pc-linux-elf -mattr=avx512f --x86-asm-syntax=intel -filetype=asm -o $@ $<


%.256.as : %.ll
	llc --mtriple=x86_64-pc-linux-elf -mattr=avx2 --x86-asm-syntax=intel -o $@ $<

%.128.as : %.ll
	llc --mtriple=x86_64-pc-linux-elf --x86-asm-syntax=intel -o $@ $<

%.as : %.bc
	llc --mtriple=x86_64-pc-linux-elf --x86-asm-syntax=intel -o $@ $<

%.bc : %.c
	clang -c --target=x86_64-pc-linux-elf -mavx512f -emit-llvm -O3 -o $@ $<

%.ll : %.bc
	llvm-dis $<

.PHONY : all clean
.PRECIOUS : %.bc

clean :
	rm -rf *.as *.bc abi_test.ll
