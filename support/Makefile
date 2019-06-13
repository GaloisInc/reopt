all : llvm_return_test.as abi_test.ll abi_test.as

%.as : %.ll
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
