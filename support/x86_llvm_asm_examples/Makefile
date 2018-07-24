run : test_movsd
	./test_movsd

clean :
	rm -f test_movsd *.ll

test_movsd : test_movsd.ll
	clang -o $@ $<

%.ll : %.c
	clang -Wall -S -emit-llvm -O3 $<

.PHONY : run clean
