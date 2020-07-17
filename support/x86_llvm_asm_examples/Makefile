run : test_movsd test_repstos.ll
	./test_movsd

clean :
	rm -f test_movsd *.ll

test_movsd : test_movsd.ll
	clang -o $@ $<

test_repstos : test_repstos.ll
	clang -o $@ $<

%.ll : %.c
	clang -Wall -S -emit-llvm -O3 $<

.PHONY : run clean
