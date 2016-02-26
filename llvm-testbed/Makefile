.PHONY: all
all: tmp/main

tmp/main: main.c tmp/memcmp_F4004ec.s | tmp
	gcc -o $@ $^

tmp/memcmp_F4004ec.s: memcmp_4004ec.ll | tmp
	llc -o $@ $^

tmp:
	mkdir -p tmp

.PHONY: clean
clean:
	-rm -rf tmp
