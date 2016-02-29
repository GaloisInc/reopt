.PHONY: all
all: tmp/main

tmp/main: main.c tmp/memcmp_F4004ec.s | tmp
	gcc -o $@ $^

tmp/%.s: %.ll | tmp
	llc -o $@ $^

tmp:
	mkdir -p tmp

.PHONY: clean
clean:
	-rm -rf tmp
