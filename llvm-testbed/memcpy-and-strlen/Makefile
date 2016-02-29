.PHONY: all
all: tmp/main

tmp/main: main.c tmp/memcpy_400a28.s tmp/strlen_4009b0.s | tmp
	gcc -Wall -o $@ $^

tmp/%.s: %.ll | tmp
	llc -o $@ $^

tmp:
	mkdir -p tmp

.PHONY: clean
clean:
	-rm -rf tmp
