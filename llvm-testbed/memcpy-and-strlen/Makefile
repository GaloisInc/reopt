.PHONY: all
all: tmp/main tmp/generator-musl

# tmp/main: main.c tmp/memcpy_400a28.s tmp/strlen_4009b0.s | tmp
tmp/main: main.c tmp/memcpy_400645.s tmp/strlen_4005d0.s
	gcc -Wall -o $@ $^

tmp/%.s: %.ll | tmp
	llc -o $@ $^

# http://www.musl-libc.org/download.html
tmp/generator-musl: generator.c | tmp
	musl-gcc -static -Wall -o $@ $^

tmp:
	mkdir -p tmp

.PHONY: clean
clean:
	-rm -rf tmp
