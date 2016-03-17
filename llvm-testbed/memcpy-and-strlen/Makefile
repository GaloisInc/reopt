.PHONY: all
all: tmp/main tmp/generator-musl

LIB_REOPT=../../libreopt/build

# tmp/main: main.c tmp/memcpy_400a28.s tmp/strlen_4009b0.s | tmp
tmp/main: main.c tmp/memcpy_400645.s tmp/strlen_4005d0.s | tmp libreopt
	gcc -Wall -o $@ $^ -lreopt -L${LIB_REOPT}

tmp/%.s: %.ll | tmp
	llc -o $@ $^

# http://www.musl-libc.org/download.html
tmp/generator-musl: generator.c | tmp
	musl-gcc -static -Wall -o $@ $^

tmp:
	mkdir -p tmp

.PHONY: libreopt
libreopt: ${LIB_REOPT}/libreopt.a

${LIB_REOPT}/libreopt.a:
	cd ../../libreopt/ && make

.PHONY: clean
clean:
	-rm -rf tmp
