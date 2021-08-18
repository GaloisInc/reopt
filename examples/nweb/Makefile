clang ?= clang
lld ?= ld.lld
cpp_flags ?=

# This builds nweb and runs the
all : nweb23.reopt.exe nweb23.crt

%.crt : %.ann %.ll
	reopt-vcg $<
	touch $@

clean :
	rm -f nweb23.exe nweb23.reopt.exe nweb23.ann nweb23.ll

.PHONY : all clean

nweb23.exe : nweb23.c
	$(clang) -o $@ -g $<

nweb23.ll nweb23.ann : nweb23.exe nweb23.h
	reopt --export-llvm nweb23.ll --annotations nweb23.ann --header nweb23.h nweb23.exe || true
	python -m json.tool nweb23.ann > nweb23.ann.pretty
	mv nweb23.ann.pretty nweb23.ann

nweb23.reopt.exe : nweb23.exe
	reopt -o nweb23.reopt.exe $<
