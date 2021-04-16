# This builds nweb and runs the
all : nweb23.reopt.exe nweb23.ann

# Use cabal to run reopt if we are in source tree, and otherwise search path.
ifneq ("$(wildcard ../../reopt.cabal)","")
reopt := cabal run exe:reopt --
else
reopt := reopt
endif

clean :
	rm -f nweb23.exe nweb23.reopt.exe nweb23.ann nweb23.ll

.PHONY : all clean

nweb23.exe : nweb23.c
	cc -o $@ -g $<

nweb23.ll nweb23.ann : nweb23.exe
	$(reopt) --llvm -o nweb23.ll --annotations nweb23.ann nweb23.exe || true

nweb23.reopt.exe : nweb23.exe
	$(reopt) -o nweb23.reopt.exe $<
