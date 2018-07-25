CLANG=clang-3.5
GCC=gcc-4.9
GAS=as

# Wrapper program that allows you to use diet libc via 'diet gcc ...'
DIET=diet

# GCC wrapper that uses musl libc.
MUSL_GCC=musl-gcc
MUSL_ROOT=$(HOME)/local/opt/musl
# You shouldn't need to override these when you override 'MUSL_ROOT',
# at least if you installed musl locally.
MUSL_CPP_FLAGS=-I$(MUSL_ROOT)/include
MUSL_LD_FLAGS=-L$(MUSL_ROOT)/lib
MUSL_LIBS=$(MUSL_ROOT)/lib/crt1.o $(MUSL_ROOT)/lib/crti.o $(MUSL_ROOT)/lib/crtn.o -lc

################################################################

# "Configuration matrix"
exes:=\
$(foreach libc,gnu diet musl,\
  $(foreach cc,gcc clang,\
    $(foreach main,empty_main_args empty_main_no_args,\
      tmp/$(main)_$(libc)_$(cc)\
)))

.PHONY : all
all : reopt

# Build the executables.
.PHONY: exes
exes : $(exes)

# Run reopt on all executables. When reoptimizing succeeds, run the
# original and reoptimized binary and compare exit codes.
.PHONY : reopt
reopt : exes | tmp
	for f in $(exes); do \
	  out=reopt_tmp/$$(basename $$f).reopt; \
		echo "################################################################"; \
	  echo "Running reopt on $$f ..."; \
	  stack exec -- reopt -o $$out \
	    --gas $(GAS) \
	    $$f; \
	  echo "... exited $$?."; \
	  echo; \
	  if [ -e $$out ]; then \
	    echo "Running $$f ..."; \
	    ./$$f; \
	    echo "... exited $$?."; \
	    echo; \
		  echo "Running $$out ..."; \
	    chmod +x $$out; \
	    ./$$out; \
			echo "... exited $$?."; \
	  fi; \
	  echo; \
	done


################################################################

tmp/%_gnu_gcc : %.c Makefile | tmp
	$(GCC) -static -o $@ $<
tmp/%_gnu_clang : %.c Makefile | tmp
	$(CLANG) -static -o $@ $<

tmp/%_musl_gcc : %.c Makefile | tmp
	$(MUSL_GCC) -static -o $@ $<
# There is no wrapper for clang + musl, so we have to manually disable
# gnu libc and enable musl.
tmp/%_musl_clang : %.c Makefile | tmp
	$(CLANG) -static -nostdlib -nostdinc $(MUSL_CPP_FLAGS) $(MUSL_LD_FLAGS) -o $@ $< $(MUSL_LIBS)

tmp/%_diet_gcc : %.c Makefile | tmp
	$(DIET) $(GCC) -static -o $@ $<
tmp/%_diet_clang : %.c Makefile | tmp
	$(DIET) $(CLANG) -static -o $@ $<

################################################################

tmp :
	mkdir -p tmp reopt_tmp

clean :
	-rm -rf tmp reopt_tmp
