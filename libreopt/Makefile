# This contains the makefile for building supporting files for reopt.
#
# It essentially builds two types of files:
#
# 1. For each architecture supported by reopt, we build a file
#    "build/%ARCH%/libreopt.bc" that contains a bc file with the
#    machine specific builtins.
# 2. For the current architecture, we build a file build/libreopt.a
#    that contains artifacts for the current platform.
#
# To build both of these, just run "make" or "make all".  To build only
# the bitcode file, run "make bitcode".  To delete the build artifacts,
# run "make clean".

# Directory to put all build artifacts inside
BUILDDIR=build

# Get list of files to generate at different bitwidths
SIZES:=8 16 32 64
GENFILES:=${SIZES:%=${BUILDDIR}/reopt.MemCopy.i%.ll} ${SIZES:%=${BUILDDIR}/reopt.MemSet.i%.ll}

# Keep all intermediate files
.SECONDARY:

# Get list of objects that we will put in an archive.
LLFILES=reopt.SystemCall.FreeBSD.ll reopt.SystemCall.Linux.ll
OBJECTS=$(patsubst %.ll,${BUILDDIR}/%.o,${LLFILES}) $(patsubst %.ll,%.o,${GENFILES})

ARCHIVE:=${BUILDDIR}/libreopt.a
CLANG:=clang
CFLAGS:=-O3

LLVM_LINK=llvm-link

# List of architectures we build standard library for.
ARCH_TRIPLES:=x86_64-unknown-linux-gnu x86_64-unknown-freebsd

.PHONY: all
all: ${ARCHIVE}(${OBJECTS}) libreopt

# This target builds the OS-specific libreopt.bc files.
.PHONY: bitcode
bitcode: $(LIBREOPT_BC)

# Delete all build artifacts
.PHONY: clean
clean:
	-rm -rf ${BUILDDIR}



# Path to libreopt.bc files to generate
LIBREOPT_BC:=$(patsubst %, build/%/libreopt.bc, ${ARCH_TRIPLES})



# Build rule for libreopt.bc files
build/%/libreopt.bc: ${GENFILES}
	mkdir -p $(dir $@)
	${LLVM_LINK} -o $@ $^

# Extra dependencies for libreopt.bc files
build/x86_64-unknown-linux-gnu/libreopt.bc: src/reopt.SystemCall.Linux.ll
build/x86_64-unknown-freebsd/libreopt.bc: src/reopt.SystemCall.FreeBSD.ll

# Rule for build parameterized memcopy and memset files.
${BUILDDIR}/reopt.MemCopy.%.ll: src/reopt.MemCopy.iN.ll
	mkdir -p ${BUILDDIR}
	./generate.sh $@
${BUILDDIR}/reopt.MemSet.%.ll: src/reopt.MemSet.iN.ll
	mkdir -p ${BUILDDIR}
	./generate.sh $@

# Build object files from source .ll using system CLANG
${BUILDDIR}/%.o: src/%.ll
	mkdir -p ${BUILDDIR}
	${CLANG} -c ${CFLAGS} $^ -o $@

# Build object files from generated .ll using system CLANG
${BUILDDIR}/%.o: ${BUILDDIR}/%.ll
	mkdir -p ${BUILDDIR}
	${CLANG} -c ${CFLAGS} $^ -o $@
