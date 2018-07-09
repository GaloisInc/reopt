# This contains the makefile for building supporting files for reopt.
#
# It builds two main files:
#
# 1. "build/libreopt.bc" that contains a bc file with supporting instructions.
# 2. "build/libreopt.a" contains the artifacts for the current platform.
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
OBJECTS=$(patsubst %.ll,%.o,${GENFILES})

ARCHIVE:=${BUILDDIR}/libreopt.a
CLANG ?= clang
CFLAGS ?= -O3

LLVM_LINK ?= llvm-link

# List of architectures we build standard library for.

.PHONY: all
all: ${ARCHIVE}(${OBJECTS}) bitcode

# This target builds the OS-specific libreopt.bc files.
.PHONY: bitcode
bitcode: build/libreopt.bc

# Delete all build artifacts
.PHONY: clean
clean:
	-rm -rf ${BUILDDIR}

# Build rule for libreopt.bc files
build/libreopt.bc: ${GENFILES}
	mkdir -p build
	${LLVM_LINK} -o $@ $^

# Extra dependencies for libreopt.bc files

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
