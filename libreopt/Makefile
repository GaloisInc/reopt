
BUILDDIR=build

LLFILES=reopt.SystemCall.FreeBSD.ll reopt.SystemCall.Linux.ll
SIZES=8 16 32 64
GENFILES=${SIZES:%=${BUILDDIR}/reopt.MemCopy.i%.ll} ${SIZES:%=${BUILDDIR}/reopt.MemSet.i%.ll}

OBJECTS=$(patsubst %.ll,${BUILDDIR}/%.o,${LLFILES}) $(patsubst %.ll,%.o,${GENFILES})

ARCHIVE=${BUILDDIR}/libreopt.a

LLC=llc
CLANG=clang

NOW=$(shell date +%Y-%m-%d-%H:%M:%S )

.PHONY: all
all: ${ARCHIVE}(${OBJECTS})

${BUILDDIR}:
	mkdir -p ${BUILDDIR}

${BUILDDIR}/%.o: src/%.ll | ${BUILDDIR}
	${CLANG} -c -O3 $^ -o $@
${BUILDDIR}/%.o: ${BUILDDIR}/%.ll | ${BUILDDIR}
	${CLANG} -c -O3 $^ -o $@

# Each .i<size>.ll file is rebuilt if either of the .iN.ll templates
# change.
${BUILDDIR}/reopt.MemCopy.%.ll: src/reopt.MemCopy.iN.ll
	./generate.sh $@
${BUILDDIR}/reopt.MemSet.%.ll: src/reopt.MemSet.iN.ll
	./generate.sh $@

.PHONY: clean
clean:
	-rm -rf ${BUILDDIR}
