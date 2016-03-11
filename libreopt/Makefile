
BUILDDIR=build

LLFILES=reopt.SystemCall.FreeBSD.ll reopt.SystemCall.Linux.ll
GENFILES=reopt.MemCopy.i8.ll reopt.MemCopy.i16.ll reopt.MemCopy.i32.ll reopt.MemCopy.i64.ll \
         reopt.MemSet.i8.ll reopt.MemSet.i16.ll reopt.MemSet.i32.ll reopt.MemSet.i64.ll

#FIXME: this puts the gen files in the src dir, which is a little ugly
OBJECTS=$(patsubst %.ll,${BUILDDIR}/%.o,${LLFILES}) $(patsubst %.ll,${BUILDDIR}/%.o,${GENFILES})

ARCHIVE=${BUILDDIR}/libreopt.a

LLC=llc
CLANG=clang

NOW=$(shell date +%Y-%m-%d-%H:%M:%S )

.phone: all

all: ${ARCHIVE}(${OBJECTS}) | ${BUILDDIR}

${BUILDDIR}:
	mkdir -p ${BUILDDIR}

${BUILDDIR}/%.o: src/%.ll
	${CLANG} -c -O3 $^ -o $@

${patsubst %,src/%,${GENFILES}}: src/reopt.MemCopy.iN.ll
	export SZ=`echo $@ | sed 's/.*reopt\.MemCopy\.i\([0-9]*\).ll/\1/'`
	echo ";; Automatically generated at ${NOW} from $^ md5" `md5 < $^` > $@
	sed "s/SZ/$$SZ/g" $^ >> $@

