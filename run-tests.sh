#! /bin/bash

set -e

PROOT=`pwd`
REOPT_TEST_BIN=`stack path --dist-dir`/build/reopt_test/reopt_test
FUZZ_C_DIR=/tmp/fuzz_c
FUZZ_BIN_DIR=/tmp/fuzz_bin
LOG_DIR=/tmp/fuzz_logs
RUN_LOG_DIR=${LOG_DIR}/run
GEN_LOG_DIR=${LOG_DIR}/gen

if ! [ -e deps/fuzz64/deps/xed-intel64 ]; then
  wget -O /tmp/xed-pin.tar.gz http://software.intel.com/sites/landingpage/pintool/downloads/pin-2.14-71313-gcc.4.4.7-linux.tar.gz
  tar xzf /tmp/xed-pin.tar.gz -C /tmp
  mkdir -p deps/fuzz64/deps
  cp -r /tmp/pin-*-linux/extras/xed-intel64 deps/fuzz64/deps
  rm -r /tmp/pin-*-linux
  rm /tmp/xed-pin.tar.gz
fi

(cd deps/fuzz64 && make)

if ! [ -e ${FUZZ_C_DIR} ] || ! [ -e ${FUZZ_C_DIR} ]; then
  mkdir -p ${FUZZ_C_DIR} ${FUZZ_BIN_DIR}
fi

if ! [ -e ${RUN_LOG_DIR} ] || ! [ -e ${GEN_LOG_DIR} ]; then
  mkdir -p ${RUN_LOG_DIR} ${GEN_LOG_DIR}
fi

${PROOT}/deps/fuzz64/fuzz_gen_list_tests 1 ${FUZZ_C_DIR} > ${LOG_DIR}/fuzz_gen_list_tests.log 2>&1
echo "Generated test .c files."
BIN_NAMES=`find ${FUZZ_C_DIR} -name "*.c" -exec basename {} .c \;`

for file in ${BIN_NAMES}; do
  gcc -o ${FUZZ_BIN_DIR}/${file} ${FUZZ_C_DIR}/${file}.c > ${GEN_LOG_DIR}/${file}.log 2>&1
done
echo "Generated test executables."
echo "Running reopt_test over executables."

PASSES=0
FAILS=0
for file in ${BIN_NAMES}; do
  echo -n "${file}..."
  if ! ${REOPT_TEST_BIN} -t ${FUZZ_BIN_DIR}/${file} > ${RUN_LOG_DIR}/${file}.log 2>&1; then
    PASSES=$((PASSES+1))
    echo "passed."
  else
    FAILS=$((FAILS+1))
    echo "failed."
  fi
done

echo "$PASSES tests passed."
echo "$FAILS tests failed."

