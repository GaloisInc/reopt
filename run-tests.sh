#! /bin/bash

PROOT=`pwd`
REOPT_TEST_BIN=`stack path --dist-dir`/build/reopt_test/reopt_test
FUZZ_C_DIR=/tmp/fuzz_c
FUZZ_BIN_DIR=/tmp/fuzz_bin
LOG_DIR=/tmp/fuzz_logs
RUN_LOG_DIR=${LOG_DIR}/run
GEN_LOG_DIR=${LOG_DIR}/gen

make_fuzz64() {
  (cd deps/fuzz64 && make)
}

mk_test_dirs() {
  if ! [ -e ${FUZZ_C_DIR} ] || ! [ -e ${FUZZ_C_DIR} ]; then
    mkdir -p ${FUZZ_C_DIR} ${FUZZ_BIN_DIR}
  fi

  if ! [ -e ${RUN_LOG_DIR} ] || ! [ -e ${GEN_LOG_DIR} ]; then
    mkdir -p ${RUN_LOG_DIR} ${GEN_LOG_DIR}
  fi
}

generate_fuzz() {
  ${PROOT}/deps/fuzz64/fuzz_gen_list_tests 1 ${FUZZ_C_DIR} > ${LOG_DIR}/fuzz_gen_list_tests.log 2>&1
  echo "Generated test .c files."
}

get_xed() {
  if ! [ -e deps/fuzz64/deps/xed-intel64 ]; then
    wget -O /tmp/xed-pin.tar.gz http://software.intel.com/sites/landingpage/pintool/downloads/pin-2.14-71313-gcc.4.4.7-linux.tar.gz
    tar xzf /tmp/xed-pin.tar.gz -C /tmp
    mkdir -p deps/fuzz64/deps
    cp -r /tmp/pin-*-linux/extras/xed-intel64 deps/fuzz64/deps
    rm -r /tmp/pin-*-linux
    rm /tmp/xed-pin.tar.gz
  fi
}

prepare_run() {
  get_xed
  make_fuzz64
  mk_test_dirs
  generate_fuzz
}

run_one() {
  prepare_run
  file=$1
  if ! gcc -o ${FUZZ_BIN_DIR}/${file} ${FUZZ_C_DIR}/${file}.c > ${GEN_LOG_DIR}/${file}.log 2>&1; then
    echo "Test \`$file\` was not found" >&2
    echo "Possible candidates include:" >&2
    find /tmp/fuzz_bin/ -name "*CVTSI2SD*" -exec basename {} \; >&2
    exit 1
  fi
  echo "Generated test executable."
  echo "Running reopt_test over executable."

  echo -n "Running $file..."
  if ${REOPT_TEST_BIN} -t ${FUZZ_BIN_DIR}/${file} > ${RUN_LOG_DIR}/${file}.log 2>&1; then
    echo "passed."
  else
    FAILS=$((FAILS+1))
    echo "failed."
  fi

  exit 0
}

run_all() {
  prepare_run
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
    if ${REOPT_TEST_BIN} -t ${FUZZ_BIN_DIR}/${file} > ${RUN_LOG_DIR}/${file}.log 2>&1; then
      PASSES=$((PASSES+1))
      echo "passed."
    else
      FAILS=$((FAILS+1))
      echo "failed."
    fi
  done

  echo "${PASSES} tests passed."
  echo "${FAILS} tests failed."
  exit 0
}

usage() {
  echo "usage: $(basename "$0") [-s test_name]" >&2
}

########################################################################

(( $# )) || run_all


while getopts ":s:" opt; do
  case $opt in
    s)
      run_one $OPTARG
      ;;

    \?)
      echo "Invalid option: -$OPTARG" >&2
      usage
      exit 1
      ;;

    :)
      echo "Option -$OPTARG requires an argument." >&2
      exit 1
      ;;
  esac
done



