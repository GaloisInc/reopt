#! /bin/bash

PROOT=`pwd`
REOPT_TEST_BIN=`stack path --dist-dir`/build/reopt_test/reopt_test
FUZZ_C_DIR=/tmp/fuzz_c
FUZZ_BIN_DIR=/tmp/fuzz_bin
LOG_DIR=/tmp/fuzz_logs
RUN_LOG_DIR=${LOG_DIR}/run
GEN_LOG_DIR=${LOG_DIR}/gen

TIMESTAMP=`date +%Y-%m-%d-%H:%M:%S`

SAVE_DIR=$HOME/saved/$TIMESTAMP

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
  ${PROOT}/deps/fuzz64/fuzz_gen_list_tests 1 ${FUZZ_C_DIR} > ${LOG_DIR}/fuzz_gen_list_tests.log 2>&1
  echo "Generated test .c files."
}

save_failure() {
    local file=$1
    mkdir -p ${SAVE_DIR}
    cp ${FUZZ_BIN_DIR}/${file} ${RUN_LOG_DIR}/${file}.log ${SAVE_DIR}/
}

run_one() {
  file=$1
  if ! [ -e ${FUZZ_C_DIR}/$file.c ] ; then
    echo "Test \`${FUZZ_C_DIR}/$file.c\` was not found" >&2
    echo "Possible candidates include:" >&2
    find /tmp/fuzz_bin/ -name "*$file*" -exec basename {} \; >&2
    exit 1
  fi

  echo -n "$file..."

  # HACK: reopt_test returns exit code 2 if the binary it is stepping segfaults. So if we see an exit code
  # of 2, we regenerate the segfaulting test and try again.
  SEGFAULT_RESULT=2
  RUN_LIMIT=100
  for ((i = 0; i < $RUN_LIMIT; ++i)); do
    gcc -o ${FUZZ_BIN_DIR}/${file} ${FUZZ_C_DIR}/${file}.c > ${GEN_LOG_DIR}/${file}.log 2>&1
    ${REOPT_TEST_BIN} -t ${FUZZ_BIN_DIR}/${file} > ${RUN_LOG_DIR}/${file}.log 2>&1;
    result=$?
    if [ $result -ne $SEGFAULT_RESULT ]; then
      break
    else
      echo -n "segfaulted..."
      # FIXME fuzz_iform is the right way to do this, but it doesn't seem to output a .c file. This regenerates all
      # tests and moves just the desired one, which is a waste of computation.
      mkdir -p /tmp/fuzz_scratch
      ${PROOT}/deps/fuzz64/fuzz_gen_list_tests 1 /tmp/fuzz_scratch > ${LOG_DIR}/fuzz_gen_list_tests.$i.log 2>&1
      mv /tmp/fuzz_scratch/${file}.c ${FUZZ_C_DIR}
    fi
  done

  # Save binaries in failure cases
  case $result in
      0) ;;
      *) save_failure($file) ;;
  esac
  
  case $result in
    2) # Segfault
      echo "Maxed out segfaults."
      ;;
    1) # Fail
      echo "failed."
      ;;
    0) # Pass
      echo "passed."
      ;;
    *)
      echo "Unexpected return code: $result"
      exit 1
      ;;
  esac

  return $result
}

run_all() {
  BIN_NAMES=$1

  PASSES=0
  FAILS=0
  SEG_MAXS=0

  for file in ${BIN_NAMES}; do
    run_one $file
    case $? in
      2) # Segfault
        SEG_MAXS=$((SEG_MAXS+1))
        ;;
      1) # Fail
        FAILS=$((FAILS+1))
        ;;
      0) # Pass
        PASSES=$((PASSES+1))
        ;;
      *)
        echo "Unexpected return code: $result"
        exit 1
        ;;
    esac
  done

  echo "${PASSES} tests passed."
  echo "${FAILS} tests failed."
  echo "${SEG_MAXS} tests segfaulted $RUN_LIMIT times (the allowed limit)."
  if [ $FAILS -eq 0 ] && [ $SEG_MAXS -eq 0 ]; then
    exit 0
  else
    exit 1
  fi
}

usage() {
  echo "usage: $(basename "$0") [-s test_name]" >&2
}

########################################################################

# Run all tests if no arguments are passed
(( $# )) || prepare_run
(( $# )) || run_all "`cat passers.txt`"

# Run single test if a name is passed in.
while getopts "s:an" opt; do
  case $opt in
    s)
      prepare_run
      run_one $OPTARG
      exit $?
      ;;

    a)
      prepare_run
      run_all "`find ${FUZZ_C_DIR} -type f -exec basename {} .c \;`"
      exit $?
      ;;

    n)
      run_all "`comm -13 <(sort <(cat passers.txt)) <(sort <(find ${FUZZ_BIN_DIR} -type f -exec basename {} \;))`"
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



