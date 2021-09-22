FROM galoisbinaryanalysis/reopt

COPY deps/try-reopt /root/deps/try-reopt
COPY deps/reopt-benchmark-binaries /root/deps/reopt-benchmark-binaries
COPY scripts/unpack_benchmarks.sh /root/scripts/unpack_benchmarks.sh
COPY scripts/test_reopt.sh /root/scripts/test_reopt.sh
COPY scripts/test_reopt_explore.sh /root/scripts/test_reopt_explore.sh
COPY containers/eval-with-examples/README.md /root/README.md
COPY README.md /root/reopt-README.md
COPY deps/reopt-vcg/README.md /root/reopt-vcg-README.md

WORKDIR /root
