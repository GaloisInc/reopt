FROM ubuntu:focal

RUN apt-get update \
  && DEBIAN_FRONTEND=noninteractive TZ=UTC \
       apt-get install -y --no-install-recommends llvm-8 \
  && rm -rf /var/lib/apt/lists/*
RUN ln -s ../../lib/llvm-8/bin/opt     /usr/local/bin
RUN ln -s ../../lib/llvm-8/bin/llc     /usr/local/bin
RUN ln -s ../../lib/llvm-8/bin/llvm-mc /usr/local/bin
COPY reopt/bin/reopt /usr/local/bin

WORKDIR /root