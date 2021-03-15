# Dockerfile for OCCAM binary
# produces package in /occam/build
# Arguments:
#  - UBUNTU:     bionic
#  - BUILD_TYPE: debug, release

# ARG UBUNTU

# Pull base image.
FROM buildpack-deps:bionic

# ARG BUILD_TYPE
RUN echo "Build type set to: $Release" && \
     # Install deps.
    apt-get update && \
    apt-get install -yqq software-properties-common && \
    apt-get update && \
    apt-get install -y wget libprotobuf-dev python-protobuf protobuf-compiler && \
    apt-get install -y python3-pip && \
    apt-get install -y cmake && \
    apt-get install -y llvm-10 && \
    apt-get install -y clang-10 && \
    apt-get install -y clang-format-10 

RUN pip3 --version && \
    pip3 install setuptools --upgrade && \
    pip3 install wheel && \
    pip3 install protobuf && \
    pip3 install lit
RUN apt-get install -yqq libboost-dev

RUN mkdir /go
ENV GOPATH "/go"

RUN apt-get -y install golang-go && \
    go get github.com/SRI-CSL/gllvm/cmd/...

# Install llvm10 from llvm repo since bionic comes with much older version
# WORKDIR /tmp
# RUN wget https://apt.llvm.org/llvm.sh && \
#  chmod +x llvm.sh && \
#  ./llvm.sh 10

ENV LLVM_HOME "/usr/lib/llvm-10"
ENV PATH "$LLVM_HOME/bin:/bin:/usr/bin:/usr/local/bin:/occam/utils/FileCheck_trusty:$GOPATH/bin:$PATH"

RUN cd / && rm -rf /occam && \
    git clone --recurse-submodules https://github.com/SRI-CSL/OCCAM.git occam --depth=10

WORKDIR /occam
ENV CC clang
ENV CXX clang++
ENV LLVM_COMPILER clang
ENV WLLVM_OUTPUT WARNING
ENV OCCAM_HOME "/occam"

# Build configuration.
RUN make
RUN make install
RUN make test


# Install GHCUP
ENV PATH=/root/.ghcup/bin:${PATH}

RUN mkdir -p /root/.ghcup/bin
RUN  curl -o /root/.ghcup/bin/ghcup https://downloads.haskell.org/~ghcup/0.1.14.1/x86_64-linux-ghcup-0.1.14.1 \
 && chmod a+x /root/.ghcup/bin/ghcup

# Install GHC
RUN ghcup install ghc 8.10.4 \
 && ghcup install cabal 3.4.0.0 \
 && ghcup set ghc

RUN cd / && rm -rf /reopt && \
    git clone https://github.com/GaloisInc/reopt.git reopt

WORKDIR /reopt
RUN git checkout occam-in-docker
RUN sed -i 's/git@github.com:/https:\/\/github.com\//' .gitmodules
RUN git submodule update --init
RUN cabal update
RUN cabal build exe:reopt
