# Pull base image.
FROM buildpack-deps:focal

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
    apt-get install -y clang-format-10  && \
    apt-get install -y llvm-8 && \
    apt-get install -y clang-8 && \
    apt-get install -y lld && \
    apt-get install -y binutils


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


# copy in reopt related binaries
COPY reopt/bin/reopt /usr/local/bin
COPY reopt/bin/reopt-relink /usr/local/bin
COPY reopt/bin/reopt-vcg /usr/local/bin
COPY reopt/bin/cvc4 /usr/local/bin
