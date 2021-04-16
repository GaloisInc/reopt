FROM ubuntu:20.04

# Install needed packages for building
ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=America/Los_Angeles
RUN apt-get update \
    && DEBIAN_FRONTEND=noninteractive TZ=UTC \
       apt-get install -y cmake ccache gcc g++ git \
        libgmp-dev zlib1g-dev lib32z1-dev clang-8 llvm-8

