FROM ubuntu:20.04

# Install system libraries
# https://www.haskell.org/ghcup/install/#version-2004-2010
RUN apt update -y \
 && apt install -y build-essential curl libffi-dev \
 && apt install -y libffi7 libgmp-dev libgmp10 libncurses-dev \
 && apt install -y libncurses5 libtinfo5 \
 && apt install -y git lzma zlib1g-dev libghc-zlib-dev

# Install GHCUP
ENV PATH=/root/.ghcup/bin:${PATH}
ARG ghcupVer=0.1.19.5
ARG cabalVer=3.10.1.0
ARG ghcVer=9.2.8

RUN mkdir -p /root/.ghcup/bin
RUN  curl -o /root/.ghcup/bin/ghcup https://downloads.haskell.org/~ghcup/${ghcupVer}/x86_64-linux-ghcup-${ghcupVer} \
 && chmod a+x /root/.ghcup/bin/ghcup

# Install GHC
RUN ghcup install ghc ${ghcVer} \
 && ghcup install cabal ${cabalVer} \
 && ghcup set ghc ${ghcVer}

WORKDIR /root
