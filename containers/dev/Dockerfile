FROM centos:7

# Install system libraries
RUN yum install -y centos-release-scl \
 && yum install -y http://opensource.wandisco.com/centos/7/git/x86_64/wandisco-git-release-7-2.noarch.rpm \
 && yum install -y devtoolset-8-gcc devtoolset-8-make gmp-devel zlib-devel git lzma which \
 && yum clean all \
 && rm -rf /var/cache/yum
ENV PATH=/opt/rh/devtoolset-8/root/bin:${PATH}

# Install GHCUP
ENV PATH=/root/.ghcup/bin:${PATH}
ARG ghcupVer=0.1.19.0
ARG cabalVer=3.4.0.0
ARG ghcVer=8.10.7

RUN mkdir -p /root/.ghcup/bin
RUN  curl -o /root/.ghcup/bin/ghcup https://downloads.haskell.org/~ghcup/${ghcupVer}/x86_64-linux-ghcup-${ghcupVer} \
 && chmod a+x /root/.ghcup/bin/ghcup

# Install GHC
RUN ghcup install ghc ${ghcVer} \
 && ghcup install cabal ${cabalVer} \
 && ghcup set ghc ${ghcVer}

WORKDIR /root
