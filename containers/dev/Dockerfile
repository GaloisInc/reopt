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

RUN mkdir -p /root/.ghcup/bin
RUN  curl -o /root/.ghcup/bin/ghcup https://downloads.haskell.org/~ghcup/0.1.14.1/x86_64-linux-ghcup-0.1.14.1 \
 && chmod a+x /root/.ghcup/bin/ghcup

# Install GHC
RUN ghcup install ghc 8.10.5 \
 && ghcup install cabal 3.4.0.0 \
 && ghcup set ghc 8.10.5

WORKDIR /root
