FROM galoisbinaryanalysisbot/reopt-vcg-dev

# Install GHCUP
ENV PATH=/root/.ghcup/bin:${PATH}

RUN mkdir -p /root/.ghcup/bin
RUN  curl -o /root/.ghcup/bin/ghcup https://downloads.haskell.org/~ghcup/0.1.14.1/x86_64-linux-ghcup-0.1.14.1 \
 && chmod a+x /root/.ghcup/bin/ghcup

# Install GHC
RUN ghcup install ghc 8.10.7 \
 && ghcup install cabal 3.4.0.0 \
 && ghcup set ghc 8.10.7

COPY . /home/vadd/reopt

WORKDIR /home/vadd/reopt

RUN sed -i 's/git@github.com:/https:\/\/github.com\//' .gitmodules
RUN git submodule update --init
RUN cabal update
RUN cabal build exe:reopt exe:reopt-explore

RUN cp /home/vadd/reopt-vcg/deps/cvc4-2020-09-16-x86_64-linux-opt /usr/local/bin/cvc4
COPY ./containers/dev-with-builds/README.md /home/vadd
WORKDIR /home/vadd
