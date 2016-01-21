# This script downloads binutils and compiles it for a freebsd target.
#
#!/bin/bash
set -e

INSTALL_PATH=${1?Please specify the install location as first target.}

wget http://ftp.gnu.org/gnu/binutils/binutils-2.25.1.tar.bz2
tar xfj binutils-2.25.1.tar.bz2

mkdir -p binutils-build
pushd binutils-build > /dev/null
../binutils-2.25.1/configure --target=x86_64-unknown-freebsd --prefix=$INSTALL_PATH
make
make install
popd > /dev/null
