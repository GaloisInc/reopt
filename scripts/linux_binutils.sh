# This script downloads binutils and compiles it for a freebsd target.
#
#!/bin/bash
set -e

INSTALL_PATH=${1?Please specify the install location as first target.}

BUILD_DIR=binutils-linux-build
BINUTILS=binutils-2.29
TARGET=x86_64-unknown-linux


wget http://ftp.gnu.org/gnu/binutils/$BINUTILS.tar.xz
tar xfj $BINUTILS.tar.xz

mkdir -p $BUILD_DIR
pushd $BUILD_DIR > /dev/null
../$BINUTILS/configure --target=$TARGET --prefix=$INSTALL_PATH
make
make install
popd > /dev/null
