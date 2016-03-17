#!/bin/bash
#
# This script downloads musl (a libc implementation) and compiles it for a freebsd target.

set -e

INSTALL_PATH=$HOME/opt/musl-freebsd

# INSTALL_PATH=${1?Please specify the install location as first target.}


wget -N http://www.musl-libc.org/releases/musl-1.1.12.tar.gz
tar xfz musl-1.1.12.tar.gz

mkdir -p musl-build
pushd musl-1.1.12 > /dev/null
../musl-1.1.12/configure --target=x86_64-unknown-freeebsd --prefix=$INSTALL_PATH CC="clang-3.5" CFLAGS="--target=x86_64-unknown-freebsd" AS=x86_64-unknown-freebsd-as


make
popd > /dev/null
if false; then
    sda
fi
