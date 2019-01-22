#!/bin/sh

reopt -o psrunnew run --exclude=_start --exclude mywrite --exclude mystrlen --exclude myprint --exclude myctoi --exclude myatoi --exclude run --exclude __GNU_EH_FRAME_HDR \
  --gas=$HOME/opt/binutils-linux/bin/x86_64-unknown-linux-as
