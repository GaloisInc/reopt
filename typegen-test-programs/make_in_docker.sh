#!/bin/bash

# compiles in a docker container


IMAGE=gcc:4.9

docker run --rm -v "$PWD":/workspace -w /workspace $IMAGE make
