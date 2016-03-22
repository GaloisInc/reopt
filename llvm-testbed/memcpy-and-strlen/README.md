The `.ll` files came from running

    stack exec -- ./run-llvm.sh examples/hello_world/hello_world_ubuntu_64_lts_12_04_musl

in the repo and root and copying into
`./reopted-files-from-hello-world/`, and by running

    stack exec ../../run-llvm.sh tmp/generator-musl

in this dir and copying into `./reopted-files-from-generator/`.

Do `make` and then `./tmp/main` to run the example.
