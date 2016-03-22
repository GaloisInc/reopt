#!/bin/sh
# This script redirects main in the diet libc hello world to use the "Goodbye world" main.

reopt --relink -r diet_redir.yaml -o goodbye_world hello_world_ubuntu_64_lts_12_04_diet --new goodbye_world_ubuntu_64_lts_12_05_diet.o
