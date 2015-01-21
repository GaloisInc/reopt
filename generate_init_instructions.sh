#!/bin/bash
gobjdump -d examples/nweb23_static_fresbsd | sed -e 's/.\{32\}//' | cut -f 1 -d ' ' | sed -e 's/.*://' | sed -e 's/0.*//' | sort | uniq
