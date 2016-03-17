#!/bin/bash

# Generate size-specific .i<size>.ll files from .iN.ll templates by
# replacing "SZ" with "<size>" in the template.

usage () {
  echo "usage: $0 reopt.(MemCopy|MemSet).i<size>.ll" >&2
  exit 2
}

if [ $# != 1 ]; then
  usage
fi

# sjw's md5 is called `md5`; conathan's is called `md5sum`.
MD5=$(which md5 || which md5sum)

out=$1
template=src/$(basename $out | sed 's/\.i\([0-9]*\)\.ll$/.iN.ll/')
size=$(echo $out | sed 's/.*\.i\([0-9]*\)\.ll$/\1/')

if ! [ -e $template ]; then
  echo "Template is '$template': is your input file wrong?" >&2
  
  usage
fi

echo ";; Automatically generated at ${NOW} from $$in md5" $(${MD5} < $template) > $out
sed "s/SZ/$size/g" $template >> $out
