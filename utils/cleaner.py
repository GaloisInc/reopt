#! /usr/bin/python

import sys
import re

filename = sys.argv[1]

myfile = open(filename, "r")

fileFormatFixer = re.compile(r'.*file format.*')
zeroFixer = re.compile(r' *<.*>| *#.*|\+0x0') # remove +0x0 because we can't tell when we need it
stringFixer = re.compile(r'(stos|scas|cmps|movs) .*') # remove implicit arguments on strings
nopFixer = re.compile(r'(data32 )*nop( *)(BYTE|.?WORD|XMMWORD) PTR (.s:)?') # remove pointer widths on nops
voidFixer = re.compile(r'(prefetchnta|prefetcht0|prefetchw|prefetch)( *)(BYTE|.?WORD|XMMWORD) PTR ')
nullFixer = re.compile(r'(movnti|movntdq|movntdqa)( *)(BYTE|.?WORD|XMMWORD) PTR ')
movabsFixer = re.compile(r'movabs') # change movabs to mov
funcstartFixer = re.compile(r'^\w* <\S*>:$')

for line in myfile.readlines():
    if funcstartFixer.match(line) != None or fileFormatFixer.match(line) != None:
        continue
    line = zeroFixer.sub("", line)
    line = stringFixer.sub(r'\g<1>', line)
    line = nopFixer.sub(r'nop    ', line)
    line = voidFixer.sub(r'\g<1>\g<2>', line)
    line = nullFixer.sub(r'\g<1>\g<2>', line)
    line = movabsFixer.sub("mov   ", line)
    line = line.rstrip()
    if line != "":
        print (line) 
