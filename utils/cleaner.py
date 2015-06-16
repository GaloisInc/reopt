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
xmmMovFixer = re.compile(r'(mulss|mulsd|movq|movd|movss|movsd|movhps|movhpd|movlps|movlpd)(.*?)(BYTE|.?WORD|XMMWORD) PTR ')
movabsFixer = re.compile(r'movabs') # change movabs to mov
floatRegisterFixer = re.compile(r'st(\(.\))')
funcstartFixer = re.compile(r'^\w* <\S*>:$')
movdFixer = re.compile(r'movd ')
prefetchFixer = re.compile(r'prefetch\w* ')

for line in myfile.readlines():
    if funcstartFixer.match(line) != None or fileFormatFixer.match(line) != None:
        continue
    line = zeroFixer.sub("", line)
    line = floatRegisterFixer.sub("st", line)
    line = stringFixer.sub(r'\g<1>', line)
    line = nopFixer.sub(r'nop    ', line)
    line = voidFixer.sub(r'\g<1>\g<2>', line)
    line = nullFixer.sub(r'\g<1>\g<2>', line)
    line = movabsFixer.sub("mov   ", line)
    line = xmmMovFixer.sub(r'\g<1>\g<2>', line)
    line = movdFixer.sub(r'movq ', line)
    line = prefetchFixer.sub(r'prefetch ', line)
    line = line.rstrip()
    if line != "":
        print (line) 
