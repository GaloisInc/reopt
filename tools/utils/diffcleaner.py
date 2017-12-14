#! /usr/bin/python

import sys
import re

filename = sys.argv[1]

myfile = open(filename, "r")

openMatcher = re.compile(r'^\w')
AVXMatcher = re.compile('>   \\w*:\tc[45]')

buf = ""
printBuf = False
startLine = False
midLine = False
for line in myfile.readlines():
    if midLine == True:
        midLine = False
        if AVXMatcher.match(line):
            printBuf = False
    if openMatcher.match(line):
        if printBuf:
            print(buf)
        buf = ""
        printBuf = True
    if line == "---\n":
        midLine = True
    buf = buf + line
