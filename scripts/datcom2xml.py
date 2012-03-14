#!/usr/bin/python

# Lenna X. Peterson
# arklenna@gmail.com

import sys
import re
from math import floor, ceil

title_re = re.compile(r"0\s+-+DERIVATIVE \(PER DEGREE\)-+")
header_re = re.compile(r"0\s+ALPHA")
footer_re = re.compile(r"1\s+")

if __name__ == "__main__":
    headers = []
    if len(sys.argv) == 2:
        filename = sys.argv[1]
        with open(filename) as fh:
            print "Opened file"
            title = 0
            header = 0
            for line in fh:
                if re.match(title_re, line):
                    title += 1
                    print "Found title:", line
                if title and re.match(header_re, line):
                    header += 1
                    headers.append(line)
                    print "Found header:", line
                if title and header and re.match(footer_re, line):
                    print "Found footer:", line
                    break

    for line in headers:
        line = re.sub(r"^0", " ", line)
        i = re.finditer(r"(\s*)([^\s]+)", line)
        space_sizes = []
        columns = []
        for m in i:
            space = m.group(1)
            space_sizes.append(len(space))
            col = m.group(2)
            columns.append(col)

        num_cols = len(columns)

        breakpoints = [0]
        for n, space, col in zip(xrange(num_cols), space_sizes, columns):
            print n, space, col, 
            print "(%s)" % len(col)
            if n == 0:
                width = space + len(col) + ceil(space_sizes[n+1]/2)
            elif n == num_cols:
                width = floor(space/2) + len(col)
            else:
                width = floor(space/2) + len(col) + ceil(space_sizes[n+1]/2)
            breakpoints.append(width)

        print breakpoints
# vim:sw=4:ts=4:expandtab
