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
    rows = []

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
                    headers.append((header,line))
                    print "Found header:", line
                if title and header and not re.match(header_re, line):
                    if line.strip() == "0":
                        continue
                    rows.append((header,line))
                    #print "Found row: %s (%s)" % (line, header) 
                if title and header and re.match(footer_re, line):
                    print "Found footer:", line
                    break

    breakpoints = dict() 
    for head_num, line in headers:
        if len(headers) == 0:
            break
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

        header_breakpoints = [0]
        base = 0
        for n, space, col in zip(xrange(num_cols), space_sizes, columns):
            print n, space, col, 
            print "(%s)" % len(col)
            if n == 0:
                width = space + len(col) + int(ceil(space_sizes[n+1]/2))
                point = base + width
                base += width
            elif n == num_cols-1:
                width = int(floor(space/2)) + len(col)
                point = base + width
                base += width
            else:
                width = int(floor(space/2)) + len(col) + int(ceil(space_sizes[n+1]/2))
                point = base + width
                base += width
            header_breakpoints.append(point)

        # defining breakpoints as midpoint of line between midpoints of columns
        #for n, space, col in zip(xrange(num_cols), space_sizes, columns):
            #if n == 0:
                ## pieces: 
                ## (floor(len(col)/2) + ceil(len(columns[n+1])/2))/2


        #breakpoints.append((head_num, header_breakpoints))
        #print "head num type", type(head_num)
        breakpoints[head_num] = header_breakpoints
        #breakpoints.insert(head_num, header_breakpoints)

    print breakpoints

    for head_num, line in rows:
        bp = breakpoints[head_num]
        num_cols = len(bp) - 1
        for i in xrange(num_cols):
            print line[bp[i]:bp[i+1]].strip()

# vim:sw=4:ts=4:expandtab
