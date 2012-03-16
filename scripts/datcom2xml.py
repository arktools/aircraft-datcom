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

    ### Collect correct table, header, and data lines
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
                if title and header and not re.match(header_re, line) and not re.match(footer_re, line):
                    if line.strip() == "0":
                        continue
                    rows.append((header,line))
                    #print "Found row: %s (%s)" % (line, header) 
                if title and header and re.match(footer_re, line):
                    print "Found footer:", line
                    break

    ### Loop through headers to detect whitespace to slice data lines
    breakpoints = dict() 
    for head_num, line in headers:
        if len(headers) == 0:
            break
        # replace leading 0 with space
        line = re.sub(r"^0", " ", line)
        # find all pairs of (whitespace, non-whitespace)
        i = re.finditer(r"(\s*)([^\s]+)", line)
        space_len = []
        columns = []
        col_len = []
        for m in i:
            space = m.group(1)
            space_len.append(len(space))
            col = m.group(2)
            columns.append(col)
            col_len.append(len(col))

        # first value is 0 for [0:x] slice
        header_breakpoints = [0]
        base = 0

        for n in xrange(1, len(columns)):
            # prev col width + smaller half of trailing whitespace
            right_half = col_len[n-1] + int(floor(0.5*space_len[n]))
            if n == 1:
                # all of 0th whitespace
                left_half = space_len[n-1]
            else:
                # larger half of leading whitespace
                left_half = int(ceil(0.5*space_len[n-1]))
            # half leading whitespace, col width, half trailing whitespace
            width = left_half + right_half
            point = base + width
            # set base to current breakpoint
            base = point
            header_breakpoints.append(point)

        # store header breakpoints keyed by head_num
        breakpoints[head_num] = header_breakpoints

    print breakpoints

    ### Slice data lines based on header whitespace
    for head_num, line in rows:
        bp = breakpoints[head_num]
        num_cols = len(bp)
        for i in xrange(num_cols):
            # last value
            if i == num_cols-1:
                cell = line[bp[i]:]
            else:
                cell = line[bp[i]:bp[i+1]]
            print cell.strip()
        print "\n"

# vim:sw=4:ts=4:expandtab
