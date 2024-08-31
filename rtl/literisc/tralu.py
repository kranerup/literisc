#!/usr/bin/env python3
from cpu import alu_to_sym

import sys

while True:
    line = sys.stdin.readline().strip()
    if not line:
        break
    if int(line,16) in alu_to_sym:
        print(alu_to_sym[int(line,16)],flush=True)
    else:
        print("ILLEGAL",flush=True)
