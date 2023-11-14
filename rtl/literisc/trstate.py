#!/usr/bin/env python3
from cpu import state_to_sym

import sys

while True:
    line = sys.stdin.readline().strip()
    if not line:
        break
    print(state_to_sym[int(line)],flush=True)
