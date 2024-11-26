from myhdl import *

from modules.common.signal import signal
from cpu_sys import cpu_sys
from axi import Axi4


axi = Axi4(asize=16, dsize=32, idsize=1)
clk = signal()
rstn = signal()

toVerilog.standard = 'systemverilog'
itop = toVerilog( cpu_sys, clk, rstn, axi )
