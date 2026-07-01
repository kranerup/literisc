from myhdl import *

from modules.common.signal import signal
from cpu_sys import cpu_sys
from axi import Axi4
from conf import Conf


axi = Axi4(asize=16, dsize=32, idsize=1)
conf = Conf()
clk = signal()
rstn = signal()
obs_trace = Signal(modbv(0)[69:])

toVerilog.standard = 'systemverilog'
itop = toVerilog( cpu_sys, clk, rstn, axi, conf, obs_trace )
