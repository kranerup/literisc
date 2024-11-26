from myhdl import *

from modules.common.signal import signal
from soc import soc


clk = signal()
rstn = signal()
gpio = signal(32)

toVerilog.standard = 'systemverilog'
itop = toVerilog( soc, clk, rstn, gpio )
