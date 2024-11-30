from myhdl import *

from modules.common.signal import signal
from soc import soc


clk = signal()
rstn = signal()
gpio = signal(32)
ser_txd = signal(8)
ser_send = signal()
ser_ready = signal()

toVerilog.standard = 'systemverilog'
itop = toVerilog( soc, clk, rstn, gpio,
        ser_txd,
        ser_send,
        ser_ready)
