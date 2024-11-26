
from myhdl import *

from modules.common.signal import signal
from soc import soc

def tb():
    clk = Signal(bool())
    rstn = signal()
    gpio = signal(32)

    isoc = soc( clk, rstn, gpio )

    @always(delay(10))
    def stim():
      print("=========== CLOCK ============")
      clk.next = not clk

    @instance
    def seq():
      rstn.next = 0
      yield clk.posedge
      rstn.next = 1 
      yield clk.posedge
      print("====== reset done =======")

    return instances()


traceSignals.filename = 'trace'
itb = traceSignals( tb ) 
sim = Simulation( itb )
sim.run( 2000 )
